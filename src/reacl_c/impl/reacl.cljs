(ns reacl-c.impl.reacl
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.dom :as rdom]
            [clojure.string :as str]))

(defn- warn [& args]
  (if (and js/console js/console.warn)
    (apply js/console.warn args)
    (apply println args)))

(def ^:no-doc check-performance? (atom false))

(defn- performance-check! [f & args]
  (when @check-performance?
    (let [e1 (apply f args)
          e2 (apply f args)]
      (when-not (= e1 e2)
        ;; throw or warn? throw gives the better positional information; but stops at first.
        ;; TODO: can make a data-diff to better visualize the differences?
        ;; TODO: warn only. Or remove, now that we have test-util performance test.
        (throw (ex-info "Non-optimal: dynamic elements should return equal elements for equal state and arguments." {:element-1 e1 :element-2 e2}))))))

(defprotocol ^:private IReacl
  (-instantiate-reacl [this binding] "Returns a list of Reacl components or dom elements."))

(defrecord ^:private LiftedClass [class args]
  base/E
  IReacl
  (-instantiate-reacl [this binding]
    [(if (and (rcore/reacl-class? class) (rcore/has-app-state? class))
       (apply class binding args)
       (apply class args))]))

(defn lift [class & args]
  (LiftedClass. class args))

(defn instantiate
  "Returns a Reacl component/element for the given element and state binding."
  [binding e]
  (cond
    (satisfies? IReacl e) (let [cs (-instantiate-reacl e binding)]
                            (if (= 1 (count cs))
                              (first cs)
                              (apply rdom/fragment cs)))
    (string? e) (rdom/fragment e)
    
    :else (throw (ex-info "Expected an element or a string only." {:value e}))))

(defn- instantiate-child [binding e]
  ;; returns multiple elements or strings
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    (string? e) [e]
    :else (throw (ex-info "Expected an element or a string only." {:value e}))))

(defrecord ^:private ActionMessage [action])

(defn- pass-message [child msg]
  (let [comp (rcore/get-dom child)]
    (assert (rcore/component? comp) (str "Not a component: " (pr-str comp) ". Forgot to use set-ref?"))
    (rcore/return :message [comp msg])))

(rcore/defclass ^:private toplevel this state [e]
  refs [child]
  
  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)
      (rcore/action-to-message this ->ActionMessage))

  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (let [action (:action msg)]
        (warn "Unhandled action:" action)
        (rcore/return)) 

      :else (pass-message child msg))))

(defn send-message!
  "Sends a message to the runtime component `target`."
  [target msg]
  (rcore/send-message! target msg))

(defrecord ^:private ReaclApplication [comp]
  base/Application  
  (-send-message! [this msg]
    (rcore/send-message! comp msg)))

(defn run
  "Run and mount the given element `e` in the native dom element
  `dom`, with the given initial state."
  [dom e initial-state]
  (ReaclApplication. (rcore/render-component dom
                                             toplevel
                                             initial-state
                                             e)))

(defn- transform-return [r]
  ;; a base/Returned value to a rcore/return value.
  (assert (instance? base/Returned r) (str "Expected a value created by 'return', but got: " (pr-str r) "."))
  (apply rcore/merge-returned
         (if-let [st (:opt-state r)]
           (rcore/return :app-state (first st))
           (rcore/return))
         (concat (map (fn [a] (rcore/return :action a))
                      (:actions r))
                 (map (fn [[target msg]]
                        (assert (some? target) "Missing target for message. Forgot to use set-ref?")
                        (assert (rcore/component? target) "Target for a message must be a component. Forgot to use deref?")
                        (rcore/return :message [target msg]))
                      (:messages r)))))


(rcore/defclass ^:private handle-message this state [e f]
  handle-message
  (fn [msg]
    (transform-return (f msg)))

  render
  (instantiate (rcore/bind this) e))

(extend-type base/HandleMessage
  IReacl
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-message binding e f)]))

(defn- gen-named [s]
  (rcore/class s this state [e]
               refs [child]
               handle-message (fn [msg] (pass-message child msg))
               render (-> (instantiate (rcore/bind this) e)
                          (rcore/refer child))))

(let [classes (js/WeakMap.)]
  (defn ^:no-doc named [name-id]
    (assert (instance? base/NameId name-id))
    (or (.get classes name-id)
        (let [c (gen-named (base/name-id-name name-id))]
          (.set classes name-id c)
          c))))

(extend-type base/Named
  IReacl
  (-instantiate-reacl [{e :e name-id :name-id} binding]
    [((named name-id) binding e)]))

(defrecord ^:private EventMessage [ev])

(defn- find-event-handler [ev events]
  (when-not (.-type ev)
    (throw (ex-info "Expected a JavaScript event object, with a property 'type'." {:value ev})))
  (let [en (str/lower-case (.-type ev))]
    ;; OPT: could be done a little faster - try a lookup, or prepare a different map. But it can be 'onchange' and 'onChange'.
    (some (fn [[n f]]
            ;; "change" = :onChange ?
            ;; FIXME: are things like onChangeCapture possible? how to handle them?
            (and (= en (str/lower-case (subs (name n) 2)))
                 f))
          events)))

(defn- dom-message-to-action [msg events first-ref]
  (cond
    (instance? EventMessage msg)
    (let [{ev :ev} msg
          f (find-event-handler ev events)]
      (transform-return (f ev)))

    :else
    (if-let [c (let [c (rcore/get-dom first-ref)] (and c (rcore/component? c)))]
      (rcore/return :message [c msg])
      ;; TODO: or maybe 'the first child that can receive messages'? like (dom "ab" target "de")
      (throw (ex-info "Cannot send a message to dom elements that have no other elements as children." {:value msg})))))

(defn- dom-event-handler [target]
  (fn [ev]
    (rcore/send-message! target (EventMessage. ev))))

(defn- merge-dom-attrs [target attrs events handler]
  (let [r-events (into {} (map (fn [[k v]]
                                 [k handler])
                               events))]
    (merge attrs r-events)))

(defn- native-dom [binding type attrs self-ref first-child-ref & children]
  (apply rdom/element type
         (cond-> attrs
           self-ref (assoc :ref (:reacl-ref self-ref)))
         (let [children (map (partial instantiate binding) children)]
           (when (not-empty children)
             (cons (-> (first children)
                       (rcore/refer first-child-ref))
                   (rest children))))))

(def ^:private dom-class_
  ;; There should be a finite set of tag names, so using memoize should be ok.
  (memoize
   (fn [type]
     (rcore/class ^:private (str "reacl-c.dom/" type) this state [attrs events ref & children]
                  ;; dom with action events and children.
                  refs [first]

                  local-state [handler (dom-event-handler this)]
  
                  handle-message (fn [msg] (dom-message-to-action msg events first))
  
                  render
                  (apply native-dom (rcore/bind this) type (merge-dom-attrs this attrs events handler) ref first
                         children)))))

(defn- dom-class [binding type events ref & children]
  (apply (dom-class_ type) binding events ref children))

(defn- dom [binding type attrs events ref & children]
  ;; TODO: is it even worth it to 'optimize' ?
  (if (and (empty? events) (not (some base/element? children)))
    (apply native-dom binding type attrs ref nil children)
    (apply dom-class binding type attrs events ref children)))

(extend-type dom/Element
  IReacl
  (-instantiate-reacl [{type :type attrs :attrs events :events ref :ref children :children} binding]
    [(apply dom binding type attrs events ref children)]))

(extend-type base/Fragment
  IReacl
  (-instantiate-reacl [{children :children} binding]
    (mapv (partial instantiate binding)
          children)))

(defn- keyed [binding e key]
  (-> (instantiate binding e)
      (rcore/keyed key)))

(extend-type base/Keyed
  IReacl
  (-instantiate-reacl [{e :e key :key} binding]
    [(keyed binding e key)]))

(defrecord WrapRef [reacl-ref]
  base/Ref
  (-deref-ref [this] (rcore/get-dom reacl-ref)))

(rcore/defclass ^:private with-ref this state [f & args]
  refs [child r]

  handle-message
  (fn [msg]
    (pass-message child msg))

  render
  (let [rr (WrapRef. r)]
    (apply performance-check! (partial f rr) args)
    (-> (instantiate (rcore/bind this) (apply f rr args))
        (rcore/refer child))))

(extend-type base/WithRef
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-ref binding f args)]))

(extend-type base/SetRef
  IReacl
  (-instantiate-reacl [{e :e ref :ref} binding]
    [(-> (instantiate binding e)
         (rcore/refer (:reacl-ref ref)))]))

(rcore/defclass ^:private dynamic this state [f & args]
  refs [child]
  
  handle-message
  (fn [msg]
    (pass-message child msg))
  
  render
  (do (apply performance-check! (partial f state) args)
      (-> (instantiate (rcore/bind this) (apply f state args))
          (rcore/refer child))))

(extend-type base/Dynamic
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply dynamic binding f args)]))

(defn- focus [binding e lens]
  (instantiate (rcore/focus binding lens) e))

(extend-type base/Focus
  IReacl
  (-instantiate-reacl [{e :e lens :lens} binding]
    [(focus binding e lens)]))

(rcore/defclass ^:private handle-action this state [e f]
  local-state [action-to-message
               (fn [_ action]
                 (rcore/return :message [this (ActionMessage. action)]))]

  refs [child]
  
  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (do
        ;; makes up a nice trace: (println "AM:" (:action msg) "=>" (apply f state (:action msg) args))
        (transform-return (f (:action msg))))
      :else
      (pass-message child msg)))
  
  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)
      (rcore/reduce-action action-to-message)))

(extend-type base/HandleAction
  IReacl
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-action binding e f)]))

(defrecord ^:private NewIsoState [state])

(defn- id-state [st1 st2]
  (if (= st1 st2)
    rcore/keep-state
    st2))

(rcore/defclass ^:private local-state this astate [e initial]
  local-state [lstate initial]

  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction [astate lstate]
                                       (rcore/reaction this ->NewIsoState))
                   e)
      (rcore/refer child))
  
  handle-message
  (fn [msg]
    (cond
      (instance? NewIsoState msg)
      (let [{[new-app-state new-local-state] :state} msg]
        (rcore/return :app-state (id-state astate new-app-state)
                      :local-state (id-state lstate new-local-state)))
      :else
      (pass-message child msg))))

(extend-type base/LocalState
  IReacl
  (-instantiate-reacl [{e :e initial :initial} binding]
    [(local-state binding e initial)]))

(defrecord AsyncReturn [ret])

(rcore/defclass ^:private with-async-return this state [f & args]
  local-state [send! (fn [v]
                       (assert (base/returned? v) v)
                       ;; Note: if v only contains a message, we might want to optimize and send directly?
                       ;; (didn't work for some unknown reasons though)
                       (rcore/send-message! this (AsyncReturn. v)))]
  refs [child]
  
  render
  ;; Note: for some unknown reason, sending messages directs to (get-dom child) does not work; it's not assigned when dereferenced in 'async-msg!'.
  (let []
    (apply performance-check! send! args)
    (-> (instantiate (rcore/bind this) (apply f send! args))
        (rcore/refer child)))

  handle-message
  (fn [msg]
    (condp instance? msg
      AsyncReturn (transform-return (:ret msg))

      (pass-message child msg))))

(extend-type base/WithAsyncReturn
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-async-return binding f args)]))

(rcore/defclass ^:private did-mount this state [return]
  component-did-mount (fn [] (if (rcore/returned? return)
                               return
                               (transform-return (return))))

  render (rdom/fragment))

(extend-type base/DidMount
  IReacl
  (-instantiate-reacl [{return :ret} binding]
    [(did-mount binding (if (base/returned? return) (transform-return return) return))]))

(rcore/defclass ^:private will-unmount this state [return]
  component-will-unmount (fn [] (if (rcore/returned? return)
                                  return
                                  (transform-return (return))))

  render (rdom/fragment))

(extend-type base/WillUnmount
  IReacl
  (-instantiate-reacl [{return :ret} binding]
    [(will-unmount binding (if (base/returned? return) (transform-return return) return))]))

(rcore/defclass ^:private did-update this state [e f]
  refs [child]

  handle-message
  (fn [msg]
    (pass-message child msg))
  
  component-did-update
  (fn [prev-app-state prev-local-state prev-e prev-f]
    (if (or (not= prev-app-state state)
            (not= prev-e e))
      (transform-return (f prev-app-state prev-e))
      (rcore/return)))

  render (-> (instantiate (rcore/bind this) e)
             (rcore/refer child)))

(extend-type base/DidUpdate
  IReacl
  (-instantiate-reacl [{e :e f :f} binding]
    [(did-update binding e f)]))

(defrecord ^:private MonitorMessage [new-state])

(rcore/defclass ^:private capture-state-change this state [e f]
  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction state (rcore/reaction this ->MonitorMessage)) e)
      (rcore/refer child))

  handle-message
  (fn [msg]
    (cond
      (instance? MonitorMessage msg)
      (let [new-state (:new-state msg)]
        (transform-return (f state new-state)))
      :else
      (pass-message child msg))))

(extend-type base/CaptureStateChange
  IReacl
  (-instantiate-reacl [{e :e f :f} binding]
    [(capture-state-change binding e f)]))

(rcore/defclass ^:private error-boundary this state [e f]
  
  refs [child]
  
  handle-message (fn [msg] (pass-message child msg))
  
  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child))

  component-did-catch
  (fn [error info]
    ;; Note: info is already deprecated in React, in the sense that
    ;; 'getDerivedStateFromError' does not have it. It's also very
    ;; implementation dependant, and less informative in our setup.
    ;; Leave that our for now.
    (transform-return (f error))))

(extend-type base/ErrorBoundary
  IReacl
  (-instantiate-reacl [{e :e f :f} binding]
    [(error-boundary binding e f)]))
