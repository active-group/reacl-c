(ns reacl-c.impl.reacl
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.dom :as rdom]
            [clojure.string :as str]))

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
  (apply LiftedClass. class args))

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
  (rcore/return :message [(rcore/get-dom child) msg]))

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
        ;; TODO: throw or warn only?
        (throw (ex-info "Unhandled toplevel action." {:value action}))) 

      :else (pass-message child msg))))

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

(defn- transform-return [r child]
  ;; a base/Returned value to a rcore/return value.
  (assert (instance? base/Returned r) (str "Expected a value created by 'return', but got: " (pr-str r) "."))
  (apply rcore/merge-returned
         (if-let [st (:opt-state r)]
           (rcore/return :app-state (first st))
           (rcore/return))
         (concat (map (fn [a] (rcore/return :action a))
                      (:actions r))
                 (map (fn [a] (rcore/return :message [child a]))
                      (:messages r)))))


(rcore/defclass ^:private handle-message this state [e f & args]
  refs [child]

  handle-message
  (fn [msg]
    (transform-return (apply f state msg args)
                      (rcore/get-dom child)))

  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)))

(extend-type base/HandleMessage
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply handle-message binding e f args)]))

(defn- gen-named [s]
  (rcore/class s this state [e]
               refs [child]
               handle-message (fn [msg] (pass-message child msg))
               render (-> (instantiate (rcore/bind this) e)
                          (rcore/refer child))))

(def ^:no-doc named (memoize gen-named))

(extend-type base/Named
  IReacl
  (-instantiate-reacl [{e :e name :name} binding]
    [((named name) binding e)]))

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

(defn- dom-message-to-action [events]
  (fn [msg]
    (cond
      (instance? EventMessage msg)
      (let [{ev :ev} msg
            f (find-event-handler ev events)]
        (if-let [a (f ev)]
          (rcore/return :action a)
          (rcore/return)))

      :else
      ;; TODO else messages to (only named/refered?) children?
      (throw (ex-info "Sending messages to a dom element not implemented yet." {:value msg})))))

(defn- dom-event-handler [target]
  (fn [ev]
    (rcore/send-message! target (EventMessage. ev))))

(defn- merge-dom-attrs [target attrs events handler]
  (let [r-events (into {} (map (fn [[k v]]
                                 [k handler])
                               events))]
    (merge attrs r-events)))

(defn- native-dom [binding type attrs & children]
  (apply rdom/element type attrs
         (mapcat (partial instantiate-child binding) children)))

(rcore/defclass ^:private dom++ this state [type attrs events children]
  refs [native]
  ;; dom with action events and children.

  local-state [handler (dom-event-handler this)]
  handle-message (dom-message-to-action events)
  
  render
  (-> (apply native-dom (rcore/bind this) type (merge-dom-attrs this attrs events handler)
             children)
      (rcore/refer native)))

(rcore/defclass ^:private dom+ this [type attrs events]
  ;; dom with action events, but without children (does not need state)
  refs [native]
  
  local-state [handler (dom-event-handler this)]
  handle-message (dom-message-to-action events)
  
  render
  (-> (native-dom (rcore/use-app-state nil) type
                  (merge-dom-attrs this attrs events handler))
      (rcore/refer native)))

(defn- dom [binding type attrs events children]
  ;; optimize for dom element without event handlers:
  (if (not-empty events)
    (if (empty? children)
      (dom+ type attrs events)
      ;; OPT: if no child needs state, then we can use a non-stateful class here too
      (dom++ binding type attrs events children))
    (apply native-dom binding type attrs children)))

(extend-type dom/Element
  IReacl
  (-instantiate-reacl [{type :type attrs :attrs events :events children :children} binding]
    [(dom binding type attrs events children)]))

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

(rcore/defclass ^:private with-state this state [f & args]
  refs [child]
  
  handle-message
  (fn [msg]
    (pass-message child msg))
  
  render
  (-> (instantiate (rcore/bind this) (apply f state args))
      (rcore/refer child)))

(extend-type base/WithState
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-state binding f args)]))

(defn- focus [binding e lens]
  (instantiate (rcore/focus binding lens) e))

(extend-type base/Focus
  IReacl
  (-instantiate-reacl [{e :e lens :lens} binding]
    [(focus binding e lens)]))

(rcore/defclass ^:private handle-action this state [e f & args]
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
        (transform-return (apply f state (:action msg) args)
                          (rcore/get-dom child)))
      :else
      (pass-message child msg)))
  
  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)
      (rcore/reduce-action action-to-message)))

(extend-type base/HandleAction
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply handle-action binding e f args)]))

(defrecord ^:private NewIsoState [state])

(defn- id-state [st1 st2]
  ;; TODO: use = instead? or allow the user to specify it?
  (if (identical? st1 st2)
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

(defrecord ^:private AsyncAction [v])

;; TODO: can't we make the element returned by f fixed? - like an 'inject actions?'
(rcore/defclass ^:private with-async-action this state [f & args]
  local-state [deliver! (fn [action]
                          (rcore/send-message! this (AsyncAction. action)))]

  refs [child]
  
  render
  (-> (instantiate (rcore/bind this) (apply f deliver! args))
      (rcore/refer child))

  handle-message
  (fn [msg]
    (cond
      (instance? AsyncAction msg)
      (rcore/return :action (:v msg))
      
      :else
      (pass-message child msg))))

(extend-type base/WithAsyncActions
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-async-action binding f args)]))

(defn- resolve-component [ref]
  ;; when did-update and will-mount is attached to a (dom) element, then we want to pass the native dom element.
  ;; but the wrapper classes (dom+ and dom++) are in between :-/

  ;; TODO: this terrible hack comes as a remedy, but maybe we should 'push'
  ;; the livecycle methods down to the dom classes, to get first hand
  ;; access (btw, doing to would be good anyways; we could pass many things to just one dom class).
  (let [c (rcore/get-dom ref)]
    (if (and (rcore/component? c)
             (#{dom+ dom++} (rcore/component-class c)))
      ;; Note: dom+ and dom++ must have a 'native' ref to the actual raw dom element.
      (let [native-ref (first (aget (.-props c) "reacl_refs"))]
        (rcore/get-dom native-ref))

      ;; everything else, has a dom node deeper down (resolve to that? remove fragments first)
      c)))

(rcore/defclass ^:private did-mount this state [e f & args]
  refs [child]

  handle-message
  (fn [msg]
    (pass-message child msg))
  
  component-did-mount
  (fn []
    (rcore/return :action (apply f (resolve-component child) args)))

  render (-> (instantiate (rcore/bind this) e)
             (rcore/refer child)))

(extend-type base/DidMount
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply did-mount binding e f args)]))

(rcore/defclass ^:private will-unmount this state [e f & args]
  refs [child]

  handle-message
  (fn [msg]
    (pass-message child msg))

  component-will-unmount
  (fn []
    (rcore/return :action (apply f (resolve-component child) args)))

  render (-> (instantiate (rcore/bind this) e)
             (rcore/refer child)))

(extend-type base/WillUnmount
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply will-unmount binding e f args)]))

(rcore/defclass ^:private did-update this state [e f & args]
  refs [child]

  handle-message
  (fn [msg]
    (pass-message child msg))
  
  component-did-update
  (fn [prev-app-state prev-local-state prev-e prev-f & prev-args]
    ;; TODO: need to pass old/new state, old/new args to f to be really useful?
    (rcore/return :action (apply f (resolve-component child) args)))

  render (-> (instantiate (rcore/bind this) e)
             (rcore/refer child)))

(extend-type base/DidUpdate
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply did-update binding e f args)]))

(defrecord ^:private MonitorMessage [new-state])

;; TODO: monitor that state change desire, or use 'after-update' for this?
(rcore/defclass ^:private monitor-state this state [e f & args]
  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction state (rcore/reaction this ->MonitorMessage)) e)
      (rcore/refer child))

  handle-message
  (fn [msg]
    (cond
      (instance? MonitorMessage msg)
      (let [new-state (:new-state msg)]
        (rcore/merge-returned (if-let [a (apply f state new-state args)]
                                (rcore/return :action a)
                                (rcore/return))
                              (rcore/return :app-state new-state)))
      :else
      (pass-message child msg))))

(extend-type base/MonitorState
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply monitor-state binding e f args)]))

(defn- fst-lens
  ([[a _]] a)
  ([[_ b] a] [a b]))

(rcore/defclass ^:private error-boundary this state [e f & args]
  
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
    (rcore/return :action (apply f error args))))

(extend-type base/ErrorBoundary
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    [(apply error-boundary binding e f args)]))
