(ns reacl-c.impl.reacl
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.xpath :as xp]
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
        (throw (ex-info "Non-optimal: dynamic item should return equal items for equal state and arguments." {:item-1 e1 :item-2 e2}))))))

(defprotocol ^:private IReacl
  (-instantiate-reacl [this binding] "Returns a list of Reacl components or dom elements.")
  (-xpath-pattern [this] "An xpath that selects something like this item.") ;; Note: multiple semantics of this possible; for now a very lose match.
  )

(defn xpath-pattern [e]
  (if (string? e)
    (xp/comp (xp/is= e))
    (-xpath-pattern e)))

(defn- class-args-pattern [class args]
  (xp/comp (xp/type class)
           (xp/where (xp/comp xp/args (xp/is= args)))))

(defn- wrapper-pattern [class e & rest-args]
  ;; first arg must be the sub item.
  (xp/comp (xp/type class)
           (xp/where (if (nil? rest-args) ;; can't select no args   (FIXME: an 'is-empty' selector?)
                       xp/self
                       (xp/comp xp/args (xp/is? (fn [args_]
                                                  (= (rest args_) rest-args))))))
           ;; TODO: exactly one child or not?
           (xp/where (xp/comp xp/children (xpath-pattern e)))))

(defrecord ^:private LiftedClass [class args]
  base/E
  IReacl
  (-xpath-pattern [this]
    (class-args-pattern class [args]))
  (-instantiate-reacl [this binding]
    [(if (and (rcore/reacl-class? class) (rcore/has-app-state? class))
       (apply class binding args)
       (apply class args))]))

(defn lift [class & args]
  (LiftedClass. class args))

(defn instantiate
  "Returns a Reacl component/element for the given item and state binding."
  [binding e]
  (cond
    (satisfies? IReacl e) (let [cs (-instantiate-reacl e binding)]
                            (if (= 1 (count cs))
                              (first cs)
                              (apply rdom/fragment cs)))
    (string? e) (rdom/fragment e)
    
    :else (throw (ex-info "Expected an item or a string only." {:value e}))))

(defn- instantiate-child [binding e]
  ;; returns multiple elements or strings
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    (string? e) [e]
    :else (throw (ex-info "Expected an item or a string only." {:value e}))))

(defrecord ^:private ActionMessage [action])

(defn- pass-message [child msg]
  (let [comp (rcore/get-dom child)]
    (assert (rcore/component? comp) (str "Not a component: " (pr-str comp) ". Forgot to use set-ref?"))
    (rcore/return :message [comp msg])))

(declare transform-return)
(defn- handle-effect-return [toplevel eff ret]
  (assert (base/returned? ret) "Effects must return a (return ..) value.")
  (if (not= base/keep-state (:state ret))
    (throw (ex-info "Effects must not return a new state." {:effect eff :value ret}))
    (transform-return (base/merge-returned
                       (base/make-returned base/keep-state [] (:messages ret))
                       (if-let [actions (not-empty (:actions ret))]
                         ;; new actions are not passed upwards, but handled again as toplevel actions (can be more effects, basically)
                         (base/make-returned base/keep-state [] (mapv #(vector toplevel (ActionMessage. %))
                                                                      actions))
                         (base/make-returned base/keep-state [] []))))))

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
        (if (base/effect? action)
          (handle-effect-return this action (apply (:f action) (:args action)))
          (do (warn "Unhandled action:" action)
              (rcore/return)))) 

      :else (pass-message child msg))))


(defrecord ^:private ReaclApplication [comp]
  base/Application  
  (-send-message! [this msg]
    (rcore/send-message! comp msg)))

(defn run
  [dom item initial-state]
  (ReaclApplication. (rcore/render-component dom
                                             toplevel
                                             initial-state
                                             item)))

(defn ^:no-doc transform-return [r]
  ;; a base/Returned value to a rcore/return value.
  (assert (base/returned? r) (str "Expected a value created by 'return', but got: " (pr-str r) "."))
  (apply rcore/merge-returned
         (if (not= base/keep-state (:state r))
           (rcore/return :app-state (:state r))
           (rcore/return))
         (concat (map (fn [a] (rcore/return :action a))
                      (:actions r))
                 (map (fn [[target msg]]
                        (let [c (rcore/get-dom (:reacl-ref target))]
                          (assert (some? c) (str "Target for message not available. Forgot to use set-ref?"))
                          (assert (rcore/component? c) (str "Target for message is not a component: " (pr-str c)))
                          (rcore/return :message [c msg])))
                      (:messages r)))))


(rcore/defclass ^:private handle-message this state [e f]
  handle-message
  (fn [msg]
    (transform-return (f state msg)))

  render
  (instantiate (rcore/bind this) e))

(extend-type base/HandleMessage
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-message e f))
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
    (assert (base/name-id? name-id))
    (or (.get classes name-id)
        (let [c (gen-named (base/name-id-name name-id))]
          (.set classes name-id c)
          c))))

(extend-type base/Named
  IReacl
  (-xpath-pattern [{e :e name-id :name-id}]
    (wrapper-pattern (named name-id) e))
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

(defn- dom-message-to-action [state msg events]
  (cond
    (instance? EventMessage msg)
    (let [{ev :ev} msg
          f (find-event-handler ev events)]
      (transform-return (f state ev)))

    :else
    (throw (ex-info "Cannot send a message to dom elements." {:value msg}))))

(defn- dom-event-handler [target]
  (fn [ev]
    (rcore/send-message! target (EventMessage. ev))))

(defn- merge-dom-attrs [target attrs events handler]
  (let [r-events (into {} (map (fn [[k v]]
                                 [k handler])
                               events))]
    (merge attrs r-events)))

(defn- native-dom [binding type attrs self-ref & children]
  (apply rdom/element type
         (cond-> attrs
           self-ref (assoc :ref (:reacl-ref self-ref)))
         (map (partial instantiate binding) children)))

(def dom-class-for-type
  ;; There should be a finite set of tag names, so using memoize should be ok.
  (memoize
   (fn [type]
     (rcore/class ^:private (str "reacl-c.dom/" type) this state [attrs events ref & children]
                  ;; dom with action events and children.
                  local-state [handler (dom-event-handler this)]
  
                  handle-message (fn [msg]
                                   (dom-message-to-action state msg events))
  
                  render
                  (apply native-dom (rcore/bind this) type (merge-dom-attrs this attrs events handler) ref
                         children)))))

(defn- dom-class [binding type events ref & children]
  (apply (dom-class-for-type type) binding events ref children))

(defn- dom [binding type attrs events ref & children]
  ;; TODO: is it even worth it to 'optimize' ?
  (if (and (empty? events) (not (some base/item? children))) ;; TODO: any child is ok, isn't it? (strings at least)
    (apply native-dom binding type attrs ref children)
    (apply dom-class binding type attrs events ref children)))

(extend-type dom/Element
  IReacl
  (-xpath-pattern [{type :type attrs :attrs events :events ref :ref children :children}]
    (xp/comp
     ;; match the tag, or the tag below the matching wrapper class.
     (xp/or (xp/tag type)
            (xp/comp (xp/class (dom-class-for-type type))
                     xp/children))
     ;; attributes match, with specials for class and style.
     (xp/where
      (apply xp/and
             (map (fn [[k v]]
                    (case k
                      (:class :className "class" "className") (xp/css-class? v)
                      (:style "style") (xp/style? v)
                      (xp/where (xp/comp (xp/attr k) (xp/is= v)))))
                  attrs)))
     ;; has the events
     (xp/where
      (apply xp/and
             (map (fn [[k v]]
                    ;; enough to 'have' the event.
                    (xp/where (xp/attr k)))
                  events)))
     ;; ref is ignored for now.
             
     ;; all children must match in any order (not possible to check the order, I think?)
     ;; but the number of matching children must match either - that works around
     ;; two similar children matching the same node
     (xp/where
      (if (empty? children)
        xp/self
        (xp/comp xp/children
                 ;; we want to allow a dom or text child to match further down; 'skipping' any
                 ;; of our wrapper classes in between.
                 (let [child-match (apply xp/or (map xpath-pattern children))]
                   (xp/or child-match
                          (xp/comp (xp/first-where (xp/not xp/class?))
                                   child-match)))
                 (xp/count= (count children)))))))
  (-instantiate-reacl [{type :type attrs :attrs events :events ref :ref children :children} binding]
    [(apply dom binding type attrs events ref children)]))

(extend-type base/Fragment
  IReacl
  (-xpath-pattern [{children :children}]
    ;; a frament are 'invisible', eg. matching a fragment is the same as matching (all/some) children.
    (apply xp/and (map xpath-pattern children)))
  (-instantiate-reacl [{children :children} binding]
    (mapv (partial instantiate binding)
          children)))

(defn- keyed [binding e key]
  ;; TODO: is a keyed fragment/string meaningful?
  (-> (instantiate binding e)
      (rcore/keyed key)))

(extend-type base/Keyed
  IReacl
  (-xpath-pattern [{e :e key :key}]
    ;; FIXME: the key does not work... don't know why.
    (xp/and #_(xp/where (xp/comp xp/key (xp/is= key)))
            (xpath-pattern e)))
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
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern with-ref (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-ref binding f args)]))

(rcore/defclass ^:privte set-ref this state [e ref]
  handle-message
  (fn [msg]
    (pass-message (:reacl-ref ref) msg))

  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer (:reacl-ref ref))))

(extend-type base/SetRef
  IReacl
  (-xpath-pattern [{e :e ref :ref}]
    (wrapper-pattern set-ref e ref))
  (-instantiate-reacl [{e :e ref :ref} binding]
    ;; has to be a class for now, because otherwise we would override the ref with all our 'child' refs for passing messages down.
    ;; TODO: either change that; or we could name this 'add-ref' or refer, as one can add multiple refs then...?
    [(set-ref binding e ref)]))

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
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern dynamic (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply dynamic binding f args)]))

(defn- focus [binding e lens]
  (instantiate (rcore/focus binding lens) e))

(extend-type base/Focus
  IReacl
  (-xpath-pattern [{e :e lens :lens}]
    (wrapper-pattern focus e lens))
  (-instantiate-reacl [{e :e lens :lens} binding]
    [(focus binding e lens)]))

(rcore/defclass ^:private handle-action this state [e f]
  local-state [action-to-message
               (fn [_ action]
                 ;; never handle effects, esp. because of subscriptions (maybe add as an option?)
                 (if (base/effect? action)
                   (rcore/return :action action)
                   (rcore/return :message [this (ActionMessage. action)])))]

  refs [child]
  
  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (do
        (transform-return (f state (:action msg))))
      :else
      (pass-message child msg)))
  
  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)
      (rcore/reduce-action action-to-message)))

(extend-type base/HandleAction
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-action e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-action binding e f)]))

(defrecord ^:private NewIsoState [state])

(defn- id-state [st1 st2]
  (if (= st1 st2)
    rcore/keep-state
    st2))

(rcore/defclass ^:private local-state this astate [e initial]
  local-state [lstate {:initial initial
                       :current initial}]

  component-will-receive-args
  (fn [new-e new-initial]
    (if (not= (:initial lstate) new-initial)
      (rcore/return :local-state {:initial new-initial
                                  :current new-initial})
      (rcore/return)))

  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction [astate (:current lstate)]
                                       (rcore/reaction this ->NewIsoState))
                   e)
      (rcore/refer child))
  
  handle-message
  (fn [msg]
    (cond
      (instance? NewIsoState msg)
      (let [{[new-app-state new-local-state] :state} msg]
        (rcore/return :app-state (id-state astate new-app-state)
                      :local-state (id-state lstate (assoc lstate :current new-local-state))))
      :else
      (pass-message child msg))))

(extend-type base/LocalState
  IReacl
  (-xpath-pattern [{e :e initial :initial}]
    (wrapper-pattern local-state e initial))
  (-instantiate-reacl [{e :e initial :initial} binding]
    [(local-state binding e initial)]))

(defrecord ^:private AsyncReturn [ret])

(rcore/defclass ^:private with-async-return this state [f & args]
  local-state [send! (fn [v]
                       (assert (base/returned? v) v)
                       ;; Note: if v only contains a message, we might want to optimize and send directly?
                       ;; (didn't work for some unknown reasons though)
                       (rcore/send-message! this (AsyncReturn. v))
                       nil)]
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
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern with-async-return (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-async-return binding f args)]))

(defrecord ^:private Mount []) (def ^:private mount-msg (Mount.))
(defrecord ^:private Unmount []) (def ^:private unmount-msg (Unmount.))
(defrecord ^:private Update []) (def ^:private update-msg (Update.))

(rcore/defclass ^:private once this state [f cleanup-f]
  local-state [done nil]

  handle-message
  (fn [msg]
    (condp = msg
      mount-msg
      (let [ret (f state)]
        (rcore/merge-returned (transform-return ret)
                              (rcore/return :local-state ret)))
      
      update-msg
      (let [ret (f state)]
        (if (not= done ret)
          (rcore/merge-returned (transform-return ret)
                                (rcore/return :local-state ret))
          (rcore/return)))

      unmount-msg
      (if (some? cleanup-f)
        (transform-return (cleanup-f state))
        (rcore/return))

      (throw (ex-info "Cannot send a message to once items." {:value msg}))))
  
  component-did-update
  (fn [prev-app-state prev-local-state prev-ret prev-cleanup-ret]
    ;; Note: Reacl does not guarantee that the state is accurate in this method (yet); need to go through a message.
    (rcore/return :message [this update-msg]))

  component-did-mount
  (fn []
    (rcore/return :message [this mount-msg]))

  component-will-unmount
  (fn []
    (rcore/return :message [this unmount-msg]))

  render (rdom/fragment))

(extend-type base/Once
  IReacl
  (-xpath-pattern [{f :f cleanup-f :cleanup-f}]
    (class-args-pattern once [f cleanup-f]))
  (-instantiate-reacl [{f :f cleanup-f :cleanup-f} binding]
    [(once binding f cleanup-f)]))

(defrecord ^:private MonitorMessage [new-state])

(rcore/defclass ^:private handle-state-change this state [e f]
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

(extend-type base/HandleStateChange
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-state-change e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-state-change binding e f)]))

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
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern error-boundary e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(error-boundary binding e f)]))
