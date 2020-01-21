(ns reacld.impl.reacl
  (:require [reacld.base :as base]
            [reacld.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]
            [clojure.string :as str]))

(defprotocol ^:private IReacl
  (-instantiate-reacl [this binding]))

(defrecord ^:private LiftedClass [class args]
  base/E
  IReacl
  (-instantiate-reacl [this binding]
    (if (and (reacl/reacl-class? class) (reacl/has-app-state? class))
      (apply class binding args)
      (apply class args))))

(defn lift [class & args]
  (apply LiftedClass. class args))

(defn instantiate
  "Returns a Reacl component/element for the given element and state binding."
  [binding e]
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    ;; or strings, usually
    :else (rdom/fragment e)))

(defn- instantiate-child [binding e]
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    ;; or strings, usually
    :else e))

(defrecord ^:private ActionMessage [action])

(reacl/defclass ^:private toplevel this state [e]
  render
  (-> (instantiate (reacl/bind this) e)
      (reacl/action-to-message this ->ActionMessage))

  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (let [action (:action msg)]
        (cond
          (satisfies? base/Effect action) (do (base/-run-effect! action)
                                              (reacl/return))

          :else (throw (ex-info "Unhandled toplevel action" {:value action})))) 

      :else (throw (ex-info "Unhandled toplevel message" {:value msg})))))

(defn run
  "Run and mount the given element `e` in the native dom element
  `dom`, with the given initial state."
  [dom e initial-state]
  (reacl/render-component dom
                          toplevel
                          initial-state
                          e))

(defn run-embedded
  "Return a Reacl component for the given element and state binding,
  to use it in another Reacl application."
  [binding e]
  ;; TODO: really include toplevel effects?? would make this equivalent to instantiate.
  (instantiate binding (lift toplevel e)))

(defrecord ^:private EventMessage [ev])

(defn- find-event-handler [ev events]
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
          (reacl/return :action a)
          (reacl/return)))
      
      ;; TODO else messages to children?
      )))

(defn- dom-event-handler [target]
  (fn [ev]
    (reacl/send-message! target (EventMessage. ev))))

(defn- merge-dom-attrs [target attrs events handler]
  (let [r-events (into {} (map (fn [[k v]]
                                 [k handler])
                               events))]
    (merge attrs r-events)))

(reacl/defclass ^:private dom++ this state [type attrs events children]
  local-state [handler (dom-event-handler this)]
  handle-message (dom-message-to-action events)
  
  render
  (apply rdom/element type (merge-dom-attrs this attrs events handler)
         (map (partial instantiate (reacl/bind this)) children)))

(reacl/defclass ^:private dom+ this [type attrs events]
  local-state [handler (dom-event-handler this)]
  handle-message (dom-message-to-action events)
  
  render
  (rdom/element type (merge-dom-attrs this attrs events handler)))

(defn dom- [binding type attrs children]
  (apply rdom/element type attrs (map (partial instantiate-child binding) children)))

(defn- dom [binding type attrs events children]
  ;; optimize for dom element without event handlers:
  (if (not-empty events)
    (if (empty? children)
      (dom+ type attrs events)
      ;; OPT: if no child needs state, then we can use a non-stateful class here too
      (dom++ binding type attrs events children))
    (dom- binding type attrs children)))

(extend-type dom/Element
  IReacl
  (-instantiate-reacl [{type :type attrs :attrs events :events children :children} binding]
    (dom binding type attrs events children)))

(reacl/defclass ^:private fragment this state [children]
  render (apply rdom/fragment (map (partial instantiate (reacl/bind this))
                                   children)))

(extend-type base/Fragment
  IReacl
  (-instantiate-reacl [{children :children} binding]
    (fragment binding children)))

(defn- keyed [binding e key]
  (-> (instantiate binding e)
      (reacl/keyed key)))

(extend-type base/Keyed
  IReacl
  (-instantiate-reacl [{e :e key :key} binding]
    (keyed binding e key)))

(reacl/defclass ^:private with-state this state [f & args]
  ;; TODO messages?
  render
  (instantiate (reacl/bind this) (apply f state args)))

(extend-type base/WithState
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    (apply with-state binding f args)))

(defn- focus [binding e lens]
  (instantiate (reacl/focus binding lens) e))

(extend-type base/Focus
  IReacl
  (-instantiate-reacl [{e :e lens :lens} binding]
    (focus binding e lens)))

(reacl/defclass ^:private handle-action+ this state [e f & args]
  local-state [action-to-message
               (fn [_ action]
                 (reacl/return :message [this (ActionMessage. action)]))]

  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (do
        ;; makes up a nice trace: (println "AM:" (:action msg) "=>" (apply f state (:action msg) args))
        (apply f state (:action msg) args))))
  
  render
  (-> (instantiate (reacl/bind this) e)
      (reacl/reduce-action action-to-message)))

(defn- action-handler [app-state action f args]
  (let [r (apply f app-state action args)]
    (if (instance? base/PassAction r)
      (reacl/return :action action)
      (reacl/return :app-state r))))

(defn- handle-action [binding e f & args]
  (handle-action+ binding e action-handler f args))

(extend-type base/HandleAction
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply handle-action binding e f args)))

(defn- action->return [a]
  (reduce reacl/merge-returned
          (map #(reacl/return :action %)
               (if (instance? base/MultiAction a)
                 (:actions a)
                 [a]))))

(defn- action-mapper [app-state action f args]
  (let [r (apply f app-state action args)]
    (action->return r)))

(defn- map-action [binding e f & args]
  (-> (instantiate binding e)
      (reacl/reduce-action action-mapper f args)))

(extend-type base/MapAction
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply map-action binding e f args)))

(defrecord ^:private NewIsoState [state])

(defn- id-state [st1 st2]
  ;; TODO: use = instead? or allow the user to specify it?
  (if (identical? st1 st2)
    reacl/keep-state
    st2))

(reacl/defclass ^:private local-state this astate [e initial]
  local-state [lstate initial]
  
  render
  (instantiate (reacl/use-reaction [astate lstate]
                                   (reacl/reaction this ->NewIsoState))
               e)
  
  handle-message
  (fn [msg]
    (cond
      (instance? NewIsoState msg)
      (let [{[new-app-state new-local-state] :state} msg]
        (reacl/return :app-state (id-state astate new-app-state)
                      :local-state (id-state lstate new-local-state))))))

(extend-type base/LocalState
  IReacl
  (-instantiate-reacl [{e :e initial :initial} binding]
    (local-state binding e initial)))

(defrecord ^:private AsyncAction [v])

;; TODO: can't we make the element returned by f fixed? - like an 'inject actions?'
(reacl/defclass ^:private with-async-action this state [f & args]
  local-state [deliver! (fn [action]
                          (reacl/send-message! this (AsyncAction. action)))]
  render
  (instantiate (reacl/bind this) (apply f deliver! args))

  handle-message
  (fn [msg]
    (cond
      (instance? AsyncAction msg)
      (reacl/return :action (:v msg)))))

(extend-type base/WithAsyncActions
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    (apply with-async-action binding f args)))

(reacl/defclass ^:private when-mounted this state [e f & args]
  refs [elem]
  
  component-did-mount
  (fn []
    ;; FIXME: it's only a dom node, if e is dom element - can we change that? (esp. fragments are a problem then?)
    (reacl/return :action (apply f (reacl/get-dom elem) args)))

  render (-> (instantiate (reacl/bind this) e)
             (reacl/refer elem)))

(extend-type base/WhenMounted
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply when-mounted binding e f args)))

(reacl/defclass ^:private when-unmounting this state [e f & args]
  refs [elem]

  component-will-unmount
  (fn []
    ;; FIXME: see mount.
    (reacl/return :action (apply f (reacl/get-dom elem) args)))

  render (-> (instantiate (reacl/bind this) e)
             (reacl/refer elem)))

(extend-type base/WhenUnmounting
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply when-unmounting binding e f args)))

(reacl/defclass ^:private after-update this state [e f & args]
  refs [elem]
  
  component-did-update
  (fn [prev-app-state prev-local-state prev-e prev-f & prev-args]
    ;; FIXME: it's only a dom node, if e is dom element - can we change that? (esp. fragments are a problem then?)
    ;; TODO: pass old/new state to f?
    (reacl/return :action (apply f (reacl/get-dom elem) args)))

  render (-> (instantiate (reacl/bind this) e)
             (reacl/refer elem)))

(extend-type base/AfterUpdate
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply after-update binding e f args)))

(defrecord ^:private MonitorMessage [new-state])

;; TODO: monitor that state change desire, or use 'after-update' for this?
(reacl/defclass ^:private monitor-state this state [e f & args]
  render
  (instantiate (reacl/use-reaction state (reacl/reaction this ->MonitorMessage)) e)

  handle-message
  (fn [msg]
    (cond
      (instance? MonitorMessage msg)
      (let [new-state (:new-state msg)]
        (reacl/merge-returned (action->return (apply f state new-state args))
                              (reacl/return :app-state new-state))))))

(extend-type base/MonitorState
  IReacl
  (-instantiate-reacl [{e :e f :f args :args} binding]
    (apply monitor-state binding e f args)))
