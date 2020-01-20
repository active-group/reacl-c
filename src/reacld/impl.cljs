(ns reacld.impl
  (:require [reacld.base :as base]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]))

#_(defn render [binding class & args]
  (if (and (reacl/reacl-class? class) (reacl/has-app-state? class))
    (apply class binding args)
    (apply class args)))

;; TODO: offer lifting existing Reacl classes as leaf elements.
#_(defn- ignore-binding [binding class & args]
  (apply class args))

#_(defn- lift [class-or-function & fixed-args]
  (fn [& args]
    ;; TODO: checking arity at least would be nice
    (apply elem class-or-function (concat fixed-args args))))

(defprotocol IReacl
  (-instantiate-reacl [this binding]))

(defn- instantiate [binding e]
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    (string? e) (rdom/fragment e)
    :else
    (assert false e) #_(rdom/fragment e)))

(defn- instantiate-child [binding e]
  (cond
    (satisfies? IReacl e) (-instantiate-reacl e binding)
    (string? e) e
    :else (assert false e)))

(defrecord ^:private ActionMessage [action])

;; TODO: send-message from outside to application and/or components?

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

(defn run [dom e initial-state]
  (reacl/render-component dom
                          toplevel
                          initial-state
                          e))

(defrecord ^:private Event [f ev])

(reacl/defclass dom+ this [type attrs events children]
  handle-message
  (fn [msg]
    (cond
      (instance? Event msg)
      (let [{f :f ev :ev} msg]
        (if-let [a (f ev)]
          (reacl/return :action a)
          (reacl/return)))
      
      ;; TODO else messages to children?
      ))
  
  render
  (let [r-events (into {} (map (fn [[k v]]
                                 [k (fn [ev]
                                      ;; TODO: make fixed via cache (or local state)
                                      (reacl/send-message! this (Event. v ev)))])
                               events))
        r-attrs (merge attrs r-events)]
    (apply rdom/element type r-attrs children)))

(defn dom [binding type attrs events children]
  ;; optimize for dom element without event handlers:
  (let [r-children (map (partial instantiate-child binding) children)]
    (if (not-empty events)
      (dom+ type attrs events r-children)
      (apply rdom/element type attrs r-children))))

(extend-type base/Dom
  IReacl
  (-instantiate-reacl [{type :type attrs :attrs events :events children :children} binding]
    (dom binding type attrs events children)))

(defn keyed [binding e key]
  (-> (instantiate binding e)
      (reacl/keyed key)))

(extend-type base/Keyed
  IReacl
  (-instantiate-reacl [{e :e key :key} binding]
    (keyed binding e key)))

(reacl/defclass with-state this state [f & args]
  ;; TODO messages?
  render
  (instantiate (reacl/bind this) (apply f state args)))

(extend-type base/WithState
  IReacl
  (-instantiate-reacl [{f :f args :args} binding]
    (apply with-state binding f args)))

(defn focus [binding e lens]
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

(defn handle-action [binding e f & args]
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
  (let [r (apply f action args)]
    (action->return r)))

(defn map-action [binding e f & args]
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

(reacl/defclass local-state this astate [e initial]
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

;; TODO: can't we make the element returned by f fixed?
(reacl/defclass with-async-action this state [f & args]
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
    (apply with-async-action f args)))

;; TODO: add component that emits action on-mount and on-unmount? or state? Would that be more primitive?
(reacl/defclass ^:private while-mounted+ this [raw-element mount unmount!]
  validate (do (assert (ifn? mount))
               (assert (ifn? unmount!)))
  local-state [mounted-state nil]

  component-did-mount
  (fn []
    (reacl/return :local-state (mount)))

  component-will-unmount
  (fn []
    (unmount! mounted-state) ;; TODO: really a side effect?
    (reacl/return :local-state nil))

  render raw-element)

(defn while-mounted [binding e mount unmount!]
  ;; TODO: does it really have an advantage to instantate before? (might be not= more often?)
  (while-mounted+ (instantiate binding e) mount unmount!))

(extend-type base/WhileMounted
  IReacl
  (-instantiate-reacl [{e :e mount :mount unmount :unmount} binding]
    (while-mounted binding e mount unmount)))

(defrecord ^:private MonitorMessage [new-state])

(reacl/defclass monitor-state this state [e f & args]
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
