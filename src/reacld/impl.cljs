(ns reacld.impl
  (:require [reacld.base :as base]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]))

(defn render [binding class & args]
  (if (and (reacl/reacl-class? class) (reacl/has-app-state? class))
    (apply class binding args)
    (apply class args)))

(defrecord ^:private ActionMessage [action])

;; TODO: send-message from outside to application and/or components?

(reacl/defclass ^:private toplevel this state [instantiate e]
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

(defn run [dom instantiate e initial-state]
  (reacl/render-component dom
                          toplevel
                          initial-state
                          instantiate e))

(defrecord ^:private Event [f ev])

(defn event? [k]
  (.startsWith (name k) "on"))

(defn dom-attributes? [v]
  (and (rdom/attributes? v)
       (not (satisfies? base/E v))))

(defn- analyze-dom-args [args]
  (if (empty? args)
    [{} args]
    (let [x (first args)]
      (if (dom-attributes? x)
        [x (rest args)]
        [{} args]))))

(defn- split-events [attrs]
  ;; OPT
  [(into {} (remove #(event? (first %)) attrs))
   (into {} (filter #(event? (first %)) attrs))])

(reacl/defclass dom+ this [dom-f attrs events children]
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
    (apply dom-f r-attrs children)))

(defn dom [binding instantiate dom-f & args]
  ;; optimize for dom element without event handlers:
  (let [[attrs_ children] (analyze-dom-args args)
        [attrs events] (split-events attrs_)
        r-children (map (partial instantiate binding) children)]
    (if (not-empty events)
      (dom+ dom-f attrs events r-children)
      (apply dom-f attrs r-children))))

(defn keyed [binding instantiate e key]
  (-> (instantiate binding e)
      (reacl/keyed key)))

(reacl/defclass with-state this state [instantiate f & args]
  ;; TODO messages?
  render
  (instantiate (reacl/bind this) (apply f state args)))

(defn focus [binding instantiate e lens]
  (instantiate (reacl/focus binding lens) e))

(defrecord ^:private Pass [])
(def pass-action (Pass.))

(defrecord ^:private MultiAction [actions])

(defn multi-action [& actions]
  (MultiAction. actions))

(reacl/defclass ^:private handle-action+ this state [instantiate e f & args]
  local-state [action-to-message (fn [_ action] (reacl/return :message [this (ActionMessage. action)]))]

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
    (if (= r pass-action)
      (reacl/return :action action)
      (reacl/return :app-state r))))

(defn handle-action [binding instantiate e f & args]
  (handle-action+ binding instantiate e action-handler f args))

(defn- action->return [a]
  (reduce reacl/merge-returned
          (map #(reacl/return :action %)
               (if (instance? MultiAction a)
                 (:actions a)
                 [a]))))

(defn- action-mapper [app-state action f args]
  (let [r (apply f action args)]
    (action->return r)))

(defn map-action [binding instantiate e f & args]
  (handle-action+ binding instantiate e action-mapper f args))

;; TODO: need to add args to reacl/reduce-action first
#_(defn map-action [binding instantiate e f & args]
  (-> (instantiate binding e)
      (reacl/reduce-action action-mapper f args)))

(defrecord ^:private NewIsoState [state])

(reacl/defclass local-state this astate [instantiate e initial lens]
  local-state [lstate initial]
  
  render
  (instantiate (reacl/use-reaction (lens [astate lstate])
                                   (reacl/reaction this ->NewIsoState))
               e)
  
  handle-message
  (fn [msg]
    (cond
      (instance? NewIsoState msg)
      (let [{new-state :state} msg
            [new-app-state new-local-state] (lens [astate lstate] new-state)]
        (reacl/merge-returned
         (if-not (identical? astate new-app-state) (reacl/return :app-state new-app-state) (reacl/return))
         (if-not (identical? lstate new-local-state) (reacl/return :local-state new-local-state) (reacl/return)))))))

(defrecord ^:private AsyncAction [v])

(reacl/defclass with-async-action this state [instantiate f & args]
  local-state [deliver! (fn [action]
                          (reacl/send-message! this (AsyncAction. action)))]
  render
  (instantiate (reacl/bind this) (apply f deliver! args))

  handle-message
  (fn [msg]
    (cond
      (instance? AsyncAction msg)
      (reacl/return :action (:v msg)))))

;; TODO: add component that emits action on-mount and on-unmount? or state? Would that be more primitive?
(reacl/defclass while-mounted this state [instantiate e mount unmount!]
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

  render
  (instantiate (reacl/bind this) e))

(defrecord ^:private MonitorMessage [new-state])

(reacl/defclass monitor-state this state [instantiate e f & args]
  render
  (instantiate (reacl/use-reaction state (reacl/reaction this ->MonitorMessage)) e)

  handle-message
  (fn [msg]
    (cond
      (instance? MonitorMessage msg)
      (let [new-state (:new-state msg)]
        (reacl/merge-returned (action->return (apply f state new-state args))
                              (reacl/return :app-state new-state))))))
