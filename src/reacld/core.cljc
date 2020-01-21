(ns reacld.core
  (:require [reacld.base :as base]))

;; TODO: send-message from outside to application and/or components? (and a 'handle-message' element?)

(defn with-async-actions [f & args]
  (base/->WithAsyncActions f args))

(defn fragment [& children]
  (base/->Fragment children))

(defn dynamic [f & args]
  (base/->WithState f args))

(defn id-lens
  ([v] v)
  ([_ v] v))

(defn focus [e lens]
  (if (= lens id-lens)
    e
    (base/->Focus e lens)))

(defn return [& args]
  (assert (even? (count args)) "Expected an even number of arguments.")
  (loop [args (seq args)
         state nil
         actions (transient [])]
    (if (empty? args)
      (base/->Returned state (persistent! actions))
      (let [arg (second args)
            nxt (nnext args)]
        (case (first args)
          (:state) (do (when-not (nil? state)
                         (assert false (str "A :state argument to return must be specified only once.")))
                       (recur nxt [arg] actions))
          (:action) (recur nxt state (conj! actions arg))
          (do (assert (contains? #{:state :action} (first args)) (str "Invalid argument " (first args) " to return."))
              (recur nxt state actions)))))))

(defn handle-action
  "Handles actions emitted by e, by evaluating `(f state action &
  args)` for each of them. That must return the result of
  calling [[return]] with either a new state, and maybe one or more
  other actions (or the given action unchanged). "
  [e f & args]
  (base/->HandleAction e f args))

(let [h (fn [st a f args]
          (return :action (apply f st a args)))]
  (defn map-dynamic-actions [e f & args]
    (handle-action h e f args)))

(let [h (fn [_ a f args] (apply f a args))]
  (defn map-actions [e f & args]
    (map-dynamic-actions e h f args)))

(defrecord ^:private SetStateAction [new-state])

(let [set (fn [state a]
            (if (instance? SetStateAction a)
              (return :state (:new-state a))
              (return :action a)))]
  (defn with-state-setter [f & args]
    ;; TODO 'args' not really neeeded, if f is called immediately.
    ;; TODO: must that action be unique to 'this state'? Could it interfere with others? or use a message then?
    (-> (apply f ->SetStateAction args)
        (handle-action set))))

(let [h (fn [set-state f args]
          (apply dynamic f set-state args))]
  (defn interactive [f & args]
    (with-state-setter h f args)))

(defn- id-merge [m1 m2]
  (reduce-kv (fn [r k v]
               (if (identical? (get r k) v)
                 r
                 (assoc r k v)))
             m1
             m2))

(defn merge-lens
  ([[s1 s2]] (merge s1 s2))
  ([[s1 s2] ns]
   ;; Note: if s1/s2 are records, then this restores that:
   ;; id-merge makes the result be identical? if all updated keys are identical?
   [(id-merge s1 (select-keys ns (keys s1)))
    (id-merge s2 (select-keys ns (keys s2)))]))

(defn add-state [initial lens e] ;; aka extend-state?
  (base/->LocalState (focus e lens) initial))

(defn hide-state [e initial lens]
  (add-state initial lens e))

(defn hide-merged-state [e initial]
  (add-state initial merge-lens e))

(defn- isolate-lens
  ([[outer inner]] inner)
  ([[outer inner] new-inner] [outer new-inner]))

(defn isolate-state [initial-state e]
  (add-state initial-state isolate-lens e))

(defn keyed [e key]
  (base/->Keyed e key))

(defn when-mounted [e f & args]
  (base/->WhenMounted e f args))

(defn when-unmounting [e f & args]
  (base/->WhenUnmounting e f args))

(defn after-update [e f & args]
  (base/->AfterUpdate e f args))

(defrecord ^:private EffectAction [f args]
  base/Effect
  (-run-effect! [this] (apply f args)))

(defn effect-action [f & args]
  (EffectAction. f args))

(defn monitor-state [e f & args]
  (base/->MonitorState e f args))

(defrecord ^:private Mount [node f args])
(defrecord ^:private Unmount [node f args])

(let [handle (fn [[state mstate] a]
               (condp instance? a
                 Mount
                 (return :state [state (apply (:f a) (:node a) (:args a))])
                 
                 Unmount
                 (do (apply (:f a) (:node a) mstate (:args a))
                     (return))

                 :else (return :action a)))]
  (defn while-mounted [e mount! unmount! & args]
    (-> e
        (when-mounted ->Mount mount! args)
        (when-unmounting ->Unmount unmount! args)
        (handle-action handle)
        (hide-state nil id-lens))))

(letfn [(mount [_ deliver! f args]
          (apply f deliver! args))
        (unmount [_ stop! deliver! f args]
          (stop!))
        (stu [deliver! f args]
          (while-mounted (fragment) ;; TODO: fragment, or wrap an existing element?
                         mount
                         unmount
                         deliver! f args))]
  (defn subscribe [f & args]
    (with-async-actions stu f args)))

;; TODO add a named class for make better use of reacl and react utils?
;; TODO: error boundary, getDerivedStateFromError now?.
;; TODO: need for getDerivedStateFromProps, getSnapshotBeforeUpdate ?
;; TODO: global events, like body onload?

;; TODO
#_(defn validate-state [e validator!]
  (-> (dynamic (fn [state]
                 (validator! state) ;; wrong state passed down!
                 e))
      (monitor-state (fn [old new]
                       ;; wrong state passed up!
                       (validator! new)
                       nil))))

(defmacro defn-dynamic [name state args & body]
  `(let [f# (fn [~state ~@args]
              ~@body)]
     ;; TODO: create fn with the correct arity.
     (defn ~name [& args#]
       (apply reacld.core/dynamic f# args#))))

(defmacro def-dynamic [name state & body]
  `(let [f# (fn [~state]
              ~@body)]
     (def ~name
       (reacld.core/dynamic f#))))

(defmacro defn-interactive [name state set-state args & body]
  `(let [f# (fn [~state ~set-state ~@args]
              ~@body)]
     ;; TODO: create fn with the correct arity.
     (defn ~name [& args#]
       (apply reacld.core/interactive f# args#))))

(defmacro def-interactive [name state set-state & body]
  `(let [f# (fn [~state ~set-state]
              ~@body)]
     (def ~name
       (reacld.core/interactive f#))))

(defmacro defn-effect [name args & body]
  `(let [eff# (fn ~args ~@body)]
     ;; TODO: create fn with the correct arity.
     (defn ~name [& args#]
       (apply reacld.core/effect-action eff# args#))))

(defmacro defn-subscription [name deliver! args & body]
  ;; TODO: rename; 'external-source'? 'primitive'?
  `(let [f# (fn [~deliver! ~@args] ~@body)]
     ;; TODO: create fn with the correct arity.
     (defn ~name [& args#]
       (apply reacld.core/subscribe f# args#))))

