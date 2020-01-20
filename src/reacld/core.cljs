(ns reacld.core
  (:require [reacld.impl :as impl]
            [reacld.base :as base]))

(defn run [dom e initial-state] ;; TODO: move to reacl/react namespace?
  (impl/run dom e initial-state))

;; TODO: send-message from outside to application and/or components? (and a 'handle-message' element?)

(defn with-async-actions [f & args]
  (base/WithAsyncActions. f args))

(defn fragment [& children]
  (base/Fragment. children))

(defn dynamic [f & args]
  (base/WithState. f args))

(defn id-lens
  ([v] v)
  ([_ v] v))

(defn focus [e lens]
  (if (= lens id-lens)
    e
    (base/Focus. e lens)))

(def pass-action (base/PassAction.))

(defn multi-action [& actions]
  (base/MultiAction. actions))

(def no-action (multi-action))

(defn handle-actions
  "Handle all action emitted by e into a new state which must be
  returned by `(f state action & args)`."
  [e f & args]
  (base/HandleAction. e f args))

(let [h (fn [app-state action pred f args]
          (if (pred action)
            (apply f app-state action args)
            pass-action))]
  (defn handle-action [e pred f & args]
    (handle-actions e h pred f args)))

(defn map-dynamic-actions [e f & args]
  (base/MapAction. e f args))

(let [h (fn [_ a f args] (apply f a args))]
  (defn map-actions [e f & args]
    (map-dynamic-actions e h f args)))

(defrecord ^:private SetStateAction [new-state])

(let [set (fn [state {new-state :new-state}]
            new-state)
      set? #(and (instance? SetStateAction %) %)]
  (defn with-state-setter [f & args]
    ;; TODO 'args' not really neeeded, if f is called immediately.
    ;; TODO: must that action be unique to 'this state'? Could it interfere with others? or use a message then?
    (-> (apply f ->SetStateAction args)
        (handle-action set? set))))

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
  (base/LocalState. (focus e lens) initial))

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
  ;; FIXME: because we wrap classes so much, keys can easily not be where they 'should' be - maybe copy the keys when wrapping?
  (base/Keyed. e key))

(defn when-mounted [e f & args]
  (base/WhenMounted. e f args))

(defn when-unmounting [e f & args]
  (base/WhenUnmounting. e f args))

(defn after-update [e f & args]
  (base/AfterUpdate. e f args))

(defrecord ^:private EffectAction [f args]
  base/Effect
  (-run-effect! [this] (apply f args)))

(defn effect-action [f & args]
  (EffectAction. f args))

(defn monitor-state [e f & args]
  (base/MonitorState. e f args))

(defrecord ^:private Mount [node f args])
(defrecord ^:private Unmount [node f args])

(let [on-mount (fn [[state mstate] a]
                 (if (instance? Mount a)
                   [state (apply (:f a) (:node a) (:args a))]
                   pass-action))
      on-unmount (fn [[_ mstate] a]
                   (if (instance? Unmount a)
                     (do (apply (:f a) (:node a) mstate (:args a))
                         no-action)
                     a))]
  (defn while-mounted [e mount! unmount! & args]
    (-> e
        (when-mounted ->Mount mount! args)
        (when-unmounting ->Unmount unmount! args)
        (handle-actions on-mount)
        (map-dynamic-actions on-unmount)
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

