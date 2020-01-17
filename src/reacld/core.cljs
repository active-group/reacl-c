(ns reacld.core
  (:require [reacld.impl :as impl]
            [reacld.base :as base]
            [reacl2.core :as reacl]
            [reacl2.dom :as rdom]))

(defn- instantiate-child [binding v]
  (cond
    (satisfies? base/E v) (base/-instantiate v binding)
    
    #_(reacl/reacl-class? e) #_(if (reacl/has-app-state? e)
                             (e binding)
                             (e))

    ;; any other arg as is (strings, components, elements)
    :else v
    ))

(defn- instantiate [binding e]
  ;; OPT: fragment only needed if it is a string etc.
  ;; TODO: e=nil an empty fragment?
  (rdom/fragment (instantiate-child binding e)))

(defn- instantiate-toplevel [binding e]
  ;; TODO: must have an app-state?
  (cond
    (satisfies? base/E e) (base/-instantiate e binding)

    :else (assert false (pr-str e))
    #_(reacl/reacl-class? e) #_(e binding)))

(defn run [dom e initial-state]
  (impl/run dom instantiate-toplevel e initial-state))

(defrecord Element [f args]
  base/E
  (-instantiate [this binding]
    (apply f binding args)))

(defn- invoke-ctor [ctor args]
  (let [{f :f fixed-args :fixed-args} ctor]
    ;; TODO: some way to check at least the arity; very hard to find places where too many args were given (also too little?!)
    (Element. f (concat fixed-args args))))

#_(defrecord Container [f fixed-args]
  IFn
  (-invoke [this] (invoke-ctor this nil))
  (-invoke [this a1] (invoke-ctor this (list a1)))
  (-invoke [this a1 a2] (invoke-ctor this (list a1 a2)))
  (-invoke [this a1 a2 a3] (invoke-ctor this (list a1 a2 a3)))
  (-invoke [this a1 a2 a3 a4] (invoke-ctor this (list a1 a2 a3 a4)))
  (-invoke [this a1 a2 a3 a4 a5] (invoke-ctor this (list a1 a2 a3 a4 a5)))
  (-invoke [this a1 a2 a3 a4 a5 a6] (invoke-ctor this (list a1 a2 a3 a4 a5 a6)))
  (-invoke [this a1 a2 a3 a4 a5 a6 a7] (invoke-ctor this (list a1 a2 a3 a4 a5 a6 a7)))
  (-invoke [this a1 a2 a3 a4 a5 a6 a7 a8] (invoke-ctor this (list a1 a2 a3 a4 a5 a6 a7 a8)))
  (-invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9] (invoke-ctor this (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
  (-invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10] (invoke-ctor this (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)))
  ;; TODO: rest
  )

(defn- prim-elem [class & args]
  (Element. impl/render (cons class args)))

(defn- prim-container [class & fixed-args]
  (fn [& args]
    ;; TODO: checking arity at least would be nice
    (apply prim-elem class (concat fixed-args args))))

(defn- dom [f]
  (prim-container impl/dom instantiate-child f))

(def div (dom rdom/div))
(def input (dom rdom/input))
(def form (dom rdom/form))
(def button (dom rdom/button))
(def h3 (dom rdom/h3))
(def fragment (dom rdom/fragment))
;; TODO: rest of dom

(defn dynamic [f & args]
  (apply prim-elem impl/dynamic instantiate f args))

(defn focus [e lens]
  (prim-elem impl/focus instantiate e lens))

(def pass-action impl/pass-action)

(defn multi-action [& actions]
  (apply impl/multi-action actions))

(def no-action (multi-action))

(defn handle-actions [e f & args]
  (apply prim-elem impl/handle-action instantiate e f args))

(let [h (fn [app-state action pred f args]
          (if (pred action)
            (apply f app-state action args)
            pass-action))]
  (defn handle-action [e pred f & args]
    (handle-actions e h pred f args)))

(defn map-actions [e f & args]
  (apply prim-elem impl/map-action instantiate e f args))

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

;; EXAMPLE:
#_(defn input-text [& [attrs]]
  ;; TODO: statify
  (dynamic-dom (fn [state set-state]
                 (input (merge {:type "text"}
                               attrs
                               {:value state
                                :onchange (fn [ev]
                                            (set-state (.-value (.-target ev))))})))))

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

(defn id-lens
  ([v] v)
  ([_ v] v))

(defn add-state [initial lens e] ;; aka extend-state?
  (prim-elem impl/add-state instantiate initial lens e))

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
  ;; TODO: because we wrap classes so much, keys can easily not be where they 'should' be - maybe copy the keys when wrapping?
  (prim-elem impl/keyed instantiate e key))

(defn while-mounted [e mount unmount]
  (prim-elem impl/while-mounted instantiate e mount unmount))

(defrecord ^:private EffectAction [f args]
  base/Effect
  (-run-effect! [this] (apply f args)))

(defn effect-action [f & args]
  (EffectAction. f args))

(defn with-async-actions [f & args]
  (apply prim-elem impl/with-async-action instantiate f args))

(letfn [(stu [deliver! f args]
          (while-mounted (fragment) ;; TODO: fragment, or wrap an existing element?
                         (fn []
                           (apply f deliver! args))
                         (fn [stop!]
                           (stop!))))]
  (defn subscribe [f & args]
    (with-async-actions stu f args)))

(defn monitor-state [e f & args]
  (apply prim-elem impl/monitor-state instantiate e f args))
