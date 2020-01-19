(ns reacld.core
  (:require [reacld.impl :as impl]
            [reacld.base :as base]
            [reacl2.core :as reacl]
            [reacl2.dom :as rdom]))

(defn- instantiate-child
  "Instantiates e, returning a value suitable as a child of a Reacl dom element."
  [binding e]
  (cond
    (satisfies? base/E e) (base/-instantiate e binding)

    ;; any other arg as is (esp. strings, maybe components and other dom elements)
    :else e))

(defn- instantiate 
  "Instantiates e, returning a value suitable as the value of a Reacl render clause."
  [binding e]
  ;; TODO: e=nil an empty fragment?
  (cond
    (satisfies? base/E e) (base/-instantiate e binding)
    
    :else (rdom/fragment e)))

(defn run [dom e initial-state]
  (impl/run dom instantiate e initial-state))

(defrecord Element [f args]
  base/E
  (-instantiate [this binding]
    ;; must return a Reacl component or dom element.
    (apply f binding args)))

(defn- ignore-binding [binding class & args]
  (apply class args))

(defn- elem [class-or-function & args]
  (if (and (reacl/reacl-class? class-or-function) (not (reacl/has-app-state? class-or-function)))
    (Element. ignore-binding (cons class-or-function args))
    (Element. class-or-function args)))

(defn- wrapper-elem [wrapper e & args]
  ;; wrapper must take an instantiate fn as first arg, then e, then args.
  (apply elem wrapper instantiate e args))

(defn- lift [class-or-function & fixed-args]
  (fn [& args]
    ;; TODO: checking arity at least would be nice
    (apply elem class-or-function (concat fixed-args args))))

(defn- dom [f]
  ;; TODO: use raw dom element if no Element children and no events (and maybe split the two things)
  (lift impl/dom instantiate-child f))

(def div (dom rdom/div))
(def input (dom rdom/input))
(def form (dom rdom/form))
(def button (dom rdom/button))
(def h3 (dom rdom/h3))
(def fragment (dom rdom/fragment))
;; TODO: rest of dom; other namespace?

(defn dynamic [f & args]
  ;; FIXME: we can't copy/change the key at runtime (can we?) so always give this a fixed but random key? or add dynamic-keyed? or leave it up to the user to use 'keyed' at the right places.
  (apply elem impl/with-state instantiate f args))

(defn focus [e lens]
  (wrapper-elem impl/focus e lens))

(def pass-action impl/pass-action)

(defn multi-action [& actions]
  (apply impl/multi-action actions))

(def no-action (multi-action))

(defn handle-actions [e f & args]
  (apply wrapper-elem impl/handle-action e f args))

(let [h (fn [app-state action pred f args]
          (if (pred action)
            (apply f app-state action args)
            pass-action))]
  (defn handle-action [e pred f & args]
    (handle-actions e h pred f args)))

(defn map-actions [e f & args]
  (apply wrapper-elem impl/map-action e f args))

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
  (wrapper-elem impl/local-state e initial lens))

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
  (wrapper-elem impl/keyed e key))

(defn while-mounted [e mount unmount]
  (wrapper-elem impl/while-mounted e mount unmount))

(defrecord ^:private EffectAction [f args]
  base/Effect
  (-run-effect! [this] (apply f args)))

(defn effect-action [f & args]
  (EffectAction. f args))

(defn with-async-actions [f & args]
  (apply elem impl/with-async-action instantiate f args))

;; Note: subscriptions are already a high level feature to 'deliver async actions while mounted'.
(letfn [(stu [deliver! f args]
          (while-mounted (fragment) ;; TODO: fragment, or wrap an existing element?
                         (fn []
                           (apply f deliver! args))
                         (fn [stop!]
                           (stop!))))]
  (defn subscribe [f & args]
    (with-async-actions stu f args)))

(defn monitor-state [e f & args]
  (apply wrapper-elem impl/monitor-state e f args))

;; TODO: allow access to side-effects of rendering, like .clientHeight of the dom (did-update + ref maybe)
