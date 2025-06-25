(ns ^:no-doc reacl-c.base
  ;; Internal namespace; don't depend on it.
  (:require #?(:cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [active.clojure.record :as r])
            [active.clojure.functions :as f]
            [schema.core :as s]))

(defprotocol E
  (-is-dynamic? [this] "If the item depends on the state in rendering or behaviour. Some optimizations can be applied if false."))

(defn item? [v]
  (or (string? v)
      (nil? v)
      (satisfies? E v)))

(defn is-dynamic? [item]
  (if (or (string? item)
          (nil? item))
    false
    (-is-dynamic? item)))

(defn- item-list-pred [v]
  ;; nil for easier 'conditional rendering'
  (or (nil? v) (item? v)))

(defn assert-item-list [where cs] ;; -> more like a util?
  (assert (every? item-list-pred cs)
          (let [c (first (filter #(not (item-list-pred %)) cs))]
            (str "Expected only items or strings, but this is neither: " (pr-str c)))))

;; Note: These refs have to be compatible with react/createRef (it's
;; mostly here to avoid too much namespace dependeny)
(deftype ^:priviate Ref [^:mutable current])

(defn make-ref []
  (Ref. nil))

(deftype NameId [name])

(defn make-name-id [s]
  {:pre [(string? s)]}
  (NameId. s))

(defn name-id? [v]
  (instance? NameId v))

(defn name-id-name [^NameId v]
  {:pre [(name-id? v)]}
  (.-name v))

(r/define-record-type Fragment
  (really-make-fragment children)
  really-fragment?
  [children fragment-children]
  E
  (-is-dynamic? [{children :children}]
    (some is-dynamic? children)))

(defn make-fragment [children]
  (if (empty? children)
    nil
    (really-make-fragment children)))

(defn fragment? [v]
  ;; nil is an empty fragment
  (or (nil? v) (really-fragment? v)))

(r/define-record-type Dynamic
  (make-dynamic name-id f args)
  dynamic?
  [name-id dynamic-name-id ;; optional
   f dynamic-f
   args dynamic-args]
  E
  (-is-dynamic? [_] true))

(r/define-record-type Static
  (make-static name-id f args)
  static?
  [name-id static-name-id ;; optinoal
   f static-f
   args static-args]
  E
  (-is-dynamic? [{e :e}] false))

(r/define-record-type WithRefs
  (make-with-refs n f args)
  with-refs?
  [n with-refs-n
   f with-refs-f
   args with-refs-args]
  E
  (-is-dynamic? [_] true))

(r/define-record-type WithAsync
  (make-with-async f args)
  with-async?
  [f with-async-f
   args with-async-args]
  E
  (-is-dynamic? [_] true))

(r/define-record-type Focus
  (make-focus e lens)
  focus?
  [e focus-e
   lens focus-lens]
  E
  (-is-dynamic? [{e :e}] (is-dynamic? e)))

(r/define-record-type Initializer
  (make-initializer f args)
  initializer?
  [f initializer-f
   args initializer-args])

(defn eval-local-state-init [init]
  (if (initializer? init)
    (apply (initializer-f init) (initializer-args init))
    init))

(r/define-record-type LocalState
  (make-local-state e initial)
  local-state?
  [e local-state-e
   initial local-state-initial]
  E
  (-is-dynamic? [{e :e}]
                #_(is-dynamic? e)
                ;; assume it is (why add local-state if not)
                true))

(r/define-record-type HandleAction
  (make-handle-action e f pred)
  handle-action?
  [e handle-action-e
   f handle-action-f
   pred handle-action-pred]
  E
  (-is-dynamic? [{e :e}] true))

(r/define-record-type Refer
  (make-refer e ref)
  refer?
  [e refer-e
   ref refer-ref]
  E
  (-is-dynamic? [{e :e}] (is-dynamic? e)))

(defn deref-ref [r]
  (let [r (if (refer? r)
            (refer-ref r)
            r)]
    (.-current r)))

(r/define-record-type HandleStateChange
  (make-handle-state-change e f)
  handle-state-change?
  [e handle-state-change-e
   f handle-state-change-f]
  E
  (-is-dynamic? [_] (is-dynamic? e)))

(r/define-record-type HandleMessage
  (make-handle-message f e)
  handle-message?
  [f handle-message-f
   e handle-message-e]
  E
  (-is-dynamic? [this] true))

(r/define-record-type Named
  (make-named name-id e validate-state!)
  named?
  [name-id named-name-id
   e named-e
   validate-state! named-validate-state!]
  E
  (-is-dynamic? [{e :e validate-state! :validate-state!}] (or (some? validate-state!) (is-dynamic? e))))

(r/define-record-type HandleError
  (make-handle-error e f)
  handle-error?
  [e handle-error-e
   f handle-error-f]
  E
  (-is-dynamic? [_] (is-dynamic? e)))

(r/define-record-type Keyed
  (make-keyed e key)
  keyed?
  [e keyed-e
   key keyed-key]
  E
  (-is-dynamic? [{e :e}] (is-dynamic? e)))

(r/define-record-type Lifecycle
  (make-lifecycle init finish)
  lifecycle?
  [init lifecycle-init
   finish lifecycle-finish]
  E
  (-is-dynamic? [_] true))

(defrecord KeepState [])
(def keep-state (KeepState.))

(defn keep-state? [v]
  (= keep-state v))

(r/define-record-type Returned
  (make-returned state actions messages)
  returned?
  [state returned-state
   actions returned-actions
   messages returned-messages])

(defn return-schema [state message action]
  {:state (s/conditional keep-state? (s/eq keep-state)
                         :else state)
   :messages [message]
   :actions [action]})

(def empty-return (make-returned keep-state [] []))

(defn merge-returned
  [& rs]
  {:pre [(every? returned? rs)]}
  (loop [r1 empty-return
         rs rs]
    (if (empty? rs)
      r1
      (let [r2 (first rs)
            rm (make-returned (if (not= keep-state (:state r2)) (:state r2) (:state r1))
                              (vec (concat (:actions r1) (:actions r2)))
                              (vec (concat (:messages r1) (:messages r2))))]
        (recur rm (rest rs))))))

(defprotocol Application
  (-stop! [this])
  (-send-message! [this msg]))

(r/define-record-type Effect
  (make-effect f args)
  simple-effect?
  [f effect-f
   args effect-args])

(r/define-record-type ComposedEffect
  (make-composed-effect eff first-f rest-fs)
  composed-effect?
  [eff composed-effect-eff
   first-f composed-effect-first-f
   rest-fs composed-effect-rest-fs])

(defn map-composed-effect [e f]
  (make-composed-effect (f (composed-effect-eff e))
                        (f/comp f (composed-effect-first-f e))
                        (map #(f/comp f %) (composed-effect-rest-fs e))))

(defn effect? [v]
  (or (simple-effect? v)
      (composed-effect? v)))

(defn- run-composed-effect! [eff run-recur!]
  (reduce (fn [[value ret] next-f]
            (let [next-eff (next-f value)
                  [value2 ret2] (run-recur! next-eff)]
              [value2 (merge-returned ret ret2)]))
          (run-recur! (composed-effect-eff eff))
          (cons (composed-effect-first-f eff)
                (composed-effect-rest-fs eff))))

(declare run-effect!)

(defn run-effect!
  "Returns a tuple [value ret]. If an effect returns a [[return]]
  value, then 'value' is the returned state, and 'ret' everything else.
  For any other value, 'ret' is empty."
  [eff]
  ;; Note: the test-runner has a separate implementation of this (which uses an emulator of effects)
  {:pre [(effect? eff)]}
  (if (composed-effect? eff)
    ;; run composed effect
    (run-composed-effect! eff run-effect!)
    
    ;; run simple effect
    (let [result (apply (:f eff) (:args eff))]
      (if (returned? result)
        (if (not= keep-state (:state result))
          [(:state result) (assoc result :state keep-state)]
          [nil result])
        [result empty-return]))))
