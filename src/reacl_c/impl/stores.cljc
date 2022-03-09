(ns ^:no-doc reacl-c.impl.stores
  (:require [active.clojure.lens :as lens]))

(defprotocol IStore
  (-get [this])
  (-set [this v]))

(defn store-get [s]
  (-get s))

(defn store-set! [s v] ;; TODO: + callback?
  (-set s v))

;; this is used for the toplevel
(deftype ^:private DelegateStore [^mutable state ^mutable set-state!]
  IStore
  (-get [this] (.-state this))
  (-set [this v]
    ;; Note: this means only persistent datastructures possible.
    (when (not= v (.-state this))
      ;; nil=callback - TODO: use?
      ((.-set-state! this) v nil))))

(defn make-delegate-store! [state set-state!]
  (DelegateStore. state set-state!))

(defn reset-delegate-store! [^DelegateStore s state set-state!]
  (assert (instance? DelegateStore s))
  (set! (.-state s) state)
  (set! (.-set-state! s) set-state!))

;; this is used for the local-state items
(deftype ^:private BaseStore [^mutable init-expr ^mutable state watcher]
  IStore
  (-get [this] (.-state this))
  (-set [this v]
    ;; Note: this means only persistent datastructures possible.
    (when (not= state v)
      (set! (.-state this) v)
      (watcher v))))

(defn make-resettable-store! [init-expr value-f watcher]
  (BaseStore. init-expr (value-f init-expr) watcher))

(defn maybe-reset-resettable-store! [^BaseStore store init-expr value-f]
  (assert (instance? BaseStore store))
  (if (not= init-expr (.-init-expr store))
    (do (set! (.-init-expr store) init-expr)
        (set! (.-state store) (value-f init-expr))
        ;; watcher not called intentionally.
        true)
    false))

(defrecord ^:private ConcStore [s1 s2]
  IStore
  (-get [_] [(-get s1) (-get s2)])
  (-set [_ [v1 v2]]
    (-set s2 v2)
    (-set s1 v1)))

(defn conc-store [s1 s2]
  (ConcStore. s1 s2))

(defrecord ^:private FocusStore [s lens]
  IStore
  (-get [_] (lens/yank (-get s) lens))
  (-set [_ v] (-set s (lens/shove (-get s) lens v))))

(defn focus-store [s lens]
  (let [conc? (instance? ConcStore s)]
    ;; Note: lens/first lens/second on a store with 'local-state' is a
    ;; common pattern, which might benefit largely from this
    ;; optimization:
    (cond
      (and conc? (= lens lens/first))
      (:s1 s)
      (and conc? (= lens lens/second))
      (:s2 s)
      :else
      (FocusStore. s lens))))

(defrecord ^:private InterceptStore [s f]
  IStore
  (-get [_] (-get s))
  (-set [_ v]
    (let [curr (-get s)]
      (when (not= curr v)
        (let [[v callback] (f curr v)]
          (-set s v)
          (when callback (callback)))))))

(defn intercept-store [s f]
  (InterceptStore. s f))

(def void-store
  (reify IStore
    (-get [_] nil)
    (-set [_ v]
      ;; setting to 'nil' is kindof more ok, as get returns nil too.
      (assert (nil? v) (str "Tried to put a value into a void store: " (pr-str v) ". Possible cause: trying to set the state of a static item.")))))
