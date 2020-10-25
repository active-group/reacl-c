(ns ^:no-doc reacl-c.impl.stores
  (:require [active.clojure.lens :as lens]))

;; TODO: rethink where comparisons = are really meaningful.

(defprotocol IStore
  (-get [this])
  (-set [this v]))

(defn store-get [s]
  (-get s))

(defn store-set! [s v] ;; TODO: + callback?
  (-set s v))

(defn store-update! [s f]
  (let [[v x] (f (store-get s))]
    (store-set! s v)
    x))

(defrecord DelegateStore [state set-state!]
  IStore
  (-get [_] state)
  (-set [_ v]
    ;; nil=callback - TODO: use?
    (when (not= v state)
      (set-state! v nil))))

(defn delegate-store [state set-state!]
  (DelegateStore. state set-state!))

(defrecord ^:private BaseStore [init-a a watcher]
  IStore
  (-get [_] @a)
  (-set [_ v]
    ;; Note: this means only persistent datastructured possible.
    (when (not= @a v)
      (reset! a v)
      (watcher v))))

(defn make-resettable-store! [init watcher]
  (BaseStore. (atom init) (atom init) watcher))

(defn maybe-reset-store! [store init]
  (assert (instance? BaseStore store))
  (if (not= init @(:init-a store))
    (do (reset! (:init-a store) init)
        (reset! (:a store) init)
        ;; watcher not called intentionally.
        true)
    false))

(defrecord ^:private ConcStore [s1 s2]
  IStore
  (-get [_] [(-get s1) (-get s2)])
  (-set [_ [v1 v2]]
    ;; Note: this means only persistent datastructured possible.
    (when (not= (-get s2) v2) (-set s2 v2))
    (when (not= (-get s1) v1) (-set s1 v1))))

(defn conc-store [s1 s2]
  (ConcStore. s1 s2))

(defrecord FocusStore [s lens]
  IStore
  (-get [this] (lens/yank (-get s) lens))
  (-set [this v] (-set s (lens/shove (-get s) lens v))))

(defn focus-store [s lens]
  (FocusStore. s lens))

(defrecord InterceptStore [s f]
  IStore
  (-get [this] (-get s))
  (-set [this v]
    (let [curr (-get s)]
      (when (not= curr v)
        (let [[v callback] (f curr v)]
          (-set s v)
          (when callback (callback)))))))

(defn handle-store-updates [s f]
  (InterceptStore. s f))

(def void-store
  (reify IStore
    (-get [this] nil)
    (-set [this v] (when-not (nil? v) (assert false))) ;; TODO: exn
    ))

