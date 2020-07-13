(ns reacl-c.impl.tu-reacl
  "Implementation of test-utils with Reacl test-utils, reusing the Reacl implementation of items."
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.impl.reacl :as impl]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as rxpath]))

;; Note: just reusing rcore/test-util is not a good fit, esp. because
;; when/if xpath reveals much of the internals (dom class wrapper,
;; other classes that are an implementation detail). Maybe this should
;; be replaced by our own simulator.


(defn- xpath-named [n]
  (cond
    (string? n)
    ;; a dom tag might always be one element under the corresponding
    ;; dom wrapper class, but should appear on the 'same level' for
    ;; the user.
    (rxpath/or (rxpath/tag n)
               (rxpath/comp (rxpath/type (impl/dom-class-for-type n)) rxpath/children (rxpath/tag n)))

    
    (some? (core/meta-name-id n)) (rxpath/class (impl/named (core/meta-name-id n)))
    (base/name-id? n) (rxpath/class (impl/named n))
    :else (assert false n)))

(defn- xpath-item
  "Matches an item similar to the given one. The given item is
  intepreted as a pattern, and may contain 'less' attributes or
  children."
  [v]
  (impl/xpath-pattern v))

(defn- ->ret [r]
  (assert (rcore/returned? r))
  ;; reacl return => reacl-c return
  (apply core/return
         (apply concat
                (let [s (rcore/returned-app-state r)]
                  (when-not (rcore/keep-state? s)
                    [:state s]))
                (map (fn [a]
                       [:action a])
                     (rcore/returned-actions r)))))

(defn env
  [item & [options]]
  ;; Note: this tests items using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [this-env (atom nil)
        ;; Note: this basically replicates impl/toplevel - can't reuse
        ;; that easily, because it would create another level (maybe
        ;; with a change to get-components?)
        action-reducer (fn [_ a]
                         (if (base/effect? a)
                           ;; execute effect, ignoring it's value
                           (let [[v ret] (base/run-effect! a)]
                             (impl/transform-return ret))
                           ;; or return actions
                           (rcore/return :action a)))
        class (rcore/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (rcore/return :message [(rcore/get-dom child) msg]))
                           render (-> (impl/instantiate (rcore/bind this) item)
                                      (rcore/refer child)
                                      (rcore/reduce-action action-reducer)))]
    (reset! this-env (r-tu/env class options))
    @this-env))

(defn- get-root-component [env]
  (r-tu/get-component env))

(defn get-components [env]
  ;; we always wrap the "env" class around the main component.
  (let [env-comp (get-root-component env)]
    (array-seq (.-children env-comp))))

(defn- search-root [env]
  (if (r-tu/env? env)
    ;; Note: the root can have multiple children (if a fragment is used at toplevel)
    [(get-root-component env) rxpath/children]
    [env rxpath/self]))

(defn find-all [env item]
  (let [[c base] (search-root env)]
    (rxpath/select-all c (rxpath/comp base rxpath/all (xpath-item item)))))

;; TODO: remove/deprecate
(defn find-all-named [env thing]
  (let [[c base] (search-root env)]
    (rxpath/select-all c (rxpath/comp base rxpath/all (xpath-named thing)))))

(defn with-env-return [env f]
  (->ret (r-tu/with-component-return (get-root-component env)
           (fn [comp]
             (f)))))

(defn mount!
  [env state]
  (->ret (r-tu/mount! env state)))

(defn update!
  [env state]
  (->ret (r-tu/update! env state)))

(defn unmount!
  [env]
  (->ret (r-tu/unmount! env)))

;; TODO: make find-env public in reacl.beta... then reduce !! fns to f+push

(defn- dom-node? [comp]
  (string? (.-type comp)))

(defn invoke-callback!
  [comp callback event]
  (assert (dom-node? comp) (str "Must be a dom node to invoke callbacks: " (pr-str comp)))
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn invoke-callback!!
  [comp callback event]
  (assert (dom-node? comp) (str "Must be a dom node to invoke callbacks: " (pr-str comp)))
  (->ret (r-tu/invoke-callback!! comp callback event)))

(defn- inject-return_ [comp ret f]
  {:pre [(some? comp)]}
  (let [comp (if (dom-node? comp)
               ;; When a dom item was selected, we have the raw dom node here;
               ;; to injecting a return, there must be a wrapper class
               ;; around it.
               ;; Also, it must be the corresponding dom wrapper class, because
               ;; otherwise the return could be different (e.g. if it's a
               ;; handle-action, returning from there would be really different)
               (let [p (.-parent comp)]
                 (when-not (and p (.-type p) (impl/dom-class-for-type (.-type comp)))
                   (assert false "The given node is a dom node without any event handlers attached. It's not possible to inject something there."))
                 p)
               comp)]
    (->ret (f comp (impl/transform-return ret)))))

(defn inject-return!
  [comp ret]
  (inject-return_ comp ret r-tu/inject-return!))

(defn inject-return!!
  [comp ret]
  (inject-return_ comp ret r-tu/inject-return!!))

(defn send-message!
  [comp msg]
  {:pre [(some? comp)]}
  ;; TODO: should be possible with something like this, but it isn't:
  #_(inject-return! comp (core/return :message [(reify base/Ref (-deref-ref [_] (.-instance comp))) msg]))
  ;; TODO: better check the comp? sending a message to a fragment/dom/string item gives weird reacl errors.
  (->ret (r-tu/send-message! comp msg)))

(defn send-message!!
  [comp msg]
  {:pre [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string item gives weird reacl errors.
  (->ret (r-tu/send-message!! comp msg)))

(defn- find-ref [env ref]
  ;; Note: deref host would return the 'reacl component' instead of the test renderer component
  (let [comp (core/deref ref)]
    (let [tcomp (.find (get-root-component env)
                       (fn [ti]
                         (= (.-instance ti) comp)))]
      tcomp)))

