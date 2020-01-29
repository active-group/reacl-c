(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath]))

;; Note: just reusing rcore/test-util is not a good fit, esp. because
;; when/if xpath reveals much of the internals (dom class wrapper,
;; other classes that are an implementation detail). Maybe this should
;; be replaced by our own simulator.

(defn- ->ret [r]
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
  "Returns a new test environment to test the behavior of the given element."
  [element & [options]]
  ;; Note: this tests elements using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [class (rcore/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (rcore/return :message [(rcore/get-dom child) msg]))
                           render (-> (impl/instantiate (rcore/bind this) element)
                                      (rcore/refer child)))]
    (r-tu/env class options)))

(def get-component r-tu/get-component)

(defn mount!
  "Mounts the element of the given test environment with the given
  state, and returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/mount! env state)))

(defn update!
  "Updates the state of the element of the given test environment, and
  returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/update! env state)))

(def ^:dynamic *max-update-loops* 100)

(defn update!!
  "Updates the state of the element of the given test environment, and
  if the state is changed in reaction to that, then keeps on updating
  it. Returns actions and the final changed state, if it was changed
  at all. Throws if there are more than *max-update-loops* recursions,
  which are a sign for bug in the element."
  [env state]
  (loop [r (core/return)
         state state
         n 1]
    (let [r2 (update! env state)
          rm (base/merge-returned r r2)]
      (if (:opt-state r2)
        (let [state (first (:opt-state r2))]
          (when (> n *max-update-loops*)
            (throw (ex-info "Elements keeps on updating. Check any [[did-update]] elements, which should eventually reach a fixed state." {:intermediate-state state})))
          (recur rm state (inc n)))
        rm))))

(defn unmount!
  "Unmounts the element of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (->ret (r-tu/unmount! env)))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  {:per [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string element gives weird reacl errors.
  (->ret (r-tu/send-message! comp msg)))

(defn invoke-callback! [comp callback event]
  ;; TODO: enable this on dom class elements?! then we can remove the xpath case for the raw dom element.
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-action! [comp action]
  ;; Note: for dom tags in an xpath, the user will find the native element; so actions cannot be injected into them :-/
  ;; Rule: dom element => invoke-callback; non-dom elements => inject-action + inject-state-change.
  ;; TODO: document that, if it cannot be changed.
  (->ret (r-tu/inject-return! comp (rcore/return :action action))))

(defn inject-state-change! [comp state]
  (->ret (r-tu/inject-change! comp state)))
