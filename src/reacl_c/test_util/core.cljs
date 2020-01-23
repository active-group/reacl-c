(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath]))

;; Note: just reusing reacl/test-util is not a good fit, esp. because
;; when/if xpath reveals much of the internals (dom class wrapper,
;; other classes that are an implementation detail). Maybe this should
;; be replaced by our own simulator.

(defn- ->ret [r]
  ;; reacl return => reacl-c return
  (apply core/return
         (apply concat
                (let [s (reacl/returned-app-state r)]
                  (when-not (reacl/keep-state? s)
                    [:state s]))
                (map (fn [a]
                       [:action a])
                     (reacl/returned-actions r)))))

(defn env
  "Returns a new test environment to test the behavior of the given element."
  [element & [options]]
  ;; Note: this tests elements using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [class (reacl/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (reacl/return :message [(reacl/get-dom child) msg]))
                           render (-> (impl/instantiate (reacl/bind this) element)
                                      (reacl/refer child)))]
    (r-tu/env class options)))

(def get-component r-tu/get-component)

(defn mount!
  "Mount the element of the given test environment with the given
  state, and return actions and maybe a changed state."
  [env state]
  (->ret (r-tu/mount! env state)))

(defn update!
  "Update the state of the element of the given test environment, and
  return actions and maybe a changed state."
  [env state]
  (->ret (r-tu/update! env state)))

(defn unmount!
  "Unmount the element of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (->ret (r-tu/unmount! env)))

(defn send-message!
  "Send a message to the element of the given test environment, and
  return actions and maybe a changed state."
  [env msg]
  (->ret (r-tu/send-message! env msg)))

(defn invoke-callback! [comp callback event]
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-action! [comp action]
  ;; Note: for dom tags in an xpath, the user will find the native element; so actions cannot be injected into them :-/
  ;; Rule: dom element => invoke-callback; non-dom elements => inject-action + inject-state-change.
  ;; TODO: document that, if it cannot be changed.
  (->ret (r-tu/inject-return! comp (reacl/return :action action))))

(defn inject-state-change! [comp state]
  (->ret (r-tu/inject-change! comp state)))
