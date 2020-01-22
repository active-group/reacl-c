(ns reacld.test-util
  (:require [reacld.core :as core]
            [reacld.base :as base]
            [reacld.dom :as dom]
            [reacld.impl.reacl :as impl]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath]))

#_(defn resolve
  "If element is dynamic, then returns what it would be like, under the given state."
  [element state]
  
  )

(defn- ->ret [r]
  ;; reacl return => reacld return
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

;; TODO: the following needs an xpath variant.

(defn invoke-callback! [comp callback event]
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-action! [comp action]
  (->ret (r-tu/inject-return! comp (reacl/return :action action))))

(defn inject-change! [comp state]
  (->ret (r-tu/inject-change! comp state)))

;; TODO: inject-state! (which mutates local-state and returns app-state) ?

