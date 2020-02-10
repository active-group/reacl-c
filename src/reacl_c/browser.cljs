(ns reacl-c.browser ;; TODO: rename 'reacl' or react?
  (:require [reacl-c.impl.reacl :as impl]))

(defn run
  "Runs the given item as an application underneath the given
  native `dom` node, and with the given `initial-state`."
  [dom item initial-state]
  (impl/run dom item initial-state))

(defn lift
  "Returns an item implemented by the given Reacl class and arguments."
  [class & args]
  (apply impl/lift class args))

(defn render
  "Returns a Reacl element or component implementing the given item,
  and with the given state `binding`."
  [binding item]
  (impl/instantiate binding item))

;; render-child ?
