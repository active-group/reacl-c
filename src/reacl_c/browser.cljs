(ns reacl-c.browser
  (:require [reacl-c.impl.reacl :as impl]))

(defn run
  "Runs the given element `e` an application underneath the given
  native `dom` node, and with the given `initial-state`."
  [dom e initial-state]
  (impl/run dom e initial-state))

(defn lift
  "Returns an element implemented by the given Reacl class and arguments."
  [class & args]
  (apply impl/lift class args))

(defn render
  "Returns a Reacl element or component implementing the given `element`, and with the given state `binding`."
  [binding element]
  (impl/instantiate binding element))

;; render-child ?
