(ns reacl-c.browser
  (:require [reacl-c.impl.reacl :as impl]))

(defn run
  "Runs the given item as an application underneath the given
  native `dom` node, and with the given `initial-state`."
  [dom item initial-state]
  (impl/run dom item initial-state))

(defn lift-reacl
  "Returns an item implemented by the given Reacl class and
  arguments. The Reacl app-state will be the returned item's state,
  messages sent to the item will be forwarded to the class, and
  actions emitted by the class are emitted as actions from the
  returned item."
  [class & args]
  (apply impl/lift-reacl class args))

(defn reacl-render
  "Returns a Reacl element or component rending the given item,
  and with the given state `binding`, which determines the state of
  the item."
  [binding item]
  ;; TODO: with or without effects being executed? (currently without)
  (impl/reacl-render binding item))
