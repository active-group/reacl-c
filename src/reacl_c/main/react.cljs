(ns reacl-c.main.react
  "Functions for using reacl-c within a React application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as core]
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            [reacl-c.impl.react :as impl]))

(defn send-message!
  "Sends a message to the instance of a react element returned
  by [[embed]], i.e. the current value of reference set on them."
  [comp msg & [callback]]
  ;; ...or the component of that element ('this'/'current' of a ref)
  (impl/react-send-message! comp msg callback))

(defn embed
  "Returns a React element embedding the given item. Current `state` and
  state changes (`set-state!`) are controlled by the given optional
  arguments.

  Toplevel actions and effects are passed to the optional
  `handler-action!` function. To have effects being executed
  implicitly, use [[reacl-c.main/execute-effects]]. Messages can be
  sent to the item via [[send-message!]]. A `key` and a `ref` can also
  be set in the options."
  [item & [options]]
  (assert (base/item? item) item)
  (let [{state :state
         set-state! :set-state!
         handle-action! :handle-action!
         ref :ref
         key :key} options]
    (impl/react-run item
                    state
                    (or set-state! main/state-error)
                    (or handle-action! main/action-error)
                    ref
                    key)))

