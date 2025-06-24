(ns reacl-c.main.react
  "Functions for using reacl-c within a React application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            [reacl-c.impl.react :as impl]))

(defn send-message!
  "Sends a message to an embedded item via a ref to it."
  [ref msg]
  (impl/send-message-to-ref! ref msg))

(defn embed
  "Returns a React element embedding the given item. Current `:state` and
  state changes (`:set-state!`) are controlled by the corrensponding options.

  Toplevel actions and effects are passed to the optional
  `:handle-action!` function. To have effects being executed
  implicitly, use [[reacl-c.main/execute-effects]]. Messages can be
  sent to the item via [[send-message!]]. A `:key` and a `:ref` can also
  be set in the options."
  [item & [options]]
  (assert (base/item? item) item)
  ;; TODO: extract key from item within react-run
  (assert (every? #{:state :set-state! :handle-action! :key} (keys options)))
  ;; Note: when processing a state change and an action, it is assumed
  ;; that the value passed to set-state! will eventually be the one
  ;; passed down as 'state'.
  (let [{state :state
         set-state! :set-state!
         handle-action! :handle-action!
         key :key} options]
    (impl/react-run item
                    state
                    (or set-state! main/state-error)
                    (or handle-action! main/action-error)
                    key)))
