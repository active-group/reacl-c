(ns reacl-c.main.react
  "Functions for using reacl-c within a React application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as core]
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            [reacl-c.impl.reacl :as impl]))

;; om, reagent, etc.?

;; TODO: refs, keys. (take 'props'?)

(defn react-controlled
  "Returns a React element running the given item. Current state and
  state changes are 'controlled' by the given arguments. Toplevel
  actions and effects are passed to the given handler function. If you
  want to effects to be executed, use [[main/execute-effects]]."
  [item state set-state! handle-action!]
  (impl/react-run item state set-state! handle-action!))

(defn react-uncontrolled
  "Returns a React element running the given item, which manages its
  state internally. Effects are executed implicitly, but for other
  actions emitted by the item `handle-action!` is called."
  [item initial-state & [handle-action!]]
  (react-controlled (core/local-state initial-state (core/focus lens/second (main/execute-effects item)))
                    nil
                    main/state-error
                    (or handle-action! main/action-error)))
