(ns reacl-c.main.react
  "Functions for using reacl-c within a React application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.base :as base]
            [reacl-c.core :as c :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
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
  sent to the item via [[send-message!]]."
  [item & [options]]
  (assert (base/item? item) item)
  (assert (every? #{:state :set-state! :handle-action!} (keys options)))
  ;; Note: when processing a state change and an action, it is assumed
  ;; that the value passed to set-state! will eventually be the one
  ;; passed down as 'state'.
  (let [{state :state
         set-state! :set-state!
         handle-action! :handle-action!} options]
    (impl/react-run item
                    state
                    (or set-state! main/state-error)
                    (or handle-action! main/action-error))))

(let [h2 (fn [state return! f]
           (f (fn embed-item [item]
                (embed item {:state state
                             :set-state! (f/comp return! f/constantly)
                             :handle-action! (f/comp return! f/constantly (f/partial c/return :action))}))))
      h (fn [return! f]
          (c/dynamic h2 return! f))]
  (defn with-embed
    "Returns an item, that calls `f` with a function like [[embed]], which
     takes items and embeds them with the state of the returned
     item. Also, actions emitted by the embedded items are emitted
     from the returned item.

     This can be handy for adding items as children for lifted React container classes:

     ```clojure
     (defn my-container [& items]
       (with-embed
         (fn [embed]
           (apply reacl-c.interop.react/lift MyReactContainer #js {}
                  (map embed items))
      ```"
    [f]
    (c/with-async h f)))
