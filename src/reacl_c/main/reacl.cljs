(ns reacl-c.main.reacl
  "Functions for using Reacl-c within a Reacl application or library."
  (:require [reacl-c.main.react :as main-react]
            [reacl-c.core :as core]
            [active.clojure.functions :as f]
            [reacl2.core :as reacl :include-macros true]))

(defn- send-message! [target msg & [callback]]
  (reacl/send-message! target msg)
  ;; TODO: I think send-message should take a callback, ultimately invoked by React's setState
  (when callback (callback)))

(let [set-state (fn [this state callback]
                  (send-message! this [::set-state state] callback))
      handle-action (fn [this action callback]
                      (send-message! this [::handle-action action] callback))]
  (reacl/defclass ^:private runner this state [item]
    refs [self]
    
    render
    (main-react/embed item
                      {:state state
                       :ref self
                       :set-state! (f/partial set-state this)
                       :handle-action! (f/partial handle-action this)})

    handle-message
    (fn [msg]
      (cond
        (and (vector? msg) (= ::set-state (first msg)))
        (reacl/return :app-state (second msg))

        (and (vector? msg) (= ::handle-action (first msg)))
        (reacl/return :action (second msg))

        :else
        (let [comp (reacl/get-dom self)]
          (main-react/send-message! comp msg) ;; TODO: callback?
          (reacl/return))))))

(defn embed
  "Returns a Reacl component embedding the given item with the given
  Reacl `state binding`.

  Messages sent to the returned component are passed to the given
  item. Actions and effects emitted from the item are emitted from the
  returned component. To have effects being executed implicitly,
  use [[reacl-c.main/execute-effects]]."
  ([item]
   (runner (reacl/use-app-state nil) item))
  ([binding item]
   (runner binding item)))
