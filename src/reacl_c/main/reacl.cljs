(ns reacl-c.main.reacl
  "Functions for using reacl-c within a Reacl application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.main.react :as main-react]
            [reacl-c.core :as core]
            [active.clojure.functions :as f]
            [reacl2.core :as reacl :include-macros true]))

(let [set-state (fn [this state callback]
                  (reacl/send-message! this [::set-state state] callback))
      handle-action (fn [this action callback]
                      (reacl/send-message! this [::handle-action action] callback))]
  (reacl/defclass ^:private runner this state [item]
    refs [self]
    
    render
    (-> (main-react/react-controlled item state (f/partial set-state this) (f/partial handle-action this))
        (reacl/refer self))

    handle-message
    (fn [msg]
      (cond
        (and (vector? msg) (= ::set-state (first msg)))
        (reacl/return :app-state (second msg))

        (and (vector? msg) (= ::handle-action (first msg)))
        (reacl/return :action (second msg))

        :else
        ;; Note: because this outer 'reacl' is indepandant from the
        ;; 'inner reacl' that runs the item (if using the Reacl
        ;; implementation), we cannot pass messages down via
        ;; 'return :message' here:
        #_(reacl/return :message [(reacl/resolve-component (reacl/get-dom self)) msg])
        (let [comp (reacl/get-dom self)]
          (assert comp msg)
          (main/send-message! comp msg) ;; TODO: callback?
          (reacl/return))))))

;; previously named reacl-render
(defn reacl
  "Returns a Reacl component running the given item with the given
  Reacl `state binding`.

  Messages sent to the returned component are passed to the given
  item. Actions emitted from the item are emitted from the returned
  component. Note that this includes effect actions. If you want
  effects to be executed instead, use [[reacl-c.main.execute-effects]]."
  [binding item]
  (runner binding item))
