(ns reacl-c.dom-testing
  "Reduced and simplified version of reacl-c.test-util.dom-testing"
  (:require [reacl-c.main :as main]
            ["react-dom/test-utils" :as react-tu]
            [cljs-async.core :as async :include-macros true]))

(defrecord ^:private Env [app node])

(defn rendering [item & args]
  (let [f (last args)
        options (apply hash-map (drop-last args))]
    (let [e (or (:container options) (js/document.createElement "div"))
          app (main/run e item {:initial-state (:state options)
                                :handle-action! (:handle-action! options)})]
      #_(react-dom/flushSync (fn [] nil)) ;; react > 17
      (async/try-finally (fn []
                           (f (Env. app e)))
                         (fn []
                           (main/stop! app))))))

(defn container [env]
  (:node env))

(defn app [env]
  (:app env))

(defn send-message!
  "Sends a message to the item running in the given rendering environment."
  [env msg]
  (main/send-message! (app env) msg))

(defn fire-event
  [node event & [event-properties]]
  (assert (= :click event) "only clicks for now")
  (if event-properties
    (react-tu/Simulate.click node (js/Event. "click" (clj->js event-properties)))
    (react-tu/Simulate.click node)))
