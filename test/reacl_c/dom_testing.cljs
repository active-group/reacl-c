(ns reacl-c.dom-testing
  "Reduced and simplified version of reacl-c.test-util.dom-testing"
  (:require [reacl-c.main :as main]
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
