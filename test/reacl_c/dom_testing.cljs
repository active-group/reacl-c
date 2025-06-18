(ns reacl-c.dom-testing
  "Reduced and simplified version of reacl-c.test-util.dom-testing"
  (:require [reacl-c.main :as main]
            #_["react-dom/test-utils" :as react-tu]
            #_["@testing-library/react" :as react-tu]
            [cljs-async.core :as async :include-macros true]))

(defrecord ^:private Env [app node])

(defn flush-sync! [thunk]
  (main/flush-sync! thunk))

(defn rendering [item & args]
  (let [f (last args)
        options (apply hash-map (drop-last args))]
    (let [e (or (:container options) (js/document.createElement "div"))
          new-container? (not (:container options))]
      (when new-container?
        ;; Note: especially native dispatchEvents don't work if not mounted to the document. (react-tu does work around that)
        (.appendChild js/document.body e))
      (let [app (flush-sync! (fn []
                               (main/run e item {:initial-state (:state options)
                                                 :handle-action! (:handle-action! options)})))]
        (async/try-finally (fn []
                             (f (Env. app e)))
                           (fn []
                             (main/stop! app)
                             (when new-container?
                               (.removeChild js/document.body e))))))))

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
  (let [default-props {:bubbles true
                       :cancelable false}]
    (flush-sync!
     (fn []
       (.dispatchEvent node (new js/PointerEvent "click" (clj->js (merge default-props event-properties))))))))


#_(defn fire-event [node event & [event-properties]]
  (assert (= :click event) "only clicks for now")
  (if event-properties
    (react-tu/Simulate.click node (js/Event. "click" (clj->js event-properties)))
    (react-tu/Simulate.click node)))

#_(defn- fire-event* [node event]
  (react-tu/fireEvent node event))

#_(defn fire-event
  "Fire an event on the given DOM `node`, where `event` can be a DOM
  Event object, or a keywork like `:click` for standard events. See
  https://github.com/testing-library/dom-testing-library/blob/master/src/event-map.js
  for a list."
  [node event & [event-properties]]
  (fire-event* node
               (if (instance? js/Event event)
                 (do (assert (nil? event-properties) "Additional event properties can not be set on an event object argument.")
                     event)
                 ;; Note: createElemnt.click() uses default properties, but createEvent("click") doesn't :-/ damn!
                 ;; How can we distinguish it? Let's use: keyword event => default; string event => generic.
                 (let [f (if (string? event)
                           (partial react-tu/createEvent event)
                           (or (aget react-tu/createEvent (name event))
                               (throw (js/Error (str "Not a known standard event: " (pr-str event) ".")))))]
                   (f node (clj->js event-properties))))))
