(ns ^:no-doc reacl-c.impl.dom0
  (:require [clojure.string :as str]
            #_["react-dom" :as react-dom]))

(defn capture-event? [k]
  (let [s (name k)]
    (and (str/ends-with? s "apture")
         (or (str/ends-with? s "Capture")
             (str/ends-with? s "capture")))))

(defn- react-handler-name [ev-type]
  ;; Usually "click" -> "onClick", but unfortunately, there are some exceptions to it:
  ;; TODO: hook up with https://github.com/facebook/react/blob/master/packages/react-dom/src/events/DOMEventProperties.js for all of them
  ;; not exported though :-( (js/console.log react-dom/events.-DOMEventProperties.-topLevelEventsToReactNames)
  (case ev-type
    "dblclick" "ondoubleclick"
    (str "on" ev-type)))

(defn find-event-handler [events capture? ev]
  ;; OPT: could be done a little faster - try a lookup, or prepare a different map. But it can be 'onchange' and 'onChange'.
  (when-not (.-type ev)
    (js/console.error ev)
    (throw (ex-info "Expected a JavaScript event object, with a property 'type'." {:value ev})))
  (let [en (cond-> (react-handler-name (.-type ev))
             capture? (str "capture"))]
    (some (fn [[n f]]
            (and (= en (str/lower-case (name n)))
                 f))
          events)))
