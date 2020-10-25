(ns ^:no-doc reacl-c.impl.dom0
  (:require [clojure.string :as str]))

(defn capture-event? [k]
  (let [s (name k)]
    (and (str/ends-with? s "apture")
         (or (str/ends-with? s "Capture")
             (str/ends-with? s "capture")))))

(defn find-event-handler [events capture? ev]
  ;; OPT: could be done a little faster - try a lookup, or prepare a different map. But it can be 'onchange' and 'onChange'.
  (when-not (.-type ev)
    (js/console.error ev)
    (throw (ex-info "Expected a JavaScript event object, with a property 'type'." {:value ev})))
  (let [en (cond-> (str "on" (str/lower-case (.-type ev)))
             capture? (str "capture"))]
    (some (fn [[n f]]
            (and (= en (str/lower-case (name n)))
                 f))
          events)))
