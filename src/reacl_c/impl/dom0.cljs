(ns ^:no-doc reacl-c.impl.dom0
  (:require [clojure.string :as str]
            #_["react-dom" :as react-dom]))

(defn capture-event? [k]
  (let [s (name k)]
    (and (str/ends-with? s "apture")
         (or (str/ends-with? s "Capture")
             (str/ends-with? s "capture")))))

