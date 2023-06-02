(ns ^:no-doc reacl-c.impl.dom0
  (:require [clojure.string :as str]))

(defn custom-type? [node-type]
  ;; Conforms to the HTML standard - custom element must have a '-' in their name.
  (and (string? node-type) (str/includes? node-type "-")))

