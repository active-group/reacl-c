(ns reacl-c.impl.reacl0
  (:require [reacl2.core :as reacl :include-macros true]))

(defn send-message! [target msg & [callback]]
  (reacl/send-message! target msg)
  ;; TODO: I think send-message should take a callback, ultimately invoked by React's setState
  (when callback (callback)))
