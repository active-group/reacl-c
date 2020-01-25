(ns reacl-c.test-util.core
  (:require [reacl2.test-util.beta :as r-tu]))

(defmacro provided [bindings & body]
  `(r-tu/provided ~bindings ~@body))
