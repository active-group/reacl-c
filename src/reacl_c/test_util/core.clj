(ns reacl-c.test-util.core
  (:require [reacl2.test-util.beta :as r-tu]))

(defmacro provided
  "Replaces the values bound to the given vars during the
  evaluation of `body`, and sets them back to the previous values
  afterwards. Example:

  ```
  (def x 42)
  (provided [x 11]
    (is (= (* x 2) 22)))
  (is (= x 42))
  ```
  "
  [bindings & body]
  `(r-tu/provided ~bindings ~@body))
