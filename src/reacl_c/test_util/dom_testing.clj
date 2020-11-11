(ns reacl-c.test-util.dom-testing)

(defmacro wait-for [expr & options]
  `(wait-for* (fn [] ~expr) ~@options))

(defmacro wait-for-removal [expr & options]
  `(wait-for-removal* (fn [] ~expr) ~@options))

