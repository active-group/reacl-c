(ns reacl-c.test-util.dom-testing)

(defmacro wait-for
  "Returns a promise that evaluates `expr` repeatedly until it does not throw or a timeout elapses.
  Options can be:

  - `:timeout` the maximum number of milliseconds to wait,
  - `:interval` the number of milliseconds to wait between to evaluationg of `expr`.
  
  For more options see https://testing-library.com/docs/dom-testing-library/api-async#waitfor.
  "
  [expr & options]
  `(wait-for* (fn [] ~expr) ~@options))

(defmacro wait-for-removal
  "Returns a promise that evaluates `expr` repeatedly until it throws or a timeout elapses.
  Options can be:

  - `:timeout` the maximum number of milliseconds to wait,
  - `:interval` the number of milliseconds to wait between to evaluationg of `expr`.
  
  For more options see https://testing-library.com/docs/dom-testing-library/api-async#waitfor."
  [expr & options]
  `(wait-for-removal* (fn [] ~expr) ~@options))

