(ns reacl-c.test-util.xpath
  (:require [reacl2.test-util.xpath :as rp]
            [reacl-c.core :as c]
            [reacl-c.base :as base]
            [reacl-c.impl.reacl :as impl]))

(defn named [n]
  (cond
    (string? n)
    ;; a dom tag might always be one element under the corresponding
    ;; dom wrapper class, but should appear on the 'same level' for
    ;; the user.
    (rp/or (rp/tag n)
           (rp/comp (rp/type (impl/dom-class-for-type n)) rp/children (rp/tag n)))

    
    (some? (c/meta-name-id n)) (rp/class (impl/named (c/meta-name-id n)))
    (base/name-id? n) (rp/class (impl/named n))
    :else (assert false n)))

(defn item
  "Matches an item similar to the given one. The given item is
  intepreted as a pattern, and may contain 'less' attributes or
  children."
  [v]
  (impl/xpath-pattern v))
