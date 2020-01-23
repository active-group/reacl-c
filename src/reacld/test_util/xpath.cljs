(ns reacld.test-util.xpath
  (:require [reacl2.test-util.xpath :as rp]
            [reacld.impl.reacl :as impl])
  (:refer-clojure :exclude [and or contains? nth nth-last comp first last range]))

(defn named [s]
  (rp/or (rp/tag s)
         (rp/class (impl/named s))))

(defn named-var [v]
  (named (:reacld.core/name (meta v))))

(def attr rp/attr)

(def and rp/and)
(def or rp/or)
(def range rp/range)
(def nth rp/nth)
(def nth-last rp/nth-last)
(def first rp/first)
(def last rp/last)
(def root rp/root)
(def text rp/text)
(def state rp/app-state)
(def all rp/all)
(def self rp/self)
(def parent rp/parent)
(def children rp/children)

(def where rp/where)
(def is? rp/is?)
(def is= rp/is=)
(def id= rp/id=)
(def css-class? rp/css-class?)
(def style? rp/style?)

(def select-all rp/select-all)
(def select rp/select)
(def select-one rp/select-one)
(def contains? rp/contains?)

(defn comp
  "Compose the given xpath selector forms to a combined
  selector, where from left to right, the selectors restrict the filter
  further. \n
  Valid selectors are all the primitives from this module,
  as well as:\n

  - strings stand for a virtual dom node or elements named by [[reacld.core.name]] as with [[named]],
  - vars generated from one of the 'def-' and 'defn-' macros stand for those nodes created by them, as with [[named-var]]. Note: use #'name to get the vars instead of the functions.
  - keywords stand for attribute names of dom nodes as with [[attr]]\n

  Also see [[>>]] for a convenience macro version of this."

  [& selectors]
  (apply rp/comp (map (fn [v]
                        (cond
                          (string? v) (named v)
                          (var? v) (named-var v)
                          (keyword? v) (attr v)
                          :else v))
                      selectors)))
