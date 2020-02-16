(ns reacl-c.test.test-util.xpath-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.core :as tu]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
            [cljs.test :refer (is deftest testing) :include-macros true]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :include-macros true :refer [defspec]]))

(defspec item-xpath-self-test 200
  (let [same (fn [pat item]
               (let [env (tu/env item)]
                 (tu/mount! env nil)
                 (some? (xpath/select (tu/get-component env)
                                      (xpath/item pat)))))]
    (prop/for-all [v item-gen/node-item]
                  (same v v))))

;; TODO: test named.
