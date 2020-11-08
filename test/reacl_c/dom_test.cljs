(ns reacl-c.dom-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest item-equality-test
  ;; items should be referentially equal
  (testing "div"
    (is (= (dom/div) (dom/div)))
    (is (= (dom/div "a") (dom/div "a")))
    (is (= (dom/div {:onclick identity}) (dom/div {:onclick identity})))
    (is (= (dom/div (dom/div "a")) (dom/div (dom/div "a"))))))


(deftest generic-fn-test
  (is (= (dom/h "div" {:width 10})
         (dom/div {:width 10}))))
