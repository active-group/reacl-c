(ns reacl-c.dom-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [schema.core :as s]
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

(deftest defn-dom-test
  (dom/defn-dom defn-dom-test-1 :- s/Str "bar" [a b] (dom/div a b))

  (is (= "bar" (:doc (meta #'defn-dom-test-1))))

  (testing "optional attrs map"
    (is (= (dom/div "baz") (:e (defn-dom-test-1 {} "baz")))) ;; :e = base/named-e

    (is (= (dom/div "baz") (:e (defn-dom-test-1 "baz")))))

  ;; TODO: test event handler binding?
  )
