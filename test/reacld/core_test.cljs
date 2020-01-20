(ns reacld.test.core-test
  (:require [reacld.core :as r :include-macros true]
            [reacld.dom :as dom]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest element-equality-test
  ;; ell element should be referentially equal
  (testing "div"
    (is (= (dom/div) (dom/div)))
    (is (= (dom/div "a") (dom/div "a")))
    (is (= (dom/div {:onclick identity}) (dom/div {:onclick identity})))
    (is (= (dom/div (dom/div "a")) (dom/div (dom/div "a")))))
  (testing "dynamic"
    (let [f (fn [x] (dom/div x))]
      (is (= (r/dynamic f) (r/dynamic f)))))
  (testing "focus"
    (is (= (r/focus (dom/div) :a) (r/focus (dom/div) :a))))
  (testing "handle-actions"
    (let [f (fn [a])]
      (is (= (r/handle-actions (dom/div) f) (r/handle-actions (dom/div) f)))))
  (testing "map-actions"
    (let [f (fn [a] a)]
      (is (= (r/map-actions (dom/div) f) (r/map-actions (dom/div) f)))))
  (testing "interactive"
    (let [f (fn [a b _] (dom/div a b))]
      (is (= (r/interactive f :a) (r/interactive f :a)))))
  (testing "add-state"
    (is (= (r/add-state :a :b (dom/div)) (r/add-state :a :b (dom/div)))))
  (testing "keyed"
    (is (= (r/keyed (dom/div) :a) (r/keyed (dom/div) :a))))
  (testing "when-mounted"
    (is (= (r/when-mounted (dom/div) :a) (r/when-mounted (dom/div) :a))))
  (testing "when-unmounting"
    (is (= (r/when-unmounting (dom/div) :a :b) (r/when-unmounting (dom/div) :a :b))))
  (testing "after-update"
    (is (= (r/after-update (dom/div) :a :b) (r/after-update (dom/div) :a :b))))
  (testing "while-mounted"
    (is (= (r/while-mounted (dom/div) :a :b) (r/while-mounted (dom/div) :a :b))))
  (testing "with-async-actions"
    (is (= (r/with-async-actions :f :a) (r/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (r/monitor-state (dom/div) :f :a) (r/monitor-state (dom/div) :f :a))))
  )
