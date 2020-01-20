(ns reacld.test.core-test
  (:require [reacld.core :as r :include-macros true]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest element-equality-test
  ;; ell element should be referentially equal
  (testing "div"
    (is (= (r/div) (r/div)))
    (is (= (r/div "a") (r/div "a")))
    (is (= (r/div {:onclick identity}) (r/div {:onclick identity})))
    (is (= (r/div (r/div "a")) (r/div (r/div "a")))))
  (testing "dynamic"
    (let [f (fn [x] (r/div x))]
      (is (= (r/dynamic f) (r/dynamic f)))))
  (testing "focus"
    (is (= (r/focus (r/div) :a) (r/focus (r/div) :a))))
  (testing "handle-actions"
    (let [f (fn [a])]
      (is (= (r/handle-actions (r/div) f) (r/handle-actions (r/div) f)))))
  (testing "map-actions"
    (let [f (fn [a] a)]
      (is (= (r/map-actions (r/div) f) (r/map-actions (r/div) f)))))
  (testing "interactive"
    (let [f (fn [a b _] (r/div a b))]
      (is (= (r/interactive f :a) (r/interactive f :a)))))
  (testing "add-state"
    (is (= (r/add-state :a :b (r/div)) (r/add-state :a :b (r/div)))))
  (testing "keyed"
    (is (= (r/keyed (r/div) :a) (r/keyed (r/div) :a))))
  (testing "when-mounted"
    (is (= (r/when-mounted (r/div) :a) (r/when-mounted (r/div) :a))))
  (testing "when-unmounting"
    (is (= (r/when-unmounting (r/div) :a :b) (r/when-unmounting (r/div) :a :b))))
  (testing "after-update"
    (is (= (r/after-update (r/div) :a :b) (r/after-update (r/div) :a :b))))
  (testing "while-mounted"
    (is (= (r/while-mounted (r/div) :a :b) (r/while-mounted (r/div) :a :b))))
  (testing "with-async-actions"
    (is (= (r/with-async-actions :f :a) (r/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (r/monitor-state (r/div) :f :a) (r/monitor-state (r/div) :f :a))))
  )
