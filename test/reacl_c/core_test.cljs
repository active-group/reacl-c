(ns reacld.test.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
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
      (is (= (c/dynamic f) (c/dynamic f)))))
  (testing "focus"
    (is (= (c/focus (dom/div) :a) (c/focus (dom/div) :a))))
  (testing "handle-action"
    (let [f (fn [st a])]
      (is (= (c/handle-action (dom/div) f) (c/handle-action (dom/div) f)))))
  (testing "interactive"
    (let [f (fn [a b _] (dom/div a b))]
      (is (= (c/interactive :id f :a) (c/interactive :id f :a)))))
  (testing "add-state"
    (is (= (c/add-state :a :b (dom/div)) (c/add-state :a :b (dom/div)))))
  (testing "keyed"
    (is (= (c/keyed (dom/div) :a) (c/keyed (dom/div) :a))))
  (testing "did-mount"
    (is (= (c/did-mount (dom/div) :a) (c/did-mount (dom/div) :a))))
  (testing "will-unmount"
    (is (= (c/will-unmount (dom/div) :a :b) (c/will-unmount (dom/div) :a :b))))
  (testing "did-update"
    (is (= (c/did-update (dom/div) :a :b) (c/did-update (dom/div) :a :b))))
  (testing "while-mounted"
    (is (= (c/while-mounted (dom/div) :a :b) (c/while-mounted (dom/div) :a :b))))
  (testing "with-async-actions"
    (is (= (c/with-async-actions :f :a) (c/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (c/monitor-state (dom/div) :f :a) (c/monitor-state (dom/div) :f :a))))
  )
