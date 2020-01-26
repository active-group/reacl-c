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
    (let [f (fn [a])]
      (is (= (c/handle-action (dom/div) f) (c/handle-action (dom/div) f)))))
  (testing "add-state"
    (is (= (c/add-state :a :b (dom/div)) (c/add-state :a :b (dom/div)))))
  (testing "keyed"
    (is (= (c/keyed (dom/div) :a) (c/keyed (dom/div) :a))))
  (testing "did-mount"
    (is (= (c/did-mount (c/return :action :a)) (c/did-mount (c/return :action :a)))))
  (testing "will-unmount"
    (is (= (c/will-unmount (c/return :action :a)) (c/will-unmount (c/return :action :a)))))
  (testing "did-update"
    (is (= (c/did-update (dom/div) :a) (c/did-update (dom/div) :a))))
  (testing "while-mounted"
    (is (= (c/while-mounted :a :b) (c/while-mounted :a :b))))
  (testing "with-async-actions"
    (is (= (c/with-async-actions :f :a) (c/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (c/monitor-state (dom/div) :f) (c/monitor-state (dom/div) :f))))
  )

;; TODO: test every higher level feature in core.
