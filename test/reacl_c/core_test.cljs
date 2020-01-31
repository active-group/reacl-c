(ns reacl-c.test.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
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
    (is (= (c/focus :a (dom/div)) (c/focus :a (dom/div)))))
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
  (testing "with-async-actions"
    (is (= (c/with-async-actions :f :a) (c/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (c/capture-state-change (dom/div) :f) (c/capture-state-change (dom/div) :f))))
  )

(deftest while-mounted-test
  (testing "mount, unmount"
    (let [mounted (atom false)
          states (atom [])
          env (tu/env (c/while-mounted (c/fragment)
                                       #(do (reset! mounted true) :mounted)
                                       identity
                                       #(do (reset! mounted false) (swap! states conj %))))]
      (tu/mount! env :state1)
      (is @mounted)
      
      (tu/unmount! env)
      (is (not @mounted))
      (is (= [:mounted] @states))))

  (testing "process over updates"
    (let [mounted (atom false)
          states (atom [])
          upd (atom 0)
          env (tu/env (c/while-mounted (c/dynamic pr-str)
                                       #(do (reset! mounted true) :mounted)
                                       #(do (swap! states conj %) (swap! upd inc) [:updated @upd])
                                       #(do (reset! mounted false) (swap! states conj %))))]
      (tu/mount! env :state1)
      (is @mounted)

      (tu/update! env :state2)
      (is (= [:mounted] @states))

      (tu/update! env :state3)
      (is (= [:mounted [:updated 1]] @states))
      
      (tu/unmount! env)
      (is (not @mounted))
      (is (= [:mounted [:updated 1] [:updated 2]] @states)))))

(deftest defn-dynamic-test
  (testing "a regression with varargs"
    (c/defn-dynamic dyn1 state [& args]
      (dom/div))

    (is (= (dyn1) (dyn1)))

    (is (= (dyn1 "x") (dyn1 "x")))

    (c/defn-dynamic dyn2 state [a1 & args]
      (dom/div))

    (is (= (dyn1 "x") (dyn1 "x")))

    (is (= (dyn1 "x" "y") (dyn1 "x" "y")))
    ))

;; TODO: test every higher level feature in core.
