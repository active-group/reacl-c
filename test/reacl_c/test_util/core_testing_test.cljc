(ns reacl-c.test-util.core-testing-test
  (:require [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core-testing :as tu]
            #?(:cljs [cljs.test :refer (deftest is testing) :include-macros true])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(deftest render-test
  (is (= [(dom/div "42")]
         (tu/render (dom/div "42")
                    nil)))
  (is (= [(dom/div (dom/span "foo") (dom/h1 "bar"))]
         (tu/render (dom/div (dom/span "foo") (c/fragment (dom/h1 "bar")))
                    nil)))

  (is (= [(dom/div "42")]
         (tu/render (c/dynamic (fn [v] (dom/div v)))
                    "42")))

  (is (= [(dom/div "?" "42")]
         (tu/render (c/local-state "42" (c/dynamic (fn [[outer inner]] (dom/div outer inner))))
                    "?")))

  (is (= [(dom/div "42")]
         (tu/render (c/with-ref (fn [r]
                                  (c/fragment (c/init (c/return :message [r :foo]))
                                              (dom/div "42"))))
                    nil)))
  ;; TODO: cover all cases / use property test?
  )

(deftest init-finalize-test
  (is (= (c/return)
         (tu/init (c/fragment) :foo)))
  
  (is (= (c/return :state 42 :action :foo)
         (tu/init (c/lifecycle (fn [st] (c/return :state (inc st)
                                                  :action :foo))
                               (constantly (c/return)))
                  41)))
  
  (is (= (c/return :state 42)
         (tu/init (c/init (c/return :state 42))
                  nil)))

  (is (= (c/return :state 42
                   :action :foo)
         (tu/init (c/fragment (c/init (c/return :action :foo))
                              (c/once (fn [st]
                                        (c/return :state (inc st)))))
                  41)))

  (is (= (c/return :state 42)
         (tu/init (c/handle-state-change (c/init (c/return :state 22))
                                         (fn [st1 st2]
                                           (+ st1 st2)))
                  20)))


  (is (= (c/return)
         (tu/finalize (c/init (c/return :state 42))
                      nil)))
  (is (= (c/return :state 42)
         (tu/finalize (c/finalize (c/return :state 42))
                      nil))))

(deftest handle-message-test
  (is (= (c/return :state 42)
         (tu/handle-message (c/handle-message (fn [st msg]
                                                (c/return :state (+ st (:msg msg))))
                                              (dom/div))
                            20
                            {:msg 22})))

  (is (= (c/return :state 42)
         (tu/handle-message (c/local-state :foo
                                           (c/handle-message (fn [[st x] msg]
                                                               (c/return :state [(+ st (:msg msg)) x]))
                                                             (dom/div)))
                            20
                            {:msg 22}))))


(deftest contains-like?-test
  (testing "basics"
    (is (tu/contains-like? (dom/div "foo") "foo"))
    (is (tu/contains-like? (dom/div "foo") "fo"))
    (is (tu/contains-like? (dom/div "foo") (dom/div)))
    (is (tu/contains-like? (dom/div {:width "100px" :height "100px"}) (dom/div {:width "100px"})))

    (is (tu/contains-like? (c/dynamic (fn [st] (dom/div st)))
                           (dom/div "foo")
                           :state "foo"))
    (is (not (tu/contains-like? (c/dynamic (fn [st] (dom/div st)))
                                (dom/div "foo")
                                :state "bar")))
    (let [f (fn [st] (dom/div st))]
      (is (tu/contains-like? (dom/div (c/dynamic f))
                             (c/dynamic f)
                             :state "foo"))))

  (testing "sub-item-state"
    (is (tu/contains-like? (c/focus :a (dom/div "bla")) "bla"
                           :state {:a "foo"}
                           :sub-item-state "foo"))
    (is (not (tu/contains-like? (c/focus :a (dom/div "bla")) "bla"
                                :state  {:a "foo"}
                                :sub-item-state "blubb"))))

  (testing "special dom attributes"
    (is (tu/contains-like? (dom/div {:style {:width "100px" :height "50px"}})
                           (dom/div {:style {:width "100px"}})))

    (is (tu/contains-like? (dom/div {:class "foo bar"})
                           (dom/div {:class "foo"})))))

(deftest contains?-test
  (is (tu/contains? (dom/div "foo") "foo"))
  
  (is (tu/contains? (c/dynamic (fn [st] (dom/div st)))
                    (dom/div "foo")
                    :state "foo"))
  (is (tu/contains? (c/focus :a (dom/div "bla")) "bla"
                    :state {:a "foo"}
                    :sub-item-state "foo")))
