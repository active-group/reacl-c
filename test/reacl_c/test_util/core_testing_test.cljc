(ns reacl-c.test-util.core-testing-test
  (:require [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core-testing :as tu]
            #?(:cljs [cljs.test :refer (deftest is testing) :include-macros true])
            #?(:clj [clojure.test :refer (deftest is testing)])))

(deftest resolve-test
  (= (dom/div "42")
     (tu/resolve (dom/div "42")
                 nil))
  (= (dom/div (dom/span "foo") (c/fragment (dom/h1 "bar")))
     (tu/resolve (dom/div (dom/span "foo") (c/fragment (dom/h1 "bar")))
                 nil))

  (= (dom/div "42")
     (tu/resolve (c/dynamic (fn [v] (dom/div v)))
                 "42"))

  (= (dom/div "42" "?")
     (tu/resolve (c/local-state "42" (c/dynamic (fn [[a b]] (dom/div a b))))
                 "?"))

  (= (dom/div "42")
     (tu/resolve (c/with-ref (fn [_] (dom/div "42")))
                 nil))

  ;; TODO: cover all cases / use property test?
  )

(deftest init-finalize-test
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

(deftest message-test
  (is (= (c/return :state 42)
         (tu/message (c/handle-message (fn [st msg]
                                         (c/return :state (+ st (:msg msg))))
                                       (dom/div))
                     20
                     {:msg 22})))

  (is (= (c/return :state 42)
         (tu/message (c/local-state :foo
                                    (c/handle-message (fn [[st x] msg]
                                                        (c/return :state [(+ st (:msg msg)) x]))
                                                      (dom/div)))
                     20
                     {:msg 22}))))


