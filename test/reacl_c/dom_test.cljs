(ns reacl-c.dom-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c.test-util.core-testing :as ct]
            [reacl-c.base :as base]
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
  (dom/defn-dom defn-dom-test-1 :- s/Str "bar" [a b]
    (dom/div a b))

  (is (= "bar" (:doc (meta #'defn-dom-test-1))))

  (testing "optional attrs map"
    (is (ct/contains? (defn-dom-test-1 {} "baz") (dom/div "baz")))

    (is (ct/contains? (defn-dom-test-1 "baz") (dom/div "baz"))))

  (testing "event handler binding"
    (dom/defn-dom defn-dom-test-2 [attrs]
      (c/local-state "foo"
                     (c/fragment (dom/button {:onclick (:onclick attrs)
                                              :data-testid "bar"})
                                 (dom/button {:onclick (fn [st ev]
                                                         (c/call (:onx attrs) 42))
                                              :data-testid "baz"}))))
    
    (dt/rendering
     (defn-dom-test-2 {:onclick (fn [st _] (inc st))
                       :onx (fn [st v] (+ st v))})
     :state 0
     (fn [env]
       (dt/fire-event (dt/get env (dt/by-test-id "bar")) :click)
       (is (= (dt/current-state env)
              1))
       (dt/fire-event (dt/get env (dt/by-test-id "baz")) :click)
       (is (= (dt/current-state env)
              43)))))

  (testing "event handler binding in static"
    (dom/defn-dom defn-dom-test-3 :static [attrs]
      (dom/button {:onclick (fn [st ev]
                              (c/call (:onclick attrs) st))
                   :data-testid "bar"}))
    
    (dt/rendering
     (defn-dom-test-3 {:onclick (fn [st v] (conj st v))})
     :state []
     (fn [env]
       (dt/fire-event (dt/get env (dt/by-test-id "bar")) :click)
       (is (not= (dt/current-state env)
                 [[]]))
       (is (= (dt/current-state env)
              [nil])))))
  )

(deftest def-dom-test
  (dom/def-dom foo dom/div)

  (is (ct/contains? (foo "bar")
                    (dom/div "bar")))

  (dom/def-dom bar dom/div
    {:style {:margin "10px"}})

  (is (ct/contains? (bar (dom/span))
                    (dom/div {:style {:margin "10px"}} (dom/span))))
  )
