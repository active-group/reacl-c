(ns reacl-c.dom-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c.test-util.core-testing :as ct]
            [reacl-c.main-browser-test :as bt]
            [reacl-c.dom-testing :as dt]
            [schema.core :as s]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest item-equality-test
  ;; items should be referentially equal
  (testing "div"
    (is (= (dom/div) (dom/div)))
    (is (= (dom/div "a") (dom/div "a")))
    (is (= (dom/div {:onClick identity}) (dom/div {:onClick identity})))
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
                     (dom/div (dom/button {:onClick (:onClick attrs)
                                           :class "bar"})
                              (dom/button {:onClick (fn [st ev]
                                                      (assert (= (c/return :action (c/call-handler (:onX attrs) 42))
                                                                 (c/call (:onX attrs) 42)))
                                                      (c/return :action (c/call-handler (:onX attrs) 42)))
                                           :class "baz"}))))
    
    (let [[item last-state]
          (bt/capture-last-state-of (defn-dom-test-2 {:onClick (fn [st _] (inc st))
                                                      :onX (fn [st v] (+ st v))}))]
      (dt/rendering
       item
       :state 0
       (fn [env]
         (let [n (dt/container env)
               get-btn (fn [s]
                         (let [x (array-seq (.getElementsByClassName n s))]
                           (assert (not (empty? x)) s)
                           (first x)))]
           (is (some? n))
           (dt/fire-event (get-btn "bar") :click)
           (is (= 1 @last-state))

           (dt/fire-event (get-btn "baz") :click)
           (is (= 43 @last-state)))))))

  (testing "event handler binding in static"
    (dom/defn-dom defn-dom-test-3 :static [attrs]
      (dom/button {:onClick (fn [st ev]
                              (c/return :action (c/call-handler (:onClick attrs) st)))
                   :data-testid "bar"}))
    
    (let [[item last-state]
          (bt/capture-last-state-of (defn-dom-test-3 {:onClick (fn [st v] (conj st v))}))]
      (dt/rendering
       item
       :state []
       (fn [env]
         (dt/fire-event (.-firstChild (dt/container env)) :click)
         (is (not= [[]] @last-state))
         (is (= [nil] @last-state))))))

  (testing "event handler binding over two steps"
    (dom/defn-dom defn-dom-test-4 [attrs]
      (c/with-state-as [_ foo :local :foo]
        (dom/button attrs #_{:onClick (fn [st ev]
                                        (c/return :action (c/call-handler (:onClick attrs) ev)))})))
    (dom/defn-dom defn-dom-test-5 [attrs]
      (c/with-state-as [_ bar :local :bar]
        (defn-dom-test-4 attrs)))
    
    (let [[item last-state]
          (bt/capture-last-state-of (defn-dom-test-5 {:onClick (fn [st v] (conj st :done))}))]
      (dt/rendering
       item
       :state []
       (fn [env]
         (dt/fire-event (.-firstChild (dt/container env)) :click)
         (is (= [:done] @last-state))))))

  (testing "bound event handler sees state change"
    (dom/defn-dom defn-dom-test-6 [attrs]
      (c/local-state :local
                     (dom/button {:onClick (fn [st ev]
                                             (c/return :state [(conj (first st) :new-state) :new-local]
                                                       :action (c/call-handler (:onClick attrs) ev)))})))

    (dom/defn-dom defn-dom-test-7 [attrs]
      (defn-dom-test-6 attrs))

    (let [[item last-state]
          (bt/capture-last-state-of (defn-dom-test-7 {:onClick (fn [st v] (conj st :done))}))]
      (dt/rendering
       item
       :state []
       (fn [env]
         (dt/fire-event (.-firstChild (dt/container env)) :click)
         (is (= [:new-state :done] @last-state))))))
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

(deftest merge-attributes-test
  (testing "basics"
    (is (= (dom/merge-attributes) {}))
    (is (= (dom/merge-attributes nil) {}))
    (is (= (dom/merge-attributes nil nil) {}))

    (is (= (dom/merge-attributes {:a "a"} {:b "b"})
           {:a "a" :b "b"})))

  (testing "classes"
    (is (= (dom/merge-attributes {:class "a"} {:class "b"})
           {:class "a b"}))

    (is (= (dom/merge-attributes {:className "a"} {:class "b"})
           {:class "a b"}))
    (is (= (dom/merge-attributes {:class "a"} {:className "b"})
           {:class "a b"}))

    (is (= (dom/merge-attributes {:class "a"} {:className "b"})
           {:class "a b"})))

  (testing "styles"
    (is (= (dom/merge-attributes {:style {:width 10}} {:style {:height 20}})
           {:style {:width 10 :height 20}}))
    (is (= (dom/merge-attributes {:style {:width 10}} {:style {:width 20}})
           {:style {:width 20}}))
    )
  )
