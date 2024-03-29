(ns reacl-c.test-util.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.core :as tu]
            [reacl-c.main-browser-test :as bt]
            [active.clojure.functions :as f]
            [cljs.test :as t :refer (is deftest testing) :include-macros true]
            [cljs-async.core :as async]))

(deftest effect-utils-test
  (let [called-with (atom nil)
        called (atom false)]
    (c/defn-effect mk-eff0 [v]
      (reset! called v)
      (reset! called-with v)
      (c/return :state v :action ::act))

    (def eff1 (c/effect (fn []
                          (reset! called true)
                          21)))

    (is (tu/effect? eff1))
    (is (= nil (tu/effect-args eff1)))

    (is (tu/effect? (mk-eff0 42)))
    (is (tu/effect? (mk-eff0 42) mk-eff0))
    (is (= (mk-eff0 42) (mk-eff0 42)))
    (is (tu/effect-args (mk-eff0 42) [42]))

    (is (= [21 (c/return)] (tu/run-effect! eff1)))
    (is @called)

    (reset! called false)
    (is (= [42 (c/return :action ::act)] (tu/run-effect! (mk-eff0 42))))
      
    (is @called)
    (is (= 42 @called-with))))


(deftest subscription-utils-test
  (c/defn-subscription subscription-utils-test-1 deliver! [x]
    (fn [] nil))
  
  (let [sub-2-f (fn [deliver a1]
                  (fn [] nil))

        sub-2 (c/subscription sub-2-f :foo)
        sub-1 (subscription-utils-test-1 42)]

    (is (tu/subscription? sub-1))
    (is (tu/subscription? sub-2))

    (is (tu/subscription? sub-1 subscription-utils-test-1))
    (is (tu/subscription? sub-2 sub-2-f))

    (is (= sub-2-f (tu/subscription-f sub-2)))
    (is (= '(:foo) (tu/subscription-args sub-2)))

    ;; Note: subscription-utils-test-1 is a function creating a subscription, not the function implementing it.
    (is (= (tu/subscription-f (subscription-utils-test-1 :bla))
           (tu/subscription-f sub-1)))

    (is (= '(42) (tu/subscription-args sub-1)))))

(deftest map-subscriptions-test-1
  (let [sub-1-f (fn [& args]
                  (assert false "should not be called"))
        sub-1 (c/subscription sub-1-f)
        
        sub-2-f (fn [deliver! x]
                  (assert (= 42 x))
                  (deliver! ::result)
                  (fn [] nil))
        sub-2 (c/subscription sub-2-f 42)]
    (is (= (bt/emits-actions (-> (c/fragment sub-1)
                                 (tu/map-subscriptions (fn [sub]
                                                         (cond
                                                           (= sub-1 sub)
                                                           sub-2
                                                      
                                                           :else (assert false sub))))))
           [::result]))))

(deftest map-subscriptions-test-2
  (c/defn-subscription emulate-subscriptions-test-4-sub deliver! [x]
    (assert (= 42 x))
    (deliver! ::result)
    (fn [] nil))

  (let [sub-1 (c/subscription (fn [& args]
                                (assert false "should not be called")))]

    (is (= (bt/emits-actions (-> (c/fragment sub-1)
                                 (tu/map-subscriptions {sub-1 (emulate-subscriptions-test-4-sub 42)})))
           [::result])))

  (is (= (bt/emits-actions (-> (c/fragment (emulate-subscriptions-test-4-sub 21))
                               (tu/map-subscriptions {(emulate-subscriptions-test-4-sub 21) (emulate-subscriptions-test-4-sub 42)})))
         [::result])))

(deftest running-subscriptions-test
  ;; tests subscription-results and run-subsription! indirectly.
  (let [stopped? (atom false)
        sub-3 (c/subscription (fn [deliver!]
                                (deliver! :foo)
                                (js/setTimeout #(deliver! :bar) 2)
                                (js/setTimeout #(deliver! :baz) 4)
                                (fn []
                                  (reset! stopped? true))))]
    (t/async done
             (-> (tu/subscription-results sub-3
                                          3)
                 (async/then (fn [lst]
                               (is (= [:foo :bar :baz] lst))
                               (is @stopped?)))
                 (async/finally done)))))

(deftest running-subscriptions-timeout-test
  (let [stopped? (atom false)
        sub-3 (c/subscription (fn [deliver!]
                                (deliver! :foo)
                                (js/setTimeout #(deliver! :bar) 10)
                                (fn []
                                  (reset! stopped? true))))]
    (t/async done
             (-> (tu/subscription-results sub-3
                                          2
                                          1)
                 (async/then (fn [lst]
                               (is (= [:foo] lst))
                               (is @stopped?)))
                 (async/finally done)))))

