(ns reacl-c.test-util.perf-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.perf :as perf :include-macros true]
            [active.clojure.functions :as f]
            [cljs.test :refer (is deftest testing) :include-macros true]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :include-macros true :refer [defspec]]))

(deftest resolve-deep-test
  (is (= (dom/div) (perf/resolve-deep (dom/div) nil)))
  (is (= (dom/div "foo") (perf/resolve-deep (c/dynamic #(dom/div %)) "foo")))
  (let [x (c/name-id "x")]
    (is (= (c/named x (dom/div "foo")) (perf/resolve-deep (c/named x (c/dynamic #(dom/div %))) "foo"))))
  (is (= (c/focus :x (dom/div "foo")) (perf/resolve-deep (c/focus :x (c/dynamic #(dom/div %))) {:x "foo"})))
  (is (= (c/fragment (dom/div "foo")) (perf/resolve-deep (c/fragment (c/dynamic #(dom/div %))) "foo")))
  (is (= (dom/span (dom/div "foo")) (perf/resolve-deep (dom/span (c/dynamic #(dom/div %))) "foo")))
  (is (= (c/handle-action (dom/div "foo") :f) (perf/resolve-deep (c/handle-action (c/dynamic #(dom/div %)) :f) "foo")))
  ;; ...?
  )

(deftest performance-test
  (is (= [:good [{:state-1 :a
                  :state-2 :b
                  :resolved (dom/div)}]]
         (perf/performance (c/dynamic (f/partial #(dom/div))) (list :a :b))))
  (is (= [:ideal nil]
         (perf/performance (c/dynamic (f/partial #(dom/div (str %)))) (list :a :b))))

  (is (= [:ideal nil]
         (perf/performance (c/fragment (c/focus :x (c/dynamic #(dom/div %)))
                                       (c/focus :a (c/dynamic #(dom/div %))))
                           (list {:a "1" :x "y"} {:a "2" :x "y"}))))

  (let [f1 (fn [state] (dom/div)) 
        f2 (fn [state] (dom/div))
        fst (atom true)]
    (is (= [:bad [{:state :a
                   :different-at [['dynamic] {:function [f1 f2]}]}]]
           ;; Note: we 'simulate' a dynamic with a (fn) in the body, in order to make a = check against them.
           ;; The order in the result might depend on how often the impl resolves the dynamic. Not an error if the other way round.
           (perf/performance (c/dynamic (fn [state]
                                          (c/dynamic (if @fst
                                                       (do (reset! fst false) f1)
                                                       (do (reset! fst true) f2)))))
                             (list :a)))))

  (is (= [:good [{:state-1 :a
                  :state-2 :b
                  :resolved (dom/div)}]]
         (perf/performance (c/dynamic (f/partial #(dom/div))) (list :a :b)))))

#_(deftest performance-macro-test
  (let [f #(dom/div)]
    (is (perf/performance= :ideal (c/dynamic f) (list [42 {:k :a}] [42 {:k :b}]))))
  
  (let [f #(c/handle-message :f (dom/div {:onclick (fn [_] nil)} (str %)))]
    (is (reacl-c.test-util.perf/performance= :good (c/dynamic f) (list :a :b)))))

(deftest find-first-difference-test
  (let [f perf/find-first-difference]

    (is (= nil (f (c/fragment) (c/fragment))))
    (is (= [['fragment 0] {:tag ["div" "span"]}] (f (c/fragment (dom/div)) (c/fragment (dom/span)))))
    (is (= [['fragment] {:child-count [0 1]}] (f (c/fragment) (c/fragment (dom/span)))))
    
    (is (= nil (f (dom/div) (dom/div))))
    (is (= [[] {:tag ["div" "span"]}] (f (dom/div) (dom/span))))
    (is (= [['div 0] {:tag ["span" "br"]}] (f (dom/div (dom/span)) (dom/div (dom/br)))))
    (is (= [['div] {:child-count [0 1]}] (f (dom/div) (dom/div (dom/br)))))
    (is (= [['div] {:attributes [{:a 10} {:a 42}]}] (f (dom/div {:a 10 :b 1}) (dom/div {:a 42 :b 1}))))
    (is (= [['div] {:events [{:ona :x} {:ona :y}]}] (f (dom/div {:ona :x}) (dom/div {:ona :y}))))
    ;; TODO dom refs?

    (is (= nil (f (c/dynamic :f) (c/dynamic :f))))
    (is (= [['dynamic] {:function [:f1 :f2]}] (f (c/dynamic :f1) (c/dynamic :f2))))
    (is (= [['dynamic] {:arguments [[:a1] [:a2]]}] (f (c/dynamic :f1 :a1) (c/dynamic :f1 :a2))))
    ;; WithRef, WithAsynActions same

    (is (= [['handle-action] {:function [:f1 :f2]}] (f (c/handle-action (dom/div) :f1) (c/handle-action (dom/div) :f2))))
    (is (= [['handle-action] {:tag ["div" "span"]}] (f (c/handle-action (dom/div) :f) (c/handle-action (dom/span) :f))))
    ;; same other Wrappers

    (is (= [['named] {:name ["foo" "bar"]}]  (f (c/named (c/name-id "foo") (dom/div)) (c/named (c/name-id "bar") (dom/div)))))
    (let [foo1 (c/name-id "foo")
          foo2 (c/name-id "foo")]
      (is (= [['named] {:name-id [foo1 foo2]}] (f (c/named foo1 (dom/div)) (c/named foo2 (dom/div))))))
    (let [foo (c/name-id "foo")]
      (is (= [['foo] {:tag ["div" "span"]}]  (f (c/named foo (dom/div)) (c/named foo (dom/span))))))

    (is (= [['keyed] {:key ["foo" "bar"]}]  (f (c/keyed (dom/div) "foo") (c/keyed (dom/span) "bar"))))

    (is (= nil (f (c/livecycle (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b)))
                  (c/livecycle (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))))))

    (is (= [[] {:init [(f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))]}] 
           (f (c/livecycle (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b)))
              (c/livecycle (f/constantly (c/return :action :b)) (f/constantly (c/return :action :b))))))
    (is (= [[] {:finish [(f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))]}] 
           (f (c/livecycle (f/constantly (c/return)) (f/constantly (c/return :action :a)))
              (c/livecycle (f/constantly (c/return)) (f/constantly (c/return :action :b))))))

    (is (= [['focus] {:lens [:a :b]}] (f (c/focus :a (dom/div)) (c/focus :b (dom/div)))))
    (is (= [['focus 'div] {:attributes [{:a 10} {:a 42}]}] (f (c/focus :k1 (dom/div {:a 10 :b 1})) (c/focus :k1 (dom/div {:a 42 :b 1})))))
    )
  
  )

(defspec perf-test-does-not-fail {:seed 4712
                                  :num-tests 200}
  ;; mainly checks that everything is covered somehow, and it does not throw errors.
  (prop/for-all [v item-gen/item]
                (perf/performance v '(:foo :bar))))
