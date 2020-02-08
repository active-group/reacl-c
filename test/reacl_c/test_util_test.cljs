(ns reacl-c.test.test-util-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.core :as tu]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest mount-test
  (is (= (c/return)
         (-> (tu/env (dom/div))
             (tu/mount! :state))))
  (is (= (c/return :action ::act)
         (-> (tu/env (c/once (c/return :action ::act)))
             (tu/mount! :state)))))

(deftest update-test
  (let [e (tu/env (c/dynamic
                   (fn [state]
                     (if state (dom/div) (dom/span)))))]
    (tu/mount! e true)
    (is (some? (xpath/select-one (tu/get-component e) (xpath/>> ** "div"))))
    (tu/update! e false)
    (is (some? (xpath/select-one (tu/get-component e) (xpath/>> ** "span"))))))

(deftest full-update-test
  (let [e (tu/env (c/dynamic (fn [state]
                               (c/once (if (< state 10)
                                         (c/return :state (inc state))
                                         (c/return))))))]
    (is (= (c/return :state 1) (tu/mount! e 0)))
    (is (= (c/return :state 2) (tu/update! e 1))) ;; only one update
    (is (= (c/return :state 10) (tu/update!! e 2)))) ;; all updates
  (let [e (tu/env (c/dynamic (fn [state]
                               (c/once (c/return :state (inc state))))))]
    (tu/mount! e 0)
    (try (tu/update!! e 1)
         (is false)
         (catch :default e
           (is true)))))

(deftest unmount-test
  (is (= (c/return :action ::act)
         (let [e (tu/env (c/once (c/return) (c/return :action ::act)))]
           (tu/mount! e :state)
           (tu/unmount! e)))))

(deftest send-message-test
  (is (= (c/return :action :msg)
         (let [e (tu/env (->> (dom/div)
                              (c/handle-message
                               (fn [msg]
                                 (c/return :action msg)))))]
           (tu/mount! e :state)
           (tu/send-message! e :msg))))
  (is (= (c/return :state :state2)
         (let [e (tu/env (->> (dom/div)
                              (c/handle-message
                               (fn [msg]
                                 (c/return :state msg)))))]
           (tu/mount! e :state1)
           (tu/send-message! (tu/get-component e) :state2)))))

(deftest invoke-callback-test
  (let [e (tu/env (dom/div {:onclick (constantly (c/return :action :act1))}))
        as (fn [v]
             (tu/invoke-callback! (xpath/select-one (tu/get-component e) (xpath/>> ** v))
                                  :onclick #js {:type "click"}))]
    (tu/mount! e :state)
    (is (= (c/return :action :act1) (as "div")))
    ;; TODO: (is (= (c/return :action :act1) (as dom/div)))
    ))

(deftest inject-action-test
  (let [foobar (c/name-id "foobar")
        e (tu/env (c/named foobar (dom/div)))
        as (fn [v] (tu/inject-action! (xpath/select-one (tu/get-component e) (xpath/>> ** v))
                                      :act1))]
    (tu/mount! e :state)
    (is (= (c/return :action :act1) (as foobar)))
    ;; TODO: (is (= (c/return :action :act1) (as "div")))
    ;; TODO: we could make it a requirement that the item does really emit actions normally; saying plain dom elements can't be used here.
    ;; TODO: (is (= (c/return :action :act1) (as dom/div)))
    ))

(deftest inject-state-change-test
  (c/defn-dynamic foobar state [] (dom/div (pr-str state)))
  (let [e (tu/env (foobar))
        as (fn [v st] (tu/inject-state-change! (xpath/select-one (tu/get-component e) (xpath/>> ** v))
                                               st))]
    (tu/mount! e :state1)
    (is (some? (.-name (c/meta-name-id foobar))))
    (is (= (c/return :state :state2) (as foobar :state2)))
    ;; TODO: (is (= (c/return :state :state3) (as "div" :state3)))
    ;; TODO: (is (= (c/return :state :state4) (as dom/div :state4)))
    ))

(deftest resolve-deep-test
  (is (= (dom/div) (tu/resolve-deep (dom/div) nil)))
  (is (= (dom/div "foo") (tu/resolve-deep (c/dynamic #(dom/div %)) "foo")))
  (let [x (c/name-id "x")]
    (is (= (c/named x (dom/div "foo")) (tu/resolve-deep (c/named x (c/dynamic #(dom/div %))) "foo"))))
  (is (= (c/focus :x (dom/div "foo")) (tu/resolve-deep (c/focus :x (c/dynamic #(dom/div %))) {:x "foo"})))
  (is (= (c/fragment (dom/div "foo")) (tu/resolve-deep (c/fragment (c/dynamic #(dom/div %))) "foo")))
  (is (= (dom/span (dom/div "foo")) (tu/resolve-deep (dom/span (c/dynamic #(dom/div %))) "foo")))
  (is (= (c/handle-action (dom/div "foo") :f) (tu/resolve-deep (c/handle-action (c/dynamic #(dom/div %)) :f) "foo")))
  ;; ...?
  )

(deftest performance-check-test
  ;; ideal is, when it uses no state, but there is only one value in the state domain
  (is (= :ideal (tu/performance-check (dom/div) (list nil))))
  ;; not ideal is, when more than one state value exists, but it's independant of it
  (is (= :good (tu/performance-check (dom/div) (list :a :b))))
  (is (= :good (tu/performance-check (c/dynamic (c/partial #(dom/div))) (list :a :b))))
  ;; but ideal again, when that state is used.
  (is (= :ideal (tu/performance-check (c/dynamic (c/partial #(dom/div (str %)))) (list :a :b))))
  ;; bad is, when the dynamic rendering has side effects (returns different result for the same state)
  (is (= :bad (tu/performance-check (c/dynamic (fn [state]
                                                 (c/dynamic
                                                  (fn [state]
                                                    (dom/div (str state))))))
                                    (list :a))))
  )

(deftest verify-performance-test
  (is (nil? (tu/verify-performance :good (c/dynamic (c/partial #(dom/div))) (list :a :b))))
  (is (nil? (tu/verify-performance :ideal (c/dynamic (c/partial #(dom/div (str %)))) (list :a :b))))

  (is (nil? (tu/verify-performance :ideal
                                   (c/fragment (c/focus :x (c/dynamic #(dom/div %)))
                                               (c/focus :a (c/dynamic #(dom/div %))))
                                   (list {:a "1" :x "y"} {:a "2" :x "y"}))))

  (let [f1 (fn [state] (dom/div)) 
        f2 (fn [state] (dom/div))
        fst (atom true)]
    (is (= [:bad [{:state :a
                   :different-at [[base/Dynamic] {:function [f1 f2]}]}]]
           ;; Note: we 'simulate' a dynamic with a (fn) in the body, in order to make a = check against them.
           ;; The order in the result might depend on how often the impl resolves the dynamic. Not an error if the other way round.
           (tu/verify-performance :good (c/dynamic (fn [state]
                                                     (c/dynamic (if @fst
                                                                  (do (reset! fst false) f1)
                                                                  (do (reset! fst true) f2)))))
                                  (list :a)))))

  (is (= [:good [{:state-1 :a
                  :state-2 :b
                  :state-diff [:a :b nil]
                  :resolved (dom/div)}]]
         (tu/verify-performance :ideal (c/dynamic (c/partial #(dom/div))) (list :a :b)))))

(deftest find-first-difference-test
  (let [f tu/find-first-difference]

    (is (= nil (f (c/fragment) (c/fragment))))
    (is (= [[base/Fragment 0] {:tag ["div" "span"]}] (f (c/fragment (dom/div)) (c/fragment (dom/span)))))
    (is (= [[base/Fragment] {:child-count [0 1]}] (f (c/fragment) (c/fragment (dom/span)))))
    
    (is (= nil (f (dom/div) (dom/div))))
    (is (= [[] {:tag ["div" "span"]}] (f (dom/div) (dom/span))))
    (is (= [["div" 0] {:tag ["span" "br"]}] (f (dom/div (dom/span)) (dom/div (dom/br)))))
    (is (= [["div"] {:child-count [0 1]}] (f (dom/div) (dom/div (dom/br)))))
    (is (= [["div"] {:attributes [{:a 10} {:a 42}]}] (f (dom/div {:a 10 :b 1}) (dom/div {:a 42 :b 1}))))
    (is (= [["div"] {:events [{:ona :x} {:ona :y}]}] (f (dom/div {:ona :x}) (dom/div {:ona :y}))))
    ;; TODO dom refs?

    (is (= nil (f (c/dynamic :f) (c/dynamic :f))))
    (is (= [[base/Dynamic] {:function [:f1 :f2]}] (f (c/dynamic :f1) (c/dynamic :f2))))
    (is (= [[base/Dynamic] {:arguments [[:a1] [:a2]]}] (f (c/dynamic :f1 :a1) (c/dynamic :f1 :a2))))
    ;; WithRef, WithAsynActions same

    (is (= [[base/HandleAction] {:function [:f1 :f2]}] (f (c/handle-action (dom/div) :f1) (c/handle-action (dom/div) :f2))))
    (is (= [[base/HandleAction] {:tag ["div" "span"]}] (f (c/handle-action (dom/div) :f) (c/handle-action (dom/span) :f))))
    ;; same other Wrappers

    (is (= [[base/Named] {:name ["foo" "bar"]}]  (f (c/named (c/name-id "foo") (dom/div)) (c/named (c/name-id "bar") (dom/div)))))
    (let [foo1 (c/name-id "foo")
          foo2 (c/name-id "foo")]
      (is (= [[base/Named] {:name-id [foo1 foo2]}] (f (c/named foo1 (dom/div)) (c/named foo2 (dom/div))))))
    (let [foo (c/name-id "foo")]
      (is (= [["foo"] {:tag ["div" "span"]}]  (f (c/named foo (dom/div)) (c/named foo (dom/span))))))

    (is (= [[base/Keyed] {:key ["foo" "bar"]}]  (f (c/keyed (dom/div) "foo") (c/keyed (dom/span) "bar"))))

    (is (= nil (f (c/once (c/return :state :a)) (c/once (c/return :state :a)))))
    (is (= nil (f (c/once (c/return :state :a) (c/return :state :b)) (c/once (c/return :state :a) (c/return :state :b)))))

    (is (= [[] {:once [(c/return :state :a) (c/return :state :b)]}] 
           (f (c/once (c/return :state :a)) (c/once (c/return :state :b)))))
    (is (= [[] {:once-cleanup [(c/return :state :a) (c/return :state :b)]}] 
           (f (c/once (c/return) (c/return :state :a)) (c/once (c/return) (c/return :state :b)))))

    (is (= [[base/Focus] {:lens [:a :b]}] (f (c/focus :a (dom/div)) (c/focus :b (dom/div)))))
    (is (= [[base/Focus "div"] {:attributes [{:a 10} {:a 42}]}] (f (c/focus :k1 (dom/div {:a 10 :b 1})) (c/focus :k1 (dom/div {:a 42 :b 1})))))
    )
  
  )
