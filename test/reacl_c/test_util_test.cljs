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
         (-> (tu/env (c/did-mount (c/return :action ::act)))
             (tu/mount! :state)))))

(deftest update-test
  (is (= (c/return :action [::act :state2])
         (let [e (tu/env (c/dynamic
                          (fn [state]
                            (c/did-update (dom/div #_(str state))
                                          (constantly (c/return :action [::act state]))))))]
           (tu/mount! e :state1)
           (tu/update! e :state2)))))

(deftest full-update-test
  (let [e (tu/env (c/dynamic (fn [state]
                               (c/did-update (dom/div)
                                             (constantly (if (< state 10)
                                                           (c/return :state (inc state))
                                                           (c/return)))))))]
    (is (= (c/return) (tu/mount! e 0)))
    (is (= (c/return :state 2) (tu/update! e 1))) ;; only one did-update
    (is (= (c/return :state 10) (tu/update!! e 2)))) ;; all did-updates
  (let [e (tu/env (c/dynamic (fn [state]
                               (c/did-update (dom/div)
                                             (constantly (c/return :state (inc state)))))))]
    (tu/mount! e 0)
    (try (tu/update!! e 1)
         (is false)
         (catch :default e
           (is true)))))

(deftest unmount-test
  (is (= (c/return :action ::act)
         (let [e (tu/env (c/will-unmount (c/return :action ::act)))]
           (tu/mount! e :state)
           (tu/unmount! e)))))

(deftest send-message-test
  (is (= (c/return :action :msg)
         (let [e (tu/env (-> (dom/div)
                             (c/handle-message
                              (fn [msg]
                                (c/return :action msg)))))]
           (tu/mount! e :state)
           (tu/send-message! e :msg))))
  (is (= (c/return :state :state2)
         (let [e (tu/env (-> (dom/div)
                             (c/handle-message
                              (fn [msg]
                                (c/return :state msg)))))]
           (tu/mount! e :state1)
           (tu/send-message! (tu/get-component e) :state2)))))

(deftest invoke-callback-test
  (is (= (c/return :action :act1)
         (let [e (tu/env (dom/div {:onclick (constantly (c/return :action :act1))}))]
           (tu/mount! e :state)
           (tu/invoke-callback! (xpath/select-one (tu/get-component e) (xpath/>> ** "div"))
                                :onclick #js {:type "click"})))))

(deftest inject-action-test
  (is (= (c/return :action :act1)
         (let [e (tu/env (c/named (dom/div) "foobar"))]
           (tu/mount! e :state)
           (tu/inject-action! (xpath/select-one (tu/get-component e) (xpath/>> ** "foobar"))
                              :act1)))))

(deftest inject-state-change-test
  (is (= (c/return :state :state2)
         (do (c/def-dynamic foobar state (dom/div (pr-str state)))
             (let [e (tu/env foobar)]
               (tu/mount! e :state1)
               (tu/inject-state-change! (xpath/select-one (tu/get-component e) (xpath/>> ** #'foobar))
                                        :state2))))))

(deftest resolve-deep-test
  (is (= (dom/div) (tu/resolve-deep (dom/div) nil)))
  (is (= (dom/div "foo") (tu/resolve-deep (c/dynamic #(dom/div %)) "foo")))
  (is (= (c/named (dom/div "foo") "x") (tu/resolve-deep (c/named (c/dynamic #(dom/div %)) "x") "foo")))
  (is (= (c/focus (dom/div "foo") :x) (tu/resolve-deep (c/focus (c/dynamic #(dom/div %)) :x) {:x "foo"})))
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
  (tu/verify-performance! :good (c/dynamic (c/partial #(dom/div))) (list :a :b))
  (is true)
  (tu/verify-performance! :ideal (c/dynamic (c/partial #(dom/div (str %)))) (list :a :b))
  (is true)

  (try (tu/verify-performance! :good (c/dynamic (fn [state] (c/dynamic (fn [state] (dom/div (str state)))))) (list :a :b))
       (is false)
       (catch :default e
         (is true)))
    
  (try (tu/verify-performance! :ideal (c/dynamic (c/partial #(dom/div))) (list :a :b))
       (is false)
       (catch :default e
         (is true))))

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

    (is (= [[base/Named] {:name ["foo" "bar"]}]  (f (c/named (dom/div) "foo") (c/named (dom/div) "bar"))))
    (is (= [["foo"] {:tag ["div" "span"]}]  (f (c/named (dom/div) "foo") (c/named (dom/span) "foo"))))

    (is (= [[base/Keyed] {:key ["foo" "bar"]}]  (f (c/keyed (dom/div) "foo") (c/keyed (dom/span) "bar"))))

    (is (= nil (f (c/did-mount (c/return :state :a)) (c/did-mount (c/return :state :a)))))

    (is (not= (c/return :state :a) (c/return :state :b)))
    (is (not= (c/did-mount (c/return :state :a)) (c/did-mount (c/return :state :b))))
    
    (is (= [[] {:did-mount [(c/return :state :a) (c/return :state :b)]}] 
           (f (c/did-mount (c/return :state :a)) (c/did-mount (c/return :state :b)))))
    ;; will-unmount is same.

    (is (= [[base/Focus] {:lens [:a :b]}] (f (c/focus (dom/div) :a) (c/focus (dom/div) :b))))
    (is (= [[base/Focus "div"] {:attributes [{:a 10} {:a 42}]}] (f (c/focus (dom/div {:a 10 :b 1}) :k1) (c/focus (dom/div {:a 42 :b 1}) :k1))))
    )
  
  )
