(ns reacl-c.test.test-util-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
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

(deftest performance-util-test
  ;; ideal is, when it uses no state, but there is only one value in the state domain
  (testing "performance-check"
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
                                      (list :a)))))
  (testing "verify-performance"
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
           (is true)))))

