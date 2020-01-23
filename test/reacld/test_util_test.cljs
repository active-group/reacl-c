(ns reacld.test.test-util-test
  (:require [reacld.core :as r :include-macros true]
            [reacld.core :as core]
            [reacld.dom :as dom]
            [reacld.test-util.core :as tu]
            [reacld.test-util.xpath :as xpath :include-macros true]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest mount-test
  (is (= (core/return)
         (-> (tu/env (dom/div))
             (tu/mount! :state))))
  (is (= (core/return :action [::act :state])
         (-> (tu/env (core/dynamic
                      (fn [state]
                        (-> (dom/div)
                            (core/did-mount (constantly [::act state]))))))
             (tu/mount! :state)))))

(deftest update-test
  (is (= (core/return :action [::act :state2])
         (let [e (tu/env (core/dynamic
                          (fn [state]
                            (-> (dom/div)
                                (core/did-update (constantly [::act state]))))))]
           (tu/mount! e :state1)
           (tu/update! e :state2)))))

(deftest unmount-test
  (is (= (core/return :action [::act :state])
         (let [e (tu/env (core/dynamic
                          (fn [state]
                            (-> (dom/div)
                                (core/will-unmount (constantly [::act state]))))))]
           (tu/mount! e :state)
           (tu/unmount! e)))))

(deftest send-message-test
  (is (= (core/return :action [:msg :state])
         (let [e (tu/env (-> (dom/div)
                             (core/handle-message
                              (fn [state msg]
                                (core/return :action [msg state])))))]
           (tu/mount! e :state)
           (tu/send-message! e :msg))))
  (is (= (core/return :state [:state1 :state2])
         (let [e (tu/env (-> (dom/div)
                             (core/handle-message
                              (fn [state msg]
                                (core/return :state [state msg])))))]
           (tu/mount! e :state1)
           (tu/send-message! e :state2)))))

(deftest invoke-callback-test
  (is (= (core/return :action :act1)
         (let [e (tu/env (dom/div {:onclick (constantly :act1)}))]
           (tu/mount! e :state)
           (tu/invoke-callback! (xpath/select-one (tu/get-component e) (xpath/>> ** "div"))
                                :onclick #js {:type "click"})))))

(deftest inject-action-test
  (is (= (core/return :action :act1)
         (let [e (tu/env (core/named (dom/div) "foobar"))]
           (tu/mount! e :state)
           (tu/inject-action! (xpath/select-one (tu/get-component e) (xpath/>> ** "foobar"))
                              :act1)))))

(deftest inject-state-change-test
  (is (= (core/return :state :state2)
         (do (core/def-dynamic foobar state (dom/div (pr-str state)))
             (let [e (tu/env foobar)]
               (tu/mount! e :state1)
               (tu/inject-state-change! (xpath/select-one (tu/get-component e) (xpath/>> ** #'foobar))
                                        :state2))))))
