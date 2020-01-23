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
  (is (= (c/return :action [::act :state])
         (-> (tu/env (c/dynamic
                      (fn [state]
                        (-> (dom/div)
                            (c/did-mount (constantly [::act state]))))))
             (tu/mount! :state)))))

(deftest update-test
  (is (= (c/return :action [::act :state2])
         (let [e (tu/env (c/dynamic
                          (fn [state]
                            (-> (dom/div)
                                (c/did-update (constantly [::act state]))))))]
           (tu/mount! e :state1)
           (tu/update! e :state2)))))

(deftest unmount-test
  (is (= (c/return :action [::act :state])
         (let [e (tu/env (c/dynamic
                          (fn [state]
                            (-> (dom/div)
                                (c/will-unmount (constantly [::act state]))))))]
           (tu/mount! e :state)
           (tu/unmount! e)))))

(deftest send-message-test
  (is (= (c/return :action [:msg :state])
         (let [e (tu/env (-> (dom/div)
                             (c/handle-message
                              (fn [state msg]
                                (c/return :action [msg state])))))]
           (tu/mount! e :state)
           (tu/send-message! e :msg))))
  (is (= (c/return :state [:state1 :state2])
         (let [e (tu/env (-> (dom/div)
                             (c/handle-message
                              (fn [state msg]
                                (c/return :state [state msg])))))]
           (tu/mount! e :state1)
           (tu/send-message! e :state2)))))

(deftest invoke-callback-test
  (is (= (c/return :action :act1)
         (let [e (tu/env (dom/div {:onclick (constantly :act1)}))]
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
