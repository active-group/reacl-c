(ns reacl-c.main-reacl-test
  "Test interop with Reacl"
  (:require [reacl-c.main.reacl :as main]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest embed-reacl-test
  ;; TODO: maybe this should use only reacl-c/test-utils?
  ;; using Reacl classes in reacl-c
  (testing "syncs state"
    (let [it (c/reacl (reacl/class "foo" this state []
                                   render (rdom/div (str state))))]
      (is (= "ok1" (btest/text (.-firstChild (btest/renders-as it "ok1"))))))
    
    (let [it (c/reacl (reacl/class "foo" this state []
                                   component-did-mount (fn [] (reacl/return :app-state "ok11"))
                                   render (rdom/div (str state))))]
      (is (= "ok11" (btest/text (.-firstChild (btest/renders-as it "start")))))))
  
  (testing "emits actions"
    (let [it (c/reacl (reacl/class "foo" this state [v]
                                   component-did-mount (fn [] (reacl/return :action v))
                                   render (rdom/div (str state)))
                      "ok2")]
      (is (= "ok2" (btest/text (.-firstChild (btest/renders-as (-> it
                                                                   (c/handle-action (fn [state a] a)))
                                                               "start")))))))
  (testing "forwards messages"
    (let [it (c/reacl (reacl/class "foo3" this state []
                                   handle-message (fn [msg]
                                                    (reacl/return :app-state msg))
                                   render (rdom/span)))
          [x inject!] (btest/injector)
          n (btest/renders-as (dom/div (c/with-ref (fn [ref]
                                                     (c/fragment (c/dynamic dom/div)
                                                                 (c/refer it ref)
                                                                 (c/handle-action x (fn [state a]
                                                                                      (c/return :message [ref a])))))))
                              "start")]
      (inject! n (constantly (c/return :action "ok3")))
      (is (= "ok3" (btest/text (.-firstChild (.-firstChild n))))))))

(deftest embed-in-reacl-test
  ;; TODO: maybe this should only use reacl/test-utils
  ;; Using reacl-c items in Reacl
  (let [node (js/document.createElement "div")
        wr (reacl/class "foo" this state []
                        render (main/reacl (reacl/bind this)
                                           (dom/div (c/dynamic str))))]
    (reacl/render-component node wr "ok1")
    (is (= "ok1" (btest/text (.-firstChild (.-firstChild node))))))

  (let [node (js/document.createElement "div")
        [x inject!] (btest/injector)
        wr (reacl/class "foo" this state []
                        render (rdom/fragment (rdom/div state)
                                              (main/reacl (reacl/bind this)
                                                          x)))]
    (reacl/render-component node wr "start")
    (inject! node (constantly (c/return :state "ok2")))
    (is (= "ok2" (btest/text (.-firstChild (.-firstChild node)))))))

(deftest full-reacl-test
  ;; render and lift also work in combination.
  (is (btest/passes-actions
       (fn [x]
         (c/reacl (reacl/class "foo" this state []
                               render (main/reacl (reacl/bind this) x))))))
  (is (btest/passes-messages
       (fn [x]
         (c/reacl (reacl/class "foo" this state []
                               refs [child]
                               handle-message (fn [msg] (reacl/return :message [(reacl/get-dom child) msg]))
                               render (-> (main/reacl (reacl/bind this) x)
                                          (reacl/refer child)))))))
  )
