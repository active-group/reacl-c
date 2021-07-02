(ns reacl-c.main-reacl-test
  "Test interop with Reacl"
  (:require [reacl-c.main.reacl :as main]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            [reacl-c.interop.reacl :as i]
            [reacl-c.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]
            ["react-dom/test-utils" :as react-tu]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest lift-reacl-test
  ;; TODO: maybe this should use only reacl-c/test-utils?
  ;; using Reacl classes in reacl-c
  (testing "syncs state"
    (let [it (i/lift (reacl/class "foo" this state []
                                  render (rdom/div state)))]
      (is (= "ok1" (btest/text (.-firstChild (btest/renders-as it "ok1"))))))
    
    (let [it (c/focus :v
                      (i/lift (reacl/class "foo" this state []
                                           component-did-mount (fn [] (reacl/return :app-state (str state "-ok11")))
                                           render (rdom/div state))))]
      (is (= "start-ok11" (btest/text (.-firstChild (btest/renders-as it {:v "start"})))))))
  
  (testing "emits actions on mount"
    (let [it (i/lift (reacl/class "foo" this state [v]
                                  component-did-mount (fn [] (reacl/return :action v))
                                  render (rdom/div state))
                     "ok2")]
      (is (= "ok2" (btest/text (.-firstChild (btest/renders-as (-> it
                                                                   (c/handle-action (fn [state a] a)))
                                                               "start")))))))
  (testing "emits actions on event"
    (let [it (i/lift (reacl/class "foo" this state [v]
                                  handle-message (fn [msg] (reacl/return :action msg))
                                  render (rdom/div {:onclick (fn [ev]
                                                               (reacl/send-message! this v))}
                                                   state))
                     "ok2")
          ;; action -> state -> div text
          node (btest/renders-as (-> it
                                     (c/handle-action (fn [state a] a)))
                                 "start")]
      (react-tu/Simulate.click node)
      (is (= "ok2" (btest/text (.-firstChild node))))))
  
  (testing "forwards messages"
    (let [it (i/lift (reacl/class "foo3" this state []
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
                        render (main/embed (reacl/bind this)
                                           (dom/div (c/dynamic str))))]
    (reacl/render-component node wr "ok1")
    (is (= "ok1" (btest/text (.-firstChild (.-firstChild node))))))

  (let [node (js/document.createElement "div")
        [x inject!] (btest/injector)
        wr (reacl/class "foo" this state []
                        render (rdom/fragment (rdom/div state)
                                              (main/embed (reacl/bind this)
                                                          x)))]
    (reacl/render-component node wr "start")
    (inject! node (constantly (c/return :state "ok2")))
    (is (= "ok2" (btest/text (.-firstChild (.-firstChild node)))))))

(deftest full-reacl-test
  ;; i/lift and main/embed also work in combination.
  (is (btest/passes-actions
       (fn [x]
         (i/lift (reacl/class "foo" this state []
                              render (main/embed (reacl/bind this) x))))))
  (is (btest/passes-messages
       (fn [x]
         (i/lift (reacl/class "foo" this state []
                              refs [child]
                              handle-message (fn [msg] (reacl/return :message [(reacl/get-dom child) msg]))
                              render (-> (main/embed (reacl/bind this) x)
                                         (reacl/refer child)))))))
  )
