(ns reacl-c.main-reacl-test
  "Test interop with Reacl"
  (:require [reacl-c.main.reacl :as main]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            [reacl-c.interop.reacl :as i]
            [reacl-c.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]
            [active.clojure.lens :as lens]
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

(deftest lift-container-rerender-test
  (reacl/defclass lifted this state [class & args]
    render
    (-> (if (and (reacl/reacl-class? class) (reacl/has-app-state? class))
          (apply class (reacl/bind this) args)
          (apply class args))
        (reacl/action-to-message this (fn [x] x)))

    handle-message
    (fn [msg]
      (do (assert false msg)
          (reacl/return))))

  (defn lift [class & args]
    ;; Use Reacl class in Reacl-c
    (apply i/lift lifted class args))

  (reacl/defclass lifted-container this state [f attrs content]
    render
    (apply f attrs
           (map (partial main/embed (reacl/bind this)) content)))
  
  (defn lift-container [f attrs & content]
    ;; Use Reacl container fn as Reacl-C dom container fn, with Reacl-c items as content!
    (lift lifted-container f attrs content))

  (reacl/defclass cont1 this state [attrs elem]
    render
    (rdom/div attrs elem))

  (let [called (atom 0)
        item (let [f (fn []
                       (swap! called inc)
                       (dom/div "foo"))]
               (fn []
                 (lift-container cont1 {}
                                 (c/local-state "x" (c/focus lens/second (c/dynamic f))))))]
    (let [[app host] (btest/render (item) 42)]
      (is (= 1 @called))
      ;; rendering with new state, will not make f being called again (because it's focused to ignore it)
      (btest/run-in host (item) 43)
      (is (= 1 @called)))))
