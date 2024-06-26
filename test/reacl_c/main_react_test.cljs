(ns reacl-c.main-react-test
  "Test interop with Reacl"
  (:require [reacl-c.main.react :as main]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.impl.react0 :as r0]
            [reacl-c.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest embed-test
  (let [e (main/embed (dom/div "Hello World"))
        host (js/document.createElement "div")]
    (r0/render-component e host)
    (let [n (first (array-seq (.-childNodes host)))]
      (is (= (.-nodeName n) "DIV"))
      (is (= "Hello World" (.-textContent n))))))

(deftest send-message-test
  (let [e (main/embed (c/isolate-state "foo"
                                       (c/handle-message (fn [st msg]
                                                           msg)
                                                         (c/dynamic (fn [st]
                                                                      (dom/div st))))))
        host (js/document.createElement "div")]
    (r0/render-component e host)
    (main/send-message! e "bar")
    (let [n (first (array-seq (.-childNodes host)))]
      (is (= (.-nodeName n) "DIV"))
      (is (= "bar" (.-textContent n))))))

(deftest embed-state-test
  (testing "works basically"
    (let [state (atom [])
          e (main/embed (dom/button {:onClick (fn [state ev]
                                                (c/return :state (cons :click state)))}
                                    "Hello World")
                        {:state @state
                         :set-state! (fn [st]
                                       (reset! state st))})
          host (js/document.createElement "div")]
      (r0/render-component e host)
      (let [btn (first (array-seq (.-childNodes host)))]
        (dt/fire-event btn :click)
        (is (= [:click] @state)))))

  (testing "action handlers see simultanous state changes"
    ;; even is :set-state! would do something weird; we must assume
    ;; that a new state takes effect in action handlers; that's a basic guarantee of reacl-c.
    (let [state (atom [])
          e (main/embed (-> (dom/button {:onClick (fn [state ev]
                                                    (c/return :state (conj state :click)
                                                              :action :foo))}
                                        "Hello World")
                            (c/handle-action (fn [state a]
                                               (c/return :state (conj state :action)))))
                        {:state @state
                         :set-state! (fn [st]
                                       (reset! state st))})
          host (js/document.createElement "div")]
      (r0/render-component e host)
      (let [btn (first (array-seq (.-childNodes host)))]
        (dt/fire-event btn :click)
        (is (= [:click :action] @state))))))
