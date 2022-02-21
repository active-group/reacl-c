(ns reacl-c.main-react-test
  "Test interop with Reacl"
  (:require [reacl-c.main.react :as main]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            ["react" :as react]
            ["react-dom" :as react-dom]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest embed-test
  (let [e (main/embed (dom/div "Hello World"))
        host (js/document.createElement "div")]
    (react-dom/render e host)
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
    (react-dom/render e host)
    (main/send-message! e "bar")
    (let [n (first (array-seq (.-childNodes host)))]
      (is (= (.-nodeName n) "DIV"))
      (is (= "bar" (.-textContent n))))))
