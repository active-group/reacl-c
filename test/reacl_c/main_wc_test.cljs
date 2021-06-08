(ns reacl-c.main-wc-test
  "Test interop with Web Components"
  (:require [reacl-c.main.wc :as wc :include-macros true]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            ;;[reacl-c.interop.wc :as i]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dom-testing]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest base-test
  (let [m (wc/define-wc* "base-test-comp"
            (dom/div "Hello World"))]
    
    (let [e (js/document.createElement "base-test-comp")]
      (.appendChild js/document.body e)
      (try (js/console.log (.-firstChild e))
           (is (= "DIV" (.-tagName (.-firstChild e))))
           (is (= "Hello World" (.-textContent (.-firstChild e))))
           (finally (.removeChild js/document.body e))))))
