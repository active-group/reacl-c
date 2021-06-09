(ns reacl-c.main-wc-test
  "Test interop with Web Components"
  (:require [reacl-c.main.wc :as wc :include-macros true]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            ;;[reacl-c.interop.wc :as i]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dom-testing]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(defn- rendering [wc-or-name f & [before-mount]]
  (let [tag (if (string? wc-or-name)
              wc-or-name
              (let [s (name (gensym "main-wc-test"))]
                (wc/define-wc* s wc-or-name)
                s))
        e (js/document.createElement tag)]
    (when before-mount (before-mount e))
    (.appendChild js/document.body e)
    (try (f e)
         (finally (.removeChild js/document.body e)))))

(deftest base-test
  (rendering (dom/div "Hello World")
             (fn [e]
               (is (= "DIV" (.-tagName (.-firstChild e))))
               (is (= "Hello World" (.-textContent (.-firstChild e)))))))

(deftest connect-disconnect-test
  (let [connected? (atom false)]
    (rendering (-> (dom/div "Hello World")
                   (wc/connected (fn [state]
                                   (reset! connected? true)
                                   state))
                   (wc/disconnected (fn [state]
                                      (reset! connected? false)
                                      state)))
               (fn [e]
                 (is @connected?)))
    (is (not @connected?))))

(deftest attributes-test
  (testing "change after mount"
    (let [change (atom nil)]
      (rendering (-> (dom/div "Hello World")
                     (wc/attribute-changed "test" (fn [state old new]
                                                    (reset! change [old new])
                                                    state)))
                 (fn [e]
                   (.setAttribute e "test" "foo")
                   (is (= [nil "foo"] @change))))))
  (testing "change before mount"
    (let [change (atom nil)]
      (rendering (-> (c/dynamic dom/div)
                     (wc/attribute-changed "test" (fn [state old new]
                                                    (reset! change [old new])
                                                    new)))
                 (fn [e]
                   (is (= "foo" (.-textContent (.-firstChild e))))
                   (is (= [nil "foo"] @change)))
                 (fn [e]
                   (.setAttribute e "test" "foo")))))
  (testing "attribute to state util"
    (rendering (-> (wc/base (c/focus :test (c/dynamic dom/div)) {})
                   (wc/attribute :test))
               (fn [e]
                 (.setAttribute e "test" "foo")
                 (is (= "foo" (.-textContent (.-firstChild e))))))))

(deftest properties-test
  (testing "after mount"
    (rendering (-> (wc/base (dom/div) {:test 'foo})
                   (wc/property :test))
               (fn [e]
                 (is (= 'foo (.-test e)))
                 (set! (.-test e) 'bar)
                 (is (= 'bar (.-test e))))))
  (testing "before mount"
    (let [change (atom nil)]
      (rendering (-> (wc/base (dom/div) {:test 'foo})
                     (wc/property "test" :test))
                 (fn [e]
                   (is (= 'bar (.-test e))))
                 (fn [e]
                   (is (= 'foo (.-test e)))
                   (set! (.-test e) 'bar)
                   (is (= 'bar (.-test e))))))))
