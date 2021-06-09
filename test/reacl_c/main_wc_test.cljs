(ns reacl-c.main-wc-test
  "Test interop with Web Components"
  (:require [reacl-c.main.wc :as wc :include-macros true]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            ;;[reacl-c.interop.wc :as i]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dom-testing]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(defn- rendering-async [wc-or-name f & [before-mount]]
  (let [tag (if (string? wc-or-name)
              wc-or-name
              (let [s (name (gensym "main-wc-test"))]
                (wc/define-wc* s wc-or-name)
                s))
        e (js/document.createElement tag)]
    (when before-mount (before-mount e))
    (.appendChild js/document.body e)
    (f e (fn cleanup []
           (.removeChild js/document.body e)))))

(defn- rendering [wc-or-name f & [before-mount]]
  (rendering-async wc-or-name
                   (fn [e cleanup]
                     (try (f e)
                          (finally (cleanup))))
                   before-mount))

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

(deftest method-test
  (testing "after mount"
    (rendering (-> (wc/base (c/dynamic str) 41)
                   (wc/method :test (fn [state return a]
                                      (c/return :state (inc state)
                                                :action (return a)))))
               (fn [e]
                 (is (= 'foo (.test e 'foo)))
                 (is (= "42" (.-textContent e))))))
  (testing "before mount"
    (rendering (-> (wc/base (c/dynamic str) 41)
                   (wc/method :test (fn [state return a]
                                      (c/return :state (inc state)
                                                :action (return a)))))
               (fn [e]
                 (is (= "42" (.-textContent e))))
               (fn [e]
                 (is (= 'foo (.test e 'foo)))))))

(deftest shadow-test
  (rendering (-> (dom/div "Hello World")
                 (wc/shadow {:mode "open"}))
             (fn [e]
               (is (some? (.-shadowRoot e)))
               (is (= "DIV" (.-tagName (.-firstChild (.-shadowRoot e)))))
               (is (= "Hello World" (.-textContent (.-firstChild (.-shadowRoot e)))))))
  (rendering (-> (dom/div "Hello World")
                 (wc/shadow {:mode "closed"}))
             (fn [e]
               (is (nil? (.-shadowRoot e)))
               (is (nil? (.-firstChild e))))))

(deftest dispatch-event-test
  (let [result (atom nil)]
    (async done
           (rendering-async
            (dom/div (c/init (c/return :action (wc/dispatch-event! (wc/event "foo" {:detail ::x})))))
            (fn [e cleanup]
              (js/setTimeout (fn []
                               (cleanup)
                               (is (= ::x @result))
                               (done))
                             5))
            (fn [^js/HTMLElement e]
              (.addEventListener e "foo" (fn [ev]
                                           (reset! result (.-detail ev)))))))))
