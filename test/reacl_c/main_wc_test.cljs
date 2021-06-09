(ns reacl-c.main-wc-test
  "Test interop with Web Components"
  (:require [reacl-c.main.wc :as wc]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dom-testing]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(defn- rendering-async [wc-or-name f & [before-mount]]
  (let [tag (if (string? wc-or-name)
              wc-or-name
              (let [s (name (gensym "main-wc-test"))]
                (wc/define! s wc-or-name)
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
  (rendering (constantly (dom/div "Hello World"))
             (fn [e]
               (is (= "DIV" (.-tagName (.-firstChild e))))
               (is (= "Hello World" (.-textContent (.-firstChild e)))))))

(deftest connect-disconnect-test
  (let [connected? (atom false)]
    (rendering (-> (constantly (dom/div "Hello World"))
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
    (let [value (atom nil)]
      (rendering (-> (fn [attrs]
                       (reset! value (:test attrs))
                       (dom/div))
                     (wc/attribute :test))
                 (fn [e]
                   (.setAttribute e "test" "foo")
                   (is (= "foo" @value))))))
  (testing "change before mount"
    (let [value (atom nil)]
      (rendering (-> (fn [attrs]
                       (reset! value (:test attrs))
                       (dom/div (:test attrs)))
                     (wc/attribute :test))
                 (fn [e]
                   (is (= "foo" (.-textContent (.-firstChild e))))
                   (is (= "foo" @value)))
                 (fn [e]
                   (.setAttribute e "test" "foo"))))))

(deftest properties-test
  (testing "after mount"
    (rendering (-> (wc/initial-state (constantly (dom/div)) {:test 'foo})
                   (wc/property :test))
               (fn [e]
                 (is (= 'foo (.-test e)))
                 (set! (.-test e) 'bar)
                 (is (= 'bar (.-test e))))))
  (testing "before mount"
    (let [change (atom nil)]
      (rendering (-> (wc/initial-state (constantly (dom/div)) {:test 'foo})
                     (wc/property "test" :test))
                 (fn [e]
                   (is (= 'bar (.-test e))))
                 (fn [e]
                   (is (= 'foo (.-test e)))
                   (set! (.-test e) 'bar)
                   (is (= 'bar (.-test e))))))))

(deftest method-test
  (testing "after mount"
    (rendering (-> (wc/initial-state (constantly (c/dynamic str)) 41)
                   (wc/method "test" (fn [state return a]
                                       (c/return :state (inc state)
                                                 :action (return a)))))
               (fn [e]
                 (is (= 'foo (.test e 'foo)))
                 (is (= "42" (.-textContent e))))))
  (testing "before mount"
    (rendering (-> (wc/initial-state (constantly (c/dynamic str)) 41)
                   (wc/method "test" (fn [state return a]
                                      (c/return :state (inc state)
                                                :action (return a)))))
               (fn [e]
                 (is (= "42" (.-textContent e))))
               (fn [e]
                 (is (= 'foo (.test e 'foo)))))))

(deftest shadow-test
  (rendering (-> (constantly (dom/div "Hello World"))
                 (wc/shadow {:mode "open"}))
             (fn [e]
               (is (some? (.-shadowRoot e)))
               (is (= "DIV" (.-tagName (.-firstChild (.-shadowRoot e)))))
               (is (= "Hello World" (.-textContent (.-firstChild (.-shadowRoot e)))))))
  (rendering (-> (constantly (dom/div "Hello World"))
                 (wc/shadow {:mode "closed"}))
             (fn [e]
               (is (nil? (.-shadowRoot e)))
               (is (nil? (.-firstChild e))))))

(deftest dispatch-event-test-1
  (let [result (atom nil)]
    (async done
           (rendering-async
            (constantly (dom/div (c/init (c/return :action (wc/dispatch-event! (wc/event "foo" {:detail ::x}))))))
            (fn [e cleanup]
              (js/setTimeout (fn []
                               (cleanup)
                               (is (= ::x @result))
                               (done))
                             5))
            (fn [^js/HTMLElement e]
              (.addEventListener e "foo" (fn [ev]
                                           (reset! result (.-detail ev)))))))))

(deftest dispatch-event-test-2
  (let [result (atom nil)]
    (async done
           (rendering-async
            (constantly (dom/div (c/handle-effect-result (fn [state res]
                                                           (reset! result res)
                                                           state)
                                                         (wc/dispatch-event! (wc/event "foo" {:detail ::x})))))
            (fn [e cleanup]
              (js/setTimeout (fn []
                               (cleanup)
                               (is (= true @result)) ;; = not cancelled
                               (done))
                             5))))))
