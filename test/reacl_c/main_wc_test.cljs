(ns reacl-c.main-wc-test
  "Test interop with Web Components"
  (:require [reacl-c.main.wc :as wc]
            [reacl-c.main-browser-test :as btest]
            [reacl-c.main :as main]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
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
                                      (println "disconnecting")
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

(deftest dispatch-test-1
  (let [result (atom nil)]
    (async done
           (rendering-async
            (constantly (dom/div (c/init (c/return :action (wc/dispatch (wc/event "foo" {:detail ::x}))))))
            (fn [e cleanup]
              (js/setTimeout (fn []
                               (cleanup)
                               (is (= ::x @result))
                               (done))
                             5))
            (fn [^js/HTMLElement e]
              (.addEventListener e "foo" (fn [ev]
                                           (reset! result (.-detail ev)))))))))

(deftest dispatch-test-2
  (let [result (atom nil)]
    (async done
           (rendering-async
            (constantly (dom/div (c/execute-effect (wc/dispatch (wc/event "foo" {:detail ::x}))
                                                   (fn [state res]
                                                     (reset! result res)
                                                     state))))
            (fn [e cleanup]
              (js/setTimeout (fn []
                               (cleanup)
                               (is (= true @result)) ;; = not cancelled
                               (done))
                             5))))))

(deftest use-custom-element-test
  (testing "attributes and events"
    (let [event-res (atom nil)
          wc (-> (fn [attrs]
                   ;; TODO: event name should be "fooBar"
                   (c/init (c/return :action (wc/dispatch (wc/event "foobar" {:detail (:x attrs)})))))
                 (wc/attribute :x))]
      (async done
             (rendering-async
              "div"
              (fn [e cleanup]
                (main/run e
                  (wc/use wc {:x "42"
                              :onFooBar (fn [st ev]
                                          (reset! event-res (.-detail ev))
                                          st)}))
                (js/setTimeout (fn []
                                 (cleanup)
                                 (is (= "42" @event-res))
                                 (done))
                               1))))))

  ;; Note: Setting properties via element props is not reasonable in
  ;; the general case; property setters and getters can have side
  ;; effects; some properties can only be set after mount; how should
  ;; diffing work (with the previous value set, with the current value
  ;; of the property; delete a property or reset to 'undefined'?;
  ;; etc.) It might make some sense for 'attribute like properties',
  ;; but it's also difficould for our web components, which allow
  ;; properties to change the state of the web component, which is not
  ;; possible before the item is fully mounted (uses
  ;; send-message/setState internally).
  
  ;; Note: The React community has a very long discussion history
  ;; about that too and no agreement; there might be something
  ;; upcoming in React 18 or 19.  See:
  ;; https://github.com/facebook/react/issues/11347

  #_(testing "properties from props"
      (let [pval (atom nil)
            wc (-> (fn [attrs]
                     (c/with-state-as state
                       (reset! pval (:y state))
                       (dom/div)))
                   (wc/property :y))]
        (async done
               (rendering-async
                "div"
                (fn [e cleanup]
                  (main/run e
                    (wc/use wc {:y '42}))
                  (js/setTimeout (fn []
                                   (cleanup)
                                   (is (= '42 @pval))
                                   (done))
                                 1)))))))

(deftest redefine-test
  (testing "updated item"
    (let [name "redefine-test-1"]
      (wc/define! name (fn [] (dom/div)))

      (rendering name
                 (fn [e]
                   (is (= "DIV" (.-tagName (.-firstChild e))))))

      (wc/define! name (fn [] (dom/span)))

      (rendering name
                 (fn [e]
                   (is (= "SPAN" (.-tagName (.-firstChild e))))))))
  
  (testing "hot updated item"
    (let [name "redefine-test-2"]
      (wc/define! name (fn [] (dom/div)))

      (rendering name
                 (fn [e]
                   (let [t (.-tagName e)]
                     (is (= "DIV" (.-tagName (.-firstChild e))))

                     (wc/define! name (fn [] (dom/span)))
                     (is (= "SPAN" (.-tagName (.-firstChild e))))

                     (is (= t (.-tagName e))))))))

  ;; other things can be hot-reloaded too, but the item is the main/most important part.
  )
