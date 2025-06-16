(ns reacl-c.main-react-test
  "Test interop with React"
  (:require [reacl-c.main.react :as main]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.impl.react0 :as r0 :include-macros true]
            [reacl-c.dom-testing :as dt]
            [reacl-c.interop.react :as interop]
            [cljs.test :refer (is deftest testing async) :include-macros true]))


(defn react-rendering [react-elem f]
  (let [host (js/document.createElement "div")]
    ;; especially for event, adding it to the document can be necessary.
    (.appendChild js/document.body host)
    (dt/flush-sync! #(r0/render-component react-elem host))
    (try
      (f host)
      (finally
        (.removeChild js/document.body host)))))

(deftest embed-test
  (react-rendering
   (main/embed (dom/div "Hello World"))
   (fn [host]
     (let [n (first (array-seq (.-childNodes host)))]
       (is (= (.-nodeName n) "DIV"))
       (is (= "Hello World" (.-textContent n)))))))

(deftest send-message-test
  (let [e (main/embed (c/isolate-state "foo"
                                       (c/handle-message (fn [st msg]
                                                           msg)
                                                         (c/dynamic (fn [st]
                                                                      (dom/div st))))))]
    (react-rendering
     e
     (fn [host]
       (r0/flush-sync! #(main/send-message! e "bar"))
       (let [n (first (array-seq (.-childNodes host)))]
         (is (= (.-nodeName n) "DIV"))
         (is (= "bar" (.-textContent n))))))))

(deftest embed-state-test
  (testing "works basically"
    (let [state (atom [])]
      (react-rendering
       (main/embed (dom/button {:onClick (fn [state ev]
                                           (c/return :state (cons :click state)))}
                               "Hello World")
                   {:state @state
                    :set-state! (fn [st]
                                  (reset! state st))})
       (fn [host]
         (let [btn (first (array-seq (.-childNodes host)))]
           (assert (some? btn))
           (dt/fire-event btn :click)
           (is (= [:click] @state)))))))

  (testing "action handlers see simultanous state changes"
    ;; even is :set-state! would do something weird; we must assume
    ;; that a new state takes effect in action handlers; that's a basic guarantee of reacl-c.
    (let [state (atom [])]
      (react-rendering
       (main/embed (-> (dom/button {:onClick (fn [state ev]
                                               (c/return :state (conj state :click)
                                                         :action :foo))}
                                   "Hello World")
                       (c/handle-action (fn [state a]
                                          (c/return :state (conj state :action)))))
                   {:state @state
                    :set-state! (fn [st]
                                  (reset! state st))})
       (fn [host]
         (let [btn (first (array-seq (.-childNodes host)))]
           (assert (some? btn))
           (dt/fire-event btn :click)
           (is (= [:click :action] @state))))))))

(r0/defclass TestRefClass
  "render" (fn [this]
             "foo"))

(defn rinstance? [class comp]
  (when comp
    (when-let [p (js/Object.getPrototypeOf comp)]
      (= class (.-constructor p)))))

(deftest refs-to-native-test
  (let [f (fn [variant test]
            (let [the-ref (atom nil)]
              (react-rendering
               (main/embed (c/with-ref (fn [ref]
                                         (reset! the-ref ref)
                                         (variant ref))))
               (fn [host]
                 (is (test (c/deref @the-ref)))))))]
    (testing "as a dom attrs"
      (f #(dom/div {:ref %}) #(instance? js/Node %)))
    (testing "as a refer item to dom"
      (f #(c/refer (dom/div) %) #(instance? js/Node %)))
    (testing "as a refer item to component"
      (f #(c/refer (interop/lift TestRefClass #js{}) %) #(rinstance? TestRefClass %)))))
