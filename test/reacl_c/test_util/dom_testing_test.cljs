(ns reacl-c.test-util.dom-testing-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dom-t :include-macros true]
            [clojure.string :as string]
            [cljs-async.core :as a :refer (async await) :include-macros true]
            [cljs-async.test :refer (deftest) :include-macros true]
            [cljs.test :refer (is testing) :include-macros true]))

(defn timeout [ms]
  (a/promise (fn [f]
               (js/setTimeout #(f true) ms))))

(deftest get-query-test-1
  (dom-t/rendering
    (dom/div (dom/button {:title "Hello"}))
    (fn [env]
      (is (some? (dom-t/get env (dom-t/by-title "Hello")))))))

(deftest double-test
  (dom-t/rendering
    (dom/div (dom/button {:title "Hello"}))
    (fn [env1]
      (dom-t/rendering
       (dom/div "Foo")
       (fn [env2]
         (is (some? (dom-t/get env2 (dom-t/by-text "Foo"))))
         (is (some? (dom-t/get env1 (dom-t/by-title "Hello")))))))))

(deftest fire-event-test-2
  (dom-t/rendering
    (c/with-state-as x
      (dom/button {:title (if x "World" "Hello")
                   :type "button"
                   :onClick (constantly true)}))
    :state false
    (fn [env]
      (async
       (let [node (await (dom-t/find env (dom-t/by-title "Hello")))]
         (is (some? node))
         (dom-t/fire-event node :click)
         (await (dom-t/find env (dom-t/by-title "World"))))))))

(deftest query-equality-test
  ;; referential transparency... might be helpful
  (is (= (dom-t/by-text "foo")
         (dom-t/by-text "foo")))

  (is (= (dom-t/by-attribute "data-id" "124")
         (dom-t/by-attribute "data-id" "124")))

  (let [by-color (dom-t/build-query-fn
                  (fn [where col] nil)
                  (fn [where col] "")
                  (fn [where col] ""))]
    (is (= (by-color "white")
           (by-color "white")))))

(deftest build-query-test
  (let [by-color (dom-t/build-query-fn
                  (fn [where col]
                    ;; Note: one could use other queries on 'where' as a base, resp. dive down into the whole tree of nodes from (.-container where)
                    (filter #(= col (.-color (.-style %)))
                            (.-childNodes (.-container where))))
                  (fn [where col]
                    (str "More than one node with color: " col))
                  (fn [where col]
                    (str "No node with color: " col)))]
    (dom-t/rendering
     (c/fragment (dom/div {:style {:color "black"}})
                 (dom/div {:style {:color "blue"}})
                 (dom/div {:style {:color "blue"}}))
     :state false
     (fn [env]
       (is (some? (dom-t/query env (by-color "black"))))
       (is (nil? (dom-t/query env (by-color "white"))))

       (is (not-empty (dom-t/query-all env (by-color "black"))))

       (is (string/starts-with? (try (dom-t/get env (by-color "pink"))
                                     (catch :default e
                                       (.-message e)))
                                (str "No node with color: " "pink")))

       (is (string/starts-with? (try (dom-t/get env (by-color "blue"))
                                     (catch :default e
                                       (.-message e)))
                                (str "More than one node with color: " "blue")))))))

(deftest by-attribute-test
  (dom-t/rendering
   (dom/div {:width "100px"})
   :state false
   (fn [env]
     (is (some? (dom-t/query env (dom-t/by-attribute "width" "100px"))))

     (is (not-empty (dom-t/query-all env (dom-t/by-attribute "width" (fn [v] (= v "100px")))))))))

(deftest nothing-anything-query-test
  (dom-t/rendering
   (dom/div (dom/span "foo") "bar")
   (fn [env]

     (is (= 4 (count (dom-t/query-all env dom-t/anything))))

     (is (empty? (dom-t/query-all env dom-t/nothing))))))

(deftest all-of-test
  (dom-t/rendering
   (dom/span "foo")
   (fn [env]
     (is (some? (dom-t/query env
                             (dom-t/all-of (dom-t/by-text "foo")
                                           (dom-t/by-text #"f.."))))))))

(deftest any-of-query-test
  (dom-t/rendering
   (dom/div (dom/span "foo") (dom/span "bar"))
   (fn [env]
     (is (= 2 (count (dom-t/query-all env
                                      (dom-t/any-of (dom-t/by-text "foo")
                                                    (dom-t/by-text "bar")))))))))

(deftest subquery-test
  (dom-t/rendering
   (dom/div (dom/span "foo" (dom/span "bar")) (dom/span "bar") (dom/span "foo"))
   (fn [env]
     (is (= 1 (count (dom-t/query-all env (dom-t/within (dom-t/by-text "foo")
                                                        (dom-t/by-text "bar")))))))))


(deftest update!-test
  (dom-t/rendering
   (c/dynamic dom/div)
   :state "foo"
   (fn [env]
     (is (= (.-nodeName (dom-t/get env (dom-t/by-text "foo")))
            "DIV"))
     (dom-t/update! env (c/dynamic dom/span) "bar")
     (is (= (.-nodeName (dom-t/get env (dom-t/by-text "bar")))
            "SPAN")))))

(deftest current-and-set-state-test
  (dom-t/rendering
   (c/dynamic dom/div)
   :state "foo"
   (fn [env]
     (is (= "foo" (dom-t/current-state env)))

     (dom-t/set-state! env "bar")
     (is (= "bar" (dom-t/current-state env)))

     (is (some? (dom-t/get env (dom-t/by-text "bar")))))))

(deftest queue-actions-test-1
  (dom-t/rendering
   (dom/button {:onClick #(c/return :action :x)} "foo")
   :queue-actions? true
   (fn [env]
     (dom-t/fire-event (dom-t/get env (dom-t/by-text "foo")) :click)
        
     (is (= :x (dom-t/pop-action! env)) "returns :x")

     (is (try (dom-t/pop-action! env)
              false
              (catch :default e
                true))
         "throws if empty")
        
     (is (= ::empty (dom-t/pop-action! env ::empty))
         "returns arg if empty"))))

(deftest queue-actions-test-2
  (dom-t/rendering
   (dom/button {:onClick #(c/return :action :x)} "foo")
   (fn [env]
     (dom-t/queue-actions
      env
      (fn []
        (dom-t/fire-event (dom-t/get env (dom-t/by-text "foo")) :click)

        (is (= :x (dom-t/pop-action! env)) "returns :x")))

     ;; (dom-t/fire-event (dom-t/get env (dom-t/by-text "foo")) :click)
     (is (string/starts-with? (try (dom-t/pop-action! env)
                                   ""
                                   (catch :default e
                                     (.-message e)))
                              "Action queueing not active")
         "throws outside of thunk"))))

(deftest send-message-test
  (dom-t/rendering
   (c/handle-message (fn [st msg]
                       msg)
                     (dom/div))
   (fn [env]
     (dom-t/send-message! env :foo)
     (is (= (dom-t/current-state env)
            :foo)))))

(c/defn-effect running-effects-test-effect [x]
  (* x 2))

(deftest running-effects-test
  (let [eff-to-state #(c/dynamic (fn [st]
                                   (dom/div (str st)
                                            (dom/button {:onClick (constantly 1) :title "startup"})
                                            (when-not (zero? st)
                                              (c/execute-effect (running-effects-test-effect 21)
                                                                (fn [_ r] r))))))]
    (async
     (testing "running it"
       (dom-t/rendering
        (eff-to-state)
        :state 1
        (fn [env]
          (is (some? (dom-t/query env (dom-t/by-text "42")))))))
  
     (testing "mocking an effect"
       (with-redefs [running-effects-test-effect (fn [x]
                                                     (c/effect (fn [] (* x 3))))]
         (dom-t/rendering
          (eff-to-state)
          :state 1
          (fn [env]
            (is (some? (dom-t/query env (dom-t/by-text "63"))))))))

     (await
      (testing "mocking an effect, asynchronously"
        (a/with-redefs [running-effects-test-effect (fn [x]
                                                      (c/effect (fn [] (* x 4))))]
          (dom-t/rendering
           (eff-to-state)
           :state 0
           (fn [env]
             (async
              (await (timeout 1))
              (dom-t/fire-event (dom-t/get env (dom-t/by-title "startup"))
                                :click)
              (is (some? (await (dom-t/find env (dom-t/by-text "84")))))))))))
     )))
