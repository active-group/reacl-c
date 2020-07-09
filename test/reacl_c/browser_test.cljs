(ns reacl-c.browser-test
  (:require [reacl-c.browser :as browser]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [clojure.string :as str]
            [reacl-c.test-util.core :as tu]
            [active.clojure.lens :as lens]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as rdom]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

#_(deftest simple-performance-test
  (let [host (js/document.createElement "div")
        run (fn [st]
              (browser/run host
                (dom/div {:id "a"}
                         (apply dom/div (repeat 20 (dom/span)))
                         (c/fragment (apply dom/span (repeat 15 "x"))))
                st))
        tupd (fn [n]
               (let [st (js/window.performance.now)]
                 (doseq [i (range n)]
                   (run  i))
                 (- (js/window.performance.now) st)))]
    (run 0)
    (is (= nil
           [(tupd 100) (tupd 1000) (tupd 10000)]))
    
    ;; 1.9 reacl [152.27999998023733 1182.5650000246242 8815.754999988712]
    ;; 1.9 react [68.25999997090548 350.3499999642372 2993.8100000144914]

    )
  )


(defn renders-as* [item & [state]]
  (let [host (js/document.createElement "div")]
    (browser/run host item state)
    (array-seq (.-childNodes host))))

(defn renders-as [item & [state]]
  (first (renders-as* item state)))

(defn injector []
  (let [ret (atom nil)
        class (str (gensym "injector"))
        item (dom/button {:class class
                          :onClick (fn [state ev]
                                     (@ret state))})]
    [item (fn [host f]
            (reset! ret f)
            (let [n (first (array-seq (.getElementsByClassName host class)))]
              (assert (some? n) "injector item not found")
              (js/ReactTestUtils.Simulate.click n))
            (reset! ret nil))]))

(defn tag [node]
  (str/lower-case (.-tagName node)))

(defn text [node]
  (.-nodeValue node))

(defn passes-messages [f]
  (let [[x inject!] (injector)
        it (c/with-ref
             (fn [ref]
               (dom/div (c/fragment
                         (c/dynamic str)
                         (-> x
                             (c/handle-action (fn [_ a]
                                                (c/return :message [ref (:res a)])))))
                        (-> (f (c/handle-message (fn [state msg]
                                                   (c/return :action msg))
                                                 c/empty))
                            (c/set-ref ref)
                            (c/handle-action (fn [_ a]
                                               (c/return :state a)))))))
        node (renders-as it "start")]

    (inject! node (constantly (c/return :action {:res "ok"})))
    (= "ok" (text (.-firstChild node)))))

(defn passes-actions [f]
  (let [[x inject!] (injector)
        it (c/with-state-as st
             (dom/div st (-> (f x)
                             (c/handle-action (fn [_ a]
                                                (c/return :state a))))))
        node (renders-as it "start")]

    (inject! node (constantly (c/return :action "ok")))
    (= "ok" (text (.-firstChild node))))
  )

;; --------------------

(deftest run-test
  (let [host (js/document.createElement "div")]
    (browser/run host (c/dynamic str) "foo")
    (is (= "foo" (text (.-firstChild host))))

    (browser/run host (c/dynamic str) "bar")
    (is (= "bar" (text (.-firstChild host))))))

(deftest running-effects-test
  (let [eff (c/effect (fn [a]
                        (:res a))
                      {:res "ok"})
        n (renders-as (dom/div (c/dynamic str)
                               (c/handle-effect-result (fn [st res]
                                                         res)
                                                       eff)))]
    (is (= "ok" (text (.-firstChild n))))))

(deftest dom-test
  (is (passes-actions dom/div))
  
  (testing "simple dom"
    (let [n (renders-as (dom/div {:style {:border "1px solid black"}}
                                 (dom/span)))]
      (is (= "div" (tag n)))
      (is (= "1px solid black" (.-border (.-style n))))
      (is (= "span" (tag (.-firstChild n))))))

  (testing "dom events"
    (let [n (renders-as (dom/button {:onClick (fn [state ev]
                                                (inc state))}
                                    (c/dynamic str))
                        0)]
      (is (= "0" (text (.-firstChild n))))

      (js/ReactTestUtils.Simulate.click n)
      (is (= "1" (text (.-firstChild n)))))))

(deftest fragment-test
  (is (passes-actions c/fragment))
  
  (testing "empty fragment"
    (is (empty? (renders-as* (c/fragment)))))
  (testing "non empty fragment"
    (is (= "span" (tag (renders-as (dom/span)))))))

(deftest dynamic-test
  (is (passes-actions (fn [x] (c/dynamic (constantly x)))))
  
  (is (passes-messages (fn [x] (c/dynamic (constantly x)))))
  
  (let [it (c/dynamic (fn [st v] (if st (dom/div v) (dom/span v)))
                      "foo")]
    (is (= "div" (tag (renders-as it true))))
    (is (= "foo" (text (.-firstChild (renders-as it true)))))
    (is (= "span" (tag (renders-as it false))))))

(deftest static-test
  (is (passes-actions (fn [x] (c/static (constantly x)))))
  
  (is (passes-messages (fn [x] (c/static (constantly x)))))
  
  (let [n (renders-as (c/static (fn []
                                  (c/dynamic pr-str))) :foo)]
    (is (= "nil" (text n))))
  
  (let [upd (atom 0)
        [x inject!] (injector)
        it (dom/span (c/static (fn [] (swap! upd inc) (dom/div)))
                     x)
        node (renders-as it true)]

    (is (= "div" (tag (.-firstChild node))))
    (is (= 1 @upd))

    ;; does not render again on state change:
    (inject! node (constantly false))
    (is (= 1 @upd))))

(deftest messaging-test
  ;; tests: with-ref set-ref handle-message, also handle-action
  (let [upd (atom 0)
        [x inject!] (injector)
        it (c/with-ref (fn [ref]
                         (dom/div (c/dynamic str)
                                  (-> (c/handle-message (fn [state msg]
                                                          (c/return :state :done))
                                                        c/empty)
                                      (c/set-ref ref))
                                  (-> x
                                      (c/handle-action (fn [state a]
                                                         (c/return :message [ref a])))))))
        node (renders-as it false)]

    (is (= "false" (text (.-firstChild node))))
    (inject! node (constantly (c/return :action :done)))
    (is (= ":done" (text (.-firstChild node))))))

(deftest with-ref-test
  (is (passes-messages (fn [x] (c/with-ref (constantly x))))))

(deftest set-ref-test
  (is (passes-messages (fn [x] (c/with-ref (fn [ref] (c/set-ref x ref)))))))

(deftest handle-action-test
  (is (passes-messages (fn [x] (c/handle-action x (fn [st a] (c/return :action a)))))))

(deftest with-async-return-test
  (is (passes-messages (fn [x] (c/with-async-return (constantly x)))))
  
  (async done
         (let [n (renders-as (dom/div (c/dynamic str)
                                      (c/with-async-return (fn [return!]
                                                             (js/window.setTimeout #(return! (c/return :state :done)) 0)
                                                             c/empty)))
                             :start)]
           (is (= ":start" (text (.-firstChild n))))
           (js/window.setTimeout (fn []
                                   (is (= ":done" (text (.-firstChild n))))
                                   (done))
                                 1))))

(deftest focus-test
  (let [id (fn ([x] x) ([_ x] x))]
    (is (passes-messages (fn [x] (c/focus id x)))))
  
  (is (= "foo" (text (renders-as (c/focus :a (c/dynamic str))
                                {:a "foo"})))))

(deftest local-state-test
  (is (passes-messages (fn [x] (c/local-state nil (c/focus lens/first x)))))
  
  (testing "basic"
    (is (= (pr-str [:b :a]) (text (renders-as (c/local-state :a (c/dynamic pr-str))
                                              :b)))))
  (testing "updates"
    (let [[x inject!] (injector)
          n (renders-as (dom/div (c/local-state :a
                                                (c/fragment (c/dynamic pr-str) x)))
                        :b)]
      (is (= (pr-str [:b :a]) (text (.-firstChild n))))
      
      (inject! n (constantly [:b :c]))
      (is (= (pr-str [:b :c]) (text (.-firstChild n))))

      (inject! n (constantly [:d :c]))
      (is (= (pr-str [:d :c]) (text (.-firstChild n))))))

  (testing "reinit"
    (let [[x inject!] (injector)
          n (renders-as (dom/div (c/with-state-as st
                                   (c/fragment
                                    (c/local-state st (c/dynamic pr-str))
                                    x)))
                        :b)]
      (is (= (pr-str [:b :b]) (text (.-firstChild n))))
      
      (inject! n (constantly :c))
      (is (= (pr-str [:c :c]) (text (.-firstChild n)))))))

(deftest handle-state-change-test
  (is (passes-messages (fn [x] (c/handle-state-change x (fn [old new] new)))))
  
  (let [[x inject!] (injector)
        n (renders-as (dom/div (-> (c/fragment
                                    (c/dynamic str)
                                    x)
                                   (c/handle-state-change (fn [old new]
                                                            (+ old new)))))
                      1)]
    (is (= "1" (text (.-firstChild n))))
      
    (inject! n (constantly 3))
    (is (= "4" (text (.-firstChild n))))))

(deftest named-test
  (let [name-id (c/name-id "foo")]
    (is (passes-messages (fn [x] (c/named name-id x))))
    
    (is (= "div" (tag (renders-as (c/named name-id (dom/div))))))

    ;; has an effect on React debugging utils. TODO: is that testable?

    ))

(deftest keyed-test
  (is (passes-messages (fn [x] (c/keyed x :foo))))
  
  (let [[x inject-x!] (injector)
        [y inject-y!] (injector)
        n (renders-as (dom/div (c/with-state-as st
                                 (let [l (-> (c/local-state :a
                                                            (if st
                                                              (c/fragment (c/dynamic pr-str) x)
                                                              (c/fragment x (c/dynamic pr-str))))
                                             (c/keyed :foo))]
                                   (if st
                                     (c/fragment l (dom/span) y)
                                     (c/fragment y (dom/span) l)))))
                      false)]
    (is (= (pr-str [false :a]) (text (.-lastChild n))))
      
    (inject-x! n (constantly [false :b]))
    (inject-y! n (constantly true))
    (is (= (pr-str [true :b]) (text (.-firstChild n))))))

(deftest lifecycle-test
  (let [[x inject-x!] (injector)
        n (renders-as (dom/div (c/with-state-as st
                                 (c/fragment
                                  (c/dynamic pr-str)
                                  x
                                  (if st
                                    (c/lifecycle (fn [state] :init)
                                                 (fn [state] nil))
                                    c/empty))))
                      false)]
    (is (= "false" (text (.-firstChild n))))
    (inject-x! n (constantly true))
    (is (= ":init" (text (.-firstChild n))))
    
    (inject-x! n (constantly false))
    (is (= "nil" (text (.-firstChild n))))))

(deftest handle-error-test
  (is (passes-actions (fn [x] (c/handle-error x (fn [_ _] nil)))))
  
  (is (passes-messages (fn [x] (c/handle-error x (fn [_ _] nil)))))
  
  (tu/preventing-error-log
   (fn []
     (let [[x inject-x!] (injector)
           err (ex-info "err" {})
           n (renders-as (dom/div (c/fragment
                                   (-> (c/dynamic (fn [st]
                                                    (if (:throw? st)
                                                      (throw err)
                                                      c/empty)))
                                       (c/handle-error (fn [state error]
                                                         (assoc state
                                                                :throw? false
                                                                :error error))))
                                   (c/dynamic pr-str)
                                   x))
                         {:throw? false})]
       (is (= (pr-str {:throw? false}) (text (.-firstChild n))))

       (inject-x! n #(assoc % :throw? true))
       
       (is (= (pr-str {:throw? false
                       :error err}) (text (.-firstChild n))))
       ))))

(deftest lift-reacl-test
  (testing "syncs state"
    (let [it (browser/lift-reacl (reacl/class "foo" this state []
                                              render (rdom/div (str state))))]
      (is (= "ok1" (text (.-firstChild (renders-as it "ok1"))))))
    
    (let [it (browser/lift-reacl (reacl/class "foo" this state []
                                              component-did-mount (fn [] (reacl/return :app-state "ok11"))
                                              render (rdom/div (str state))))]
      (is (= "ok11" (text (.-firstChild (renders-as it "start")))))))
  
  (testing "emits actions"
    (let [it (browser/lift-reacl (reacl/class "foo" this state [v]
                                              component-did-mount (fn [] (reacl/return :action v))
                                              render (rdom/div (str state)))
                                 "ok2")]
      (is (= "ok2" (text (.-firstChild (renders-as (-> it
                                                       (c/handle-action (fn [state a] a)))
                                                   "start")))))))
  (testing "forwards messages"
    (let [it (browser/lift-reacl (reacl/class "foo3" this state []
                                              handle-message (fn [msg]
                                                               (reacl/return :app-state msg))
                                              render (rdom/span)))
          [x inject!] (injector)
          n (renders-as (dom/div (c/with-ref (fn [ref]
                                               (c/fragment (c/dynamic dom/div)
                                                           (c/set-ref it ref)
                                                           (c/handle-action x (fn [state a]
                                                                                (c/return :message [ref a])))))))
                        "start")]
      (inject! n (constantly (c/return :action "ok3")))
      (is (= "ok3" (text (.-firstChild (.-firstChild n))))))))

(deftest reacl-render-test
  (let [node (js/document.createElement "div")
        wr (reacl/class "foo" this state []
                        render (browser/reacl-render (reacl/bind this)
                                                     (dom/div (c/dynamic str))))]
    (reacl/render-component node wr "ok1")
    (is (= "ok1" (text (.-firstChild (.-firstChild node))))))

  (let [node (js/document.createElement "div")
        [x inject!] (injector)
        wr (reacl/class "foo" this state []
                        render (rdom/fragment (rdom/div state)
                                              (browser/reacl-render (reacl/bind this)
                                                                    x)))]
    (reacl/render-component node wr "start")
    (inject! node (constantly (c/return :state "ok2")))
    (is (= "ok2" (text (.-firstChild (.-firstChild node)))))))

(deftest full-reacl-test
  ;; render and lift also work in combination.
  (is (passes-actions (fn [x]
                        (browser/lift-reacl (reacl/class "foo" this state []
                                                         render (browser/reacl-render (reacl/bind this) x))))))
  (is (passes-messages (fn [x]
                         (browser/lift-reacl (reacl/class "foo" this state []
                                                          refs [child]
                                                          handle-message (fn [msg] (reacl/return :message [(reacl/get-dom child) msg]))
                                                          render (-> (browser/reacl-render (reacl/bind this) x)
                                                                     (reacl/refer child)))))))
  )
