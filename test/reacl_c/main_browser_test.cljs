(ns reacl-c.main-browser-test
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [clojure.string :as str]
            [reacl-c.test-util.core :as tu]
            [active.clojure.lens :as lens]
            [schema.core :as s]
            ["react-dom/test-utils" :as react-tu]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

#_(deftest simple-performance-test
    (let [host (js/document.createElement "div")
          run (fn [st]
                (main/run host
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

(deftest app-send-message-test
  (let [e (js/document.createElement "div")
        received (atom nil)
        app (main/run e
              (c/handle-message (fn [state msg]
                                  (reset! received msg)
                                  (c/return))
                                c/empty)
              nil)]
    (main/send-message! app ::hello)
    (is (= ::hello @received))))

(defn renders-as* [item & [state]]
  (let [host (js/document.createElement "div")]
    (main/run host item state)
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
              (react-tu/Simulate.click n))
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
                            (c/refer ref)
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
    (main/run host (c/dynamic str) "foo")
    (is (= "foo" (text (.-firstChild host))))

    (main/run host (c/dynamic str) "bar")
    (is (= "bar" (text (.-firstChild host))))))

(deftest effects-test
  (testing "executing effects"
    (let [executed (atom false)
          eff (c/effect (fn [a]
                          (reset! executed a)
                          nil)
                        true)
          n (renders-as (dom/div (c/init (c/return :action eff))))]
      (is @executed)))

  (testing "handling result"
    (let [eff (c/effect (fn [] "ok"))
          n (renders-as (dom/div (c/dynamic str)
                                 (c/handle-effect-result (fn [st res]
                                                           res)
                                                         eff)))]
      (is (= "ok" (text (.-firstChild n))))))

  (testing "mapping effects, with parrallel effects"
    (let [eff (c/effect (fn [] :foo))
          n (renders-as (dom/div (c/dynamic str)
                                 (-> (c/handle-effect-result (fn [st res]
                                                               ;; first of par-effects result:
                                                               (first res))
                                                             (c/par-effects eff eff))
                                     (c/map-effects {eff (c/const-effect :bar)}))))]
      (is (= ":bar" (text (.-firstChild n))))))

  (testing "state validation"
    (c/defn-effect ^:always-validate effect-test-2 :- s/Int [foo :- s/Keyword]
      "err")
    
    (try (tu/preventing-error-log
          (fn []
            (renders-as (c/once (constantly (c/return :action (effect-test-2 :foo)))))))
         (is false)
         (catch :default e
           (is (= "Output of effect-test-2 does not match schema: \n\n\t [0;33m  (not (integer? \"err\")) [0m \n\n" (.-message e))))))
  )

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

      (react-tu/Simulate.click n)
      (is (= "1" (text (.-firstChild n))))))

  (testing "dom capture events"
    (let [res (atom [])
          n (renders-as (dom/div {:onClick (fn [state ev]
                                             (swap! res conj :clickp)
                                             state)
                                  :onClickCapture (fn [state ev]
                                                    (swap! res conj :capture)
                                                    state)}
                                 (dom/div {:onClick (fn [state ev]
                                                      (swap! res conj :clickc)
                                                      state)}
                                          (c/dynamic pr-str)))
                        [])]
      (react-tu/Simulate.click (.-firstChild n))
      (is (= [:capture :clickc :clickp] @res)))))

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
    (is (= 1 @upd)))

  ;; throws on state changes
  (tu/preventing-error-log
   (fn []
     (try (renders-as (c/static #(c/once (fn [_] (c/return :state "foo")))))
          (is false)
          (catch :default e
            (is true))))))

(deftest messaging-test
  ;; tests: with-ref refer handle-message, also handle-action
  (let [upd (atom 0)
        [x inject!] (injector)
        it (c/with-ref (fn [ref]
                         (dom/div (c/dynamic str)
                                  (-> (c/handle-message (fn [state msg]
                                                          (c/return :state :done))
                                                        c/empty)
                                      (c/refer ref))
                                  (-> x
                                      (c/handle-action (fn [state a]
                                                         (c/return :message [ref a])))))))
        node (renders-as it false)]

    (is (= "false" (text (.-firstChild node))))
    (inject! node (constantly (c/return :action :done)))
    (is (= ":done" (text (.-firstChild node))))))

(deftest with-ref-test
  (is (passes-messages (fn [x] (c/with-ref (constantly x))))))

(deftest refer-test
  (is (passes-messages (fn [x] (c/with-ref (fn [ref] (c/refer x ref)))))))

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

  ;; FIXME - and I think Reacl cannot fix it.
  #_(testing "consistency - never see inconsistent states."
      (let [[x inject!] (injector)
            states (atom [])
            n (renders-as (dom/div (c/local-state 2
                                                  (c/dynamic (fn [st]
                                                               (swap! states conj st)
                                                               x))))
                          4)
            invariant (fn [[a b]]
                        (or (and (odd? a) (odd? b))
                            (and (even? a) (even? b))))]
        (is (= 1 (count @states)))
        (is (every? invariant @states))
        (inject! n [3 5])))

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

;; FIXME:
#_(deftest bubbling-events-test
  ;; state consitency upon a bubbling event.
  (let [last-c1-local (atom nil)
        inner (atom false)
        outer (atom false)
        
        c1 (c/dynamic (fn [state]
                        (reset! last-c1-local state)
                        (dom/div {:onclick (fn [state ev]
                                             (conj state :new-local-2))}
                                 (dom/div {:onclick (fn [state ev]
                                                      (conj state :new-local-1))}))))
        host (js/document.createElement "div")
        cc (main/run host c1 [])]
    
    (let [inner-div (.-firstChild (.-firstChild host))]
      (react-tu/Simulate.click inner-div (js/Event. "click" #js {:bubbles true :cancelable true})))

    (is (= [:new-local-1 :new-local-2] @last-c1-local))))
