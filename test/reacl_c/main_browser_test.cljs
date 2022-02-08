(ns reacl-c.main-browser-test
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [clojure.string :as str]
            [reacl-c.test-util.core :as tu]
            [active.clojure.lens :as lens]
            [schema.core :as s :include-macros true]
            [active.clojure.functions :as f]
            ["react-dom/test-utils" :as react-tu]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

;; some things don't work in karma/compiled mode of shadow; TODO: any way to find out?
(def karma? true)

(deftest app-send-message-test
  (let [e (js/document.createElement "div")
        received (atom nil)
        app (main/run e
              (c/handle-message (fn [state msg]
                                  (reset! received msg)
                                  (c/return))
                                c/empty))]
    (main/send-message! app ::hello)
    (is (= ::hello @received))))

(defn run-in [host item & [state]]
  (main/run host item {:initial-state state}))

(defn render [item & [state]]
  (let [host (js/document.createElement "div")]
    [(run-in host item state)
     host]))

(defn renders-as* [item & [state]]
  (let [[app host] (render item state)]
    (array-seq (.-childNodes host))))

(defn renders-as [item & [state]]
  (first (renders-as* item state)))

(defn capture-last-state [& [initial]]
  (let [last-state (atom initial)
        it (c/dynamic (fn [st]
                        (reset! last-state st)
                        c/empty))]
    [it last-state]))

(defn changes-state [item & [initial-state]]
  (let [[it at] (capture-last-state initial-state)
        n (renders-as (c/fragment
                       item
                       it)
                      initial-state)]
    @at))

(defn emits-actions [item & [initial-state]]
  (second (changes-state (c/handle-action (c/focus lens/first item)
                                          (fn [st a]
                                            [(first st) (conj (second st) a)]))
                         [initial-state []])))

(defn injector
  "Returns an item and a function that, when called with a host dom node
  in which the item is rendered and a function f, calls f as an event
  handler function `(f state)`."
  []
  (let [ret (atom nil)
        class (str (gensym "injector"))
        item (dom/button {:class class
                          :onclick (fn [state ev]
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
  (.-textContent node))

(defn passes-messages
  "Calls f with an item and returns if the returned item passes messages sent to it down to that item."
  [f]
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

(defn passes-actions
  "Calls f with an item and returns if an action emitted by that item is passed up by the returned item."
  [f]
  (let [[x inject!] (injector)
        it (c/with-state-as st
             (dom/div st (-> (f x)
                             (c/handle-action (fn [_ a]
                                                (c/return :state a))))))
        node (renders-as it "start")]

    (inject! node (constantly (c/return :action "ok")))
    (= "ok" (text (.-firstChild node))))
  )

(defn throws-like? [thunk message]
  (tu/preventing-error-log
   (fn []
     (try
       (thunk)
       false
       (catch :default e
         (str/starts-with? (.-message e) message))))))

;; --------------------

(deftest run-test
  (let [host (js/document.createElement "div")]
    (main/run host (c/dynamic str) {:initial-state "foo"})
    (is (= "foo" (text (.-firstChild host))))

    (main/run host (c/dynamic str) {:initial-state "bar"})
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
    (let [eff (c/effect (fn [] "ok"))]
      (is (= "ok"
             (changes-state (c/handle-effect-result (fn [st res]
                                                      res)
                                                    eff))))))

  (testing "mapping effects, with parrallel effects"
    (let [eff (c/effect (fn [] :foo))]
      (is (= [:bar :bar]
             (changes-state (-> (c/handle-effect-result (fn [st res]
                                                          ;; both par-effects results as new state, replaced by :bar
                                                          res)
                                                        (c/par-effects eff eff))
                                (c/map-effects {eff (c/const-effect :bar)})))))))

  ;; does not work in karma; not sure why
  (when-not karma?
    (testing "state validation"
      (c/defn-effect ^:always-validate effect-test-2 :- s/Int [foo :- s/Keyword]
        "err")

      (is (throws-like? (fn []
                          (renders-as (c/once (constantly (c/return :action (effect-test-2 :foo))))))
                        "Output of effect-test-2 does not match schema: \n\n\t [0;33m  (not (integer? \"err\")) [0m \n\n")))))

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
  ;; Note: for some reason the exception is not catchable like this in karma/compiled mode; not sure why.
  (when-not karma?
    (is (throws-like? (fn []
                        (renders-as (c/static #(c/once (fn [_] (c/return :state "foo"))))))
                      ""))))

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
  (is (passes-messages (fn [x] (c/handle-action x (fn [st a] (c/return :action a))))))

  (is (= :bar (changes-state (c/handle-action (c/init (c/return :action :bar))
                                              (fn [st a]
                                                a))))))

(deftest try-catch-test
  (let [[x inject!] (injector)

        it (dom/div x
                    (c/try-catch (c/dynamic (fn [state]
                                              (if (:throw? state)
                                                (throw (ex-info "Test" {:value :foo}))
                                                "Ok")))
                                 (c/dynamic (fn [[state error]]
                                              (c/fragment "Handled " (pr-str error)
                                                          (if (:reset? state)
                                                            (c/init (c/return :state [{:throw? false} nil]))
                                                            c/empty))))))

        host (renders-as it {:throw? false})]
    
    (is (= "Ok" (text host)))

    (tu/preventing-error-log
     #(inject! host (f/constantly {:throw? true})))
    
    (is (= "Handled #error {:message \"Test\", :data {:value :foo}}"
           (text host)))

    (inject! host (f/constantly {:throw? true :reset? true}))
    (is (= "Ok" (text host)))))


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

  (testing "consistency - never see inconsistent states."
    ;; the state of 'dynamic' will be [4 2] first, then, atomically, [3 5]; no mix of inner and outer states.
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
      (inject! n (constantly [3 5]))
      (is (every? invariant @states))))

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

    ;; has an effect on React debugging utils. probably not testable?

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

  ;; does not work in compiled/karma mode - don't know why.
  (when-not karma?
    (tu/preventing-error-log
     (fn []
       (let [err (ex-info "err" {})]
         (is (= {:throw? false
                 :error err}
                (changes-state (-> (c/dynamic (fn [st]
                                                (if (:throw? st)
                                                  (throw err)
                                                  c/empty)))
                                   (c/handle-error (fn [state error]
                                                     (assoc state
                                                            :throw? false
                                                            :error error))))))))))))

(deftest bubbling-events-test
  ;; state consitency upon a bubbling event.
  ;; Note: work with the React implementation, not with the Reacl implementation (https://github.com/active-group/reacl/issues/47).
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
        cc (main/run host c1 {:initial-state []})]
    
    (let [inner-div (.-firstChild (.-firstChild host))]
      (react-tu/Simulate.click inner-div (js/Event. "click" #js {:bubbles true :cancelable true})))

    (is (= [:new-local-1 :new-local-2] @last-c1-local))))

(deftest subscription-test
  (let [subscribed (atom nil)
        sub-impl (fn [deliver! x]
                   (assert (= x :x))
                   (reset! subscribed deliver!)
                   (deliver! :sync)
                   (fn stop-fn []
                     (reset! subscribed nil)))
        sub (c/subscription sub-impl :x)]

    (testing "emits a sync action"
      (is (= [:sync] (emits-actions sub))))
    
    (testing "emits async actions"
      (reset! subscribed nil)
      (let [[x last-state] (capture-last-state)]
        (render (c/handle-action (c/fragment sub x)
                                 conj)
                [])
        (is (= [:sync] @last-state))
        (@subscribed :async) ;; @subscribed = deliver! fn.
        (is (= [:sync :async] @last-state))))

    (testing "unsub on unmount"
      (reset! subscribed nil)
      (let [[app host] (render (c/handle-action sub
                                                conj)
                               [])]
        (run-in host (dom/div))
        (is (not @subscribed))))

    (testing "synchronous unsub"
      ;; subscribing and unsubscribing in the same update cycle... tricky.
      (reset! subscribed nil)
      (render (c/handle-action (c/dynamic (fn [sub?]
                                            (if sub? sub c/empty)))
                               (fn [st a]
                                 false))
              true)
      (is (not @subscribed))))

  )

(deftest defn-subscription-test
  (c/defn-subscription defn-subscription-test-1 ^:always-validate deliver! :- s/Keyword [arg]
    (deliver! arg)
    (fn [] nil))

  (testing "schema validation in deliver!"
    (is (throws-like? #(renders-as (defn-subscription-test-1 "foo"))
                      "Input to deliver! does not match schema:"))))

(deftest once-test
  (is (= [:init :cleanup]
         (emits-actions (c/dynamic (fn [st]
                                     (if st
                                       (c/once (constantly (c/return :action :init
                                                                     :state false))
                                               (constantly (c/return :action :cleanup)))
                                       c/empty)))
                        true)))
  (is (= [true]
         (emits-actions (c/dynamic (fn [st]
                                     (if st
                                       (c/fragment (c/once (partial c/return :action))
                                                   (c/once not))
                                       c/empty)))
                        true)))
  (is (= [:cleanup]
         (emits-actions (c/dynamic (fn [st]
                                     (if st
                                       (c/once (constantly false)
                                               (constantly (c/return :action :cleanup)))
                                       c/empty)))
                        true)))
  (is (= [:init]
         (emits-actions (c/dynamic (fn [st]
                                     (c/fragment (c/once (constantly (c/return :action :init)))
                                                 (c/once (constantly 1)))))
                        0)))

  (is (= 1 (changes-state (c/once (constantly 1)))))
  (is (= nil (changes-state (c/once (constantly nil)) :init))))

(deftest with-state-as-test
  (testing "state binding"
    (is (= (text (renders-as (c/with-state-as foo foo) "Ok"))
           "Ok"))

    (is (= (text (renders-as (c/with-state-as foo :- s/Str foo) "Ok"))
           "Ok"))

    (is (= (text (renders-as (c/with-state-as [a b] (dom/div a b)) ["foo" "bar"]))
           "foobar"))

    (is (= (text (renders-as (c/with-state-as [a b :local "bar"] (dom/div a b)) "foo"))
           "foobar")))

  (testing "schema validation"
    (s/with-fn-validation
      (is (throws-like? #(renders-as (c/with-state-as foo :- s/Str foo) 42)
                        "Input to fn")))))

(deftest with-bind-test
  (let [n (renders-as (c/with-bind
                        (fn [bind]
                          (let [h (bind (fn [state _]
                                          (update state :counter inc)))]
                            (c/focus :counter
                                     (dom/button {:onclick h}
                                                 (c/dynamic str))))))
                      {:counter 0})]
    (is (= "0" (text (.-firstChild n))))

    (react-tu/Simulate.click n)
    (is (= "1" (text (.-firstChild n))))))
