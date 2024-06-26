(ns reacl-c.main-browser-test
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [clojure.string :as str]
            [reacl-c.test-util.core :as tu]
            [active.clojure.lens :as lens]
            [schema.core :as s :include-macros true]
            [active.clojure.functions :as f]
            ["react-dom/test-utils" :as react-tu]
            [reacl-c.dom-testing :as dt]
            [cljs-async.core :as async :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

;; catching and testing for error doesn't work properly in karma/compiled mode of shadow
(def karma? (not= (aget js/window "__karma__") js/undefined))

(defn tag [node]
  (str/lower-case (.-tagName node)))

(defn text [node]
  (.-textContent node))

(deftest app-send-message-test
  (let [received (atom nil)]
    (dt/rendering
     (c/handle-message (fn [state msg]
                         (reset! received msg)
                         (c/return))
                       c/empty)
     (fn [env]
       (main/send-message! (dt/app env) ::hello)
       (is (= ::hello @received))))))

(defn changes-state [item & [initial-state]]
  (let [state (atom initial-state)]
    (dt/rendering
     (c/fragment item
                 (c/dynamic (fn [st]
                              (reset! state st)
                              (c/fragment))))
     :state initial-state
     (fn [env]
       @state))))

(defn check-node [f]
  (fn [env]
    (f (.-firstChild (dt/container env)))))

(defn rendered-text [item & [state]]
  (dt/rendering
   item
   :state state
   (check-node text)))

(defn emits-actions [item & [initial-state]]
  (second (changes-state (c/handle-action (c/focus lens/first item)
                                          (fn [st a]
                                            [(first st) (conj (second st) a)]))
                         [initial-state []])))

(defn passes-messages
  "Calls f with an item and returns if the returned item passes messages sent to it down to that item."
  [f]
  (let [received (atom nil)
        test-item (c/handle-message (fn [state msg]
                                      (reset! received msg)
                                      (c/return))
                                    (c/fragment))]
    (dt/rendering
     (f test-item)
     (fn [env]
       (dt/send-message! env ::test-message)
       (= ::test-message @received)))))

(defn passes-actions
  "Calls f with an item and returns if an action emitted by that item is passed up by the returned item."
  [f]
  (let [received (atom nil)
        test-item (c/init (c/return :action ::test-action))]
    (dt/rendering
     (c/handle-action (f test-item)
                      (fn [_ a]
                        (if (= ::test-action a)
                          (do (reset! received a)
                              (c/return))
                          (c/return :action a))))
     (fn [env]
       (= ::test-action @received)))))

(defn capture-last-state [& [initial]]
  (let [last-state (atom initial)
        it (c/dynamic (fn [st]
                        (reset! last-state st)
                        c/empty))]
    [it last-state]))

(defn capture-last-state-of [item & [initial]]
  (let [[it at] (capture-last-state initial)]
    [(c/fragment it item)
     at]))

(defn injector
  "Returns an item and a function that, when called with a host dom node
  in which the item is rendered and a function f, calls f as an event
  handler function `(f state)`."
  []
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
  ;; renders item, and can reuse same container
  (let [host (js/document.createElement "div")]
    (dt/rendering
     (c/dynamic str)
     :state "foo"
     :container host
     (fn [env]
       (is (= "foo" (text (.-firstChild host))))))

    (dt/rendering
     (c/dynamic str)
     :state "bar"
     :container host
     (fn [env]
       (is (= "bar" (text (.-firstChild host))))))))

(deftest effect-test
  (testing "executing effects"
    (let [executed (atom false)
          eff (c/effect (fn [a]
                          (reset! executed a)
                          nil)
                        true)]
      (dt/rendering
       (dom/div (c/init (c/return :action eff)))
       (fn [env]
         (is @executed)))))

  (testing "executing recursive effects"
    (let [executed (atom false)
          eff2 (c/effect (fn [a]
                           (reset! executed a)
                           nil)
                         true)
          eff1 (c/effect (fn []
                           (c/return :action eff2)))]
      (dt/rendering
       (dom/div (c/init (c/return :action eff1)))
       (fn [env]
         (is @executed)))))

  (testing "effects returning actions"
    ;; Note: these cannot be handled by handle-action, but by the handle-action! option of main/run
    (let [received (atom nil)
          eff (c/effect (fn []
                          (c/return :action :foo)))]
      (dt/rendering
       (dom/div (c/init (c/return :action eff)))
       :handle-action! (fn handle-action [a]
                         (reset! received a))
       (fn [env]
         (is (= :foo @received))))))
  
  (testing "handling result"
    (let [eff (c/effect (fn [] "ok"))]
      (is (= "ok"
             (changes-state (c/execute-effect eff
                                              (fn [st res]
                                                res)))))))

  (testing "mapping effects, with parrallel effects"
    (let [eff (c/effect (fn [] :foo))]
      (is (= [:bar :bar]
             (changes-state (-> (c/execute-effect (c/par-effects eff eff)
                                                  (fn [st res]
                                                    ;; both par-effects results as new state, replaced by :bar
                                                    res))
                                (c/map-effects {eff (c/const-effect :bar)})))))))

  ;; does not work in karma; not sure why
  (when-not karma?
    (testing "state validation"
      (c/defn-effect ^:always-validate effect-test-2 :- s/Int [foo :- s/Keyword]
        "err")

      (is (throws-like? (fn []
                          (dt/rendering (c/once (constantly (c/return :action (effect-test-2 :foo))))
                                        (fn [env] nil)))
                        "Output of effect-test-2 does not match schema: \n\n\t [0;33m  (not (integer? \"err\")) [0m \n\n")))))

(deftest dom-test
  (is (passes-actions dom/div))
  
  (testing "simple dom"
    (dt/rendering
     (dom/div {:style {:border "1px solid black"}}
              (dom/span))
     (check-node
      (fn [n]
        (is (= "div" (tag n)))
        (is (= "1px solid black" (.-border (.-style n))))
        (is (= "span" (tag (.-firstChild n))))))))

  (testing "dom events"
    (dt/rendering
     (dom/button {:onClick (fn [state ev]
                             (inc state))}
                 (c/dynamic str))
     :state 0
     (check-node
      (fn [n]
        (is (= "0" (text (.-firstChild n))))

        (react-tu/Simulate.click n)
        (is (= "1" (text (.-firstChild n))))))))

  (testing "dom capture events"
    (let [res (atom [])]
      (dt/rendering
       (dom/div {:onClick (fn [state ev]
                            (swap! res conj :clickp)
                            state)
                 :onClickCapture (fn [state ev]
                                   (swap! res conj :capture)
                                   state)}
                (dom/div {:onClick (fn [state ev]
                                     (swap! res conj :clickc)
                                     state)}
                         (c/dynamic pr-str)))
       :state []
       (check-node
        (fn [n]
          (react-tu/Simulate.click (.-firstChild n))
          (is (= [:capture :clickc :clickp] @res))))))))

(deftest defn-dom-test
  (dom/defn-dom defn-dom-test-1 [attrs foo] (dom/div attrs "bar" foo))

  (testing "rendering"
    (is (= "barbaz" (rendered-text (defn-dom-test-1 "baz"))))))

(deftest fragment-test
  (is (passes-actions c/fragment))
  
  (testing "empty fragment"
    (dt/rendering
     (c/fragment)
     (check-node
      (fn [n]
        (is (nil? n))))))
  
  (testing "non empty fragment"
    (dt/rendering
     (c/fragment (dom/span))
     (check-node
      (fn [n]
        (is (= "span" (tag n))))))))

(deftest dynamic-test
  (is (passes-actions (fn [x] (c/dynamic (constantly x)))))
  
  (is (passes-messages (fn [x] (c/dynamic (constantly x)))))
  
  (let [it (c/dynamic (fn [st v] (if st (dom/div v) (dom/span v)))
                      "foo")]
    (dt/rendering
     it
     :state true
     (check-node (fn [n]
                   (is (= "div" (tag n)))
                   (is (= "foo" (text (.-firstChild n)))))))
    (dt/rendering
     it
     :state false
     (check-node (fn [n]
                   (is (= "span" (tag n))))))))

(deftest static-test
  (is (passes-actions (fn [x] (c/static (constantly x)))))
  
  (is (passes-messages (fn [x] (c/static (constantly x)))))

  (dt/rendering
   (c/static (fn []
               (c/dynamic pr-str)))
   :state :foo
   (check-node
    (fn [n]
      (is (= "nil" (text n))))))

  (c/defn-item static-test-1 [atom]
    (swap! atom inc)
    (dom/div))

  (let [upd (atom 0)
        [x inject!] (injector)]
    (dt/rendering
     (dom/span (c/static (fn [] (static-test-1 upd)))
               x)
     :state true
     (check-node
      (fn [node]
        (is (= "div" (tag (.-firstChild node))))
        (is (= 1 @upd))

        ;; does not render again on state change:
        (inject! node (constantly false))
        (is (= 1 @upd))
        ))))

  (testing "regression with once"
    (let [v (atom [])]
      (dt/rendering
       (c/static (fn [st]
                   (swap! v conj st)
                   nil))
       :state :bar
       (fn [env]
         (is (= #{nil} (set @v))))))
    (let [v (atom [])]
      (dt/rendering
       (c/static #(c/once (fn [st]
                            (swap! v conj st)
                            (c/return))))
       :state :bar
       (fn [env]
         (is (= #{nil} (set @v)))))))

  ;; throws on state changes
  ;; Note: for some reason the exception is not catchable like this in karma/compiled mode; not sure why.
  (when-not karma?
    (is (throws-like? (fn []
                        (dt/rendering
                         (c/static #(c/once (fn [st]
                                              (c/return :state "foo"))))
                         :state :bar
                         (fn [env] nil)))
                      "Assert failed: Tried to put a value"))))

(deftest messaging-test
  ;; tests: with-ref refer handle-message, also handle-action
  (let [upd (atom 0)
        [x inject!] (injector)]
    (dt/rendering
     (c/with-ref (fn [ref]
                   (dom/div (c/dynamic str)
                            (-> (c/handle-message (fn [state msg]
                                                    (c/return :state :done))
                                                  c/empty)
                                (c/refer ref))
                            (-> x
                                (c/handle-action (fn [state a]
                                                   (c/return :message [ref a])))))))
     :state false
     (check-node
      (fn [node]
        (is (= "false" (text node)))
        (inject! node (constantly (c/return :action :done)))
        (is (= ":done" (text node))))))))

(deftest redirect-messages-test
  (let [[x at] (capture-last-state)]
    (dt/rendering
     (c/with-ref
       (fn [ref]
         (c/redirect-messages ref
                              (c/fragment "foo"
                                          (-> (c/handle-message (fn [state msg]
                                                                  (c/return :state msg))
                                                                x)
                                              (c/refer ref))))))
     (fn [env]
       (dt/send-message! env :msg)
       (is (= :msg @at))))))

(deftest ref-let-test
  (let [[x at] (capture-last-state)]
    (dt/rendering
     (c/ref-let [it-1 (c/handle-message (fn [state msg]
                                          (c/return :state [:it-1 msg]))
                                        c/empty)
                 it-2 (c/handle-message (fn [state msg]
                                          (c/return :state [:it-2 msg]))
                                        c/empty)]
                (c/handle-message
                 (fn [state msg]
                   (if (odd? msg)
                     (c/return :message [it-1 msg])
                     (c/return :message [it-2 msg])))
                 (c/fragment x it-1 it-2)))
     (fn [env]
       (dt/send-message! env 1)
       (is (= [:it-1 1] @at))

       (dt/send-message! env 2)
       (is (= [:it-2 2] @at))))))

(deftest with-ref-test
  (is (passes-messages (fn [x] (c/with-ref (constantly x)))))

  ;; important for with-ref, must be the same ref for a unique item, even if the state changes.
  (let [last-ref (atom nil)
        [x inject!] (injector)]
    (dt/rendering
     (c/with-ref (fn [ref]
                   (reset! last-ref ref)
                   (dom/div x)))
     :state true
     (fn [env]
       (let [r1 @last-ref]
         (is (some? r1))
      
         (inject! (dt/container env) (constantly false))
         (is (= r1 @last-ref)))))))

(deftest refer-test
  (is (passes-messages (fn [x] (c/with-ref (fn [ref] (c/refer x ref))))))

  (is (passes-messages (fn [x] (c/with-refs 2 (fn [[ref1 ref2]] (c/refer (c/refer x ref1) ref2)))))))

(deftest handle-action-test
  (is (passes-messages (fn [x] (c/handle-action x (fn [st a] (c/return :action a))))))

  (is (= :bar (changes-state (c/handle-action (c/init (c/return :action :bar))
                                              (fn [st a]
                                                a)))))

  ;; state changes are processes before actions / action handlers see new state
  (is (= [[:start-state :new-state] :other-action]
         (changes-state (c/handle-action (c/local-state :foo
                                                        (c/handle-action (c/init (c/return :state [:start-state :foo]
                                                                                           :action :bar))
                                                                         (fn [st a]
                                                                           (c/return :state [[(first st) :new-state] (second st)]
                                                                                     :action :other-action))))
                                         (fn [st a]
                                           (c/return :state [st a])))))))

(deftest try-catch-test
  (is (passes-actions (fn [x] (c/try-catch x c/empty))))
  
  (is (passes-messages (fn [x] (c/try-catch x c/empty))))
  
  ;; also tests lower-level 'handle-error'
  (when-not karma?
    (let [[x inject!] (injector)]
      (dt/rendering
       (dom/div x
                (c/try-catch (c/dynamic (fn [state]
                                          (if (:throw? state)
                                            (throw (ex-info "Test" {:value :foo}))
                                            "Ok")))
                             (c/dynamic (fn [[state error]]
                                          (c/fragment "Handled " (pr-str error)
                                                      (if (:reset? state)
                                                        (c/init (c/return :state [{:throw? false} nil]))
                                                        c/empty))))))
       :state {:throw? false}
       (check-node
        (fn [node]
          (is (= "Ok" (text node)))

          (tu/preventing-error-log
           #(inject! node (f/constantly {:throw? true})))
    
          (is (= "Handled #error {:message \"Test\", :data {:value :foo}}"
                 (text node)))

          (inject! node (f/constantly {:throw? true :reset? true}))
          (is (= "Ok" (text node)))))))))

(deftest with-async-test
  (is (passes-messages (fn [x] (c/with-async (constantly x)))))
  
  (async done
         (dt/rendering
          (dom/div (c/dynamic str)
                   (c/with-async (fn [invoke!]
                                   (js/window.setTimeout #(invoke! (fn [state]
                                                                     (if (empty? state)
                                                                       (c/return :state (conj state :done))
                                                                       (c/return))))
                                                         0)
                                   c/empty)))
          :state []
          (check-node
           (fn [n]
             (is (= "[]" (text (.-firstChild n))))
             (-> (async/promise (fn [resolve reject]
                                  (js/window.setTimeout (fn []
                                                          (is (= "[:done]" (text (.-firstChild n))))
                                                          (resolve true))
                                                        1)))
                 (async/then done)))))))

(deftest focus-test
  (let [id (fn ([x] x) ([_ x] x))]
    (is (passes-messages (fn [x] (c/focus id x)))))


  (is (= "foo" (rendered-text (c/focus :a (c/dynamic str))
                              {:a "foo"}))))

(deftest local-state-test
  (is (passes-messages (fn [x] (c/local-state nil (c/focus lens/first x)))))
  
  (testing "basic"
    (is (= (pr-str [:b :a]) (rendered-text (c/local-state :a (c/dynamic pr-str))
                                           :b))))
  
  (testing "updates"
    (let [[x inject!] (injector)]
      (dt/rendering
       (dom/div (c/local-state :a
                               (c/fragment (c/dynamic pr-str) x)))
       :state :b
       (check-node
        (fn [n]
          (is (= (pr-str [:b :a]) (text n)))
      
          (inject! n (constantly [:b :c]))
          (is (= (pr-str [:b :c]) (text n)))

          (inject! n (constantly [:d :c]))
          (is (= (pr-str [:d :c]) (text n))))))))

  (testing "consistency - never see inconsistent states."
    ;; the state of 'dynamic' will be [4 2] first, then, atomically, [3 5]; no mix of inner and outer states.
    (let [[x inject!] (injector)
          states (atom [])
          invariant (fn [[a b]]
                      (or (and (odd? a) (odd? b))
                          (and (even? a) (even? b))))]
      (dt/rendering
       (dom/div (c/local-state 2
                               (c/dynamic (fn [st]
                                            (swap! states conj st)
                                            x))))
       :state 4
       (fn [env]
         (is (= 1 (count @states)))
         (inject! (dt/container env) (constantly [3 5]))
         (is (every? invariant @states))))))

  (testing "reinit"
    (let [[x inject!] (injector)]
      (dt/rendering
       (dom/div (c/with-state-as st
                  (c/fragment
                   (c/local-state st (c/dynamic pr-str))
                   x)))
       :state :b
       (check-node
        (fn [n]
          (is (= (pr-str [:b :b]) (text n)))
      
          (inject! n (constantly :c))
          (is (= (pr-str [:c :c]) (text n)))))))))

(deftest handle-state-change-test
  (is (passes-messages (fn [x] (c/handle-state-change x (fn [old new] new)))))
  
  (let [[x inject!] (injector)]
    (dt/rendering
     (dom/div (-> (c/fragment
                   (c/dynamic str)
                   x)
                  (c/handle-state-change (fn [old new]
                                           (+ old new)))))
     :state 1
     (check-node
      (fn [n]
        (is (= "1" (text n)))
    
        (inject! n (constantly 3))
        (is (= "4" (text n))))))))

(deftest named-test
  (let [name-id (c/name-id "foo")]
    (is (passes-messages (fn [x] (c/named name-id x))))

    (dt/rendering
     (c/named name-id (dom/div))
     (check-node
      (fn [n]
        (is (= "div" (tag n))))))
    
    ;; that is has an effect on React debugging utils, is probably not testable?
    ))

(deftest keyed-test
  (is (passes-messages (fn [x] (c/keyed x :foo))))
  
  (let [[x inject-x!] (injector)
        [y inject-y!] (injector)]
    (dt/rendering
     (dom/div (c/with-state-as st
                (let [l (-> (c/local-state :a
                                           (if st
                                             (c/fragment (c/dynamic pr-str) x)
                                             (c/fragment x (c/dynamic pr-str))))
                            (c/keyed :foo))]
                  (if st
                    (c/fragment l (dom/span) y)
                    (c/fragment y (dom/span) l)))))
     :state false
     (check-node
      (fn [n]
        (is (= (pr-str [false :a]) (text n)))
    
        (inject-x! n (constantly [false :b]))
        (inject-y! n (constantly true))
        (is (= (pr-str [true :b]) (text n))))))))

(deftest lifecycle-test
  (let [[x inject-x!] (injector)]
    (dt/rendering
     (dom/div (c/with-state-as st
                (c/fragment
                 (c/dynamic pr-str)
                 x
                 (if st
                   (c/lifecycle (fn [state] :init)
                                (fn [state] nil))
                   c/empty))))
     :state false
     (check-node
      (fn [n]
        (is (= "false" (text n)))
        (inject-x! n (constantly true))
        (is (= ":init" (text n)))
    
        (inject-x! n (constantly false))
        (is (= "nil" (text n))))))))

(deftest bubbling-events-test
  ;; state consitency upon a bubbling event.
  ;; Note: work with the React implementation, not with the Reacl implementation (https://github.com/active-group/reacl/issues/47).
  (let [last-c1-local (atom nil)
        inner (atom false)
        outer (atom false)
        
        c1 (c/dynamic (fn [state]
                        (reset! last-c1-local state)
                        (dom/div {:onClick (fn [state ev]
                                             (conj state :new-local-2))}
                                 (dom/div {:onClick (fn [state ev]
                                                      (conj state :new-local-1))}))))]
    (dt/rendering
     c1
     :state []
     (fn [env]
       (let [inner-div (.-firstChild (.-firstChild (dt/container env)))]
         (react-tu/Simulate.click inner-div (js/Event. "click" #js {:bubbles true :cancelable true})))

       (is (= [:new-local-1 :new-local-2] @last-c1-local))))))

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
        (dt/rendering
         (c/handle-action (c/fragment sub x)
                          conj)
         :state []
         (fn [env]
           (is (= [:sync] @last-state))
           (@subscribed :async) ;; @subscribed = deliver! fn.
           (is (= [:sync :async] @last-state))))))

    (testing "unsub on unmount"
      (reset! subscribed nil)
      (dt/rendering
       (c/handle-action sub
                        conj)
       :state []
       (fn [env]
         (is @subscribed)))
      (is (not @subscribed)))

    (testing "synchronous unsub"
      ;; subscribing and unsubscribing in the same update cycle... tricky.
      (reset! subscribed nil)
      (dt/rendering
       (c/handle-action (c/dynamic (fn [sub?]
                                     (if sub? sub c/empty)))
                        (fn [st a]
                          false))
       :state true
       (fn [env] nil))
      (is (not @subscribed))))
  )

(deftest subscription-with-arg-test
  (let [order (atom [])
        sub-impl (fn [deliver! x]
                   (swap! order conj [:sub x])
                   (fn stop-fn []
                     (swap! order conj [:unsub x])))
        sub #(c/subscription sub-impl %)]

    (testing "different arg is new subscription"
      (reset! order [])
      (let [[it inject!] (injector)]
        (dt/rendering
         (c/dynamic (fn [x]
                      (c/fragment (sub x) it)))
         :state :x
         (fn [env]
           (is (= [[:sub :x]]))
           (inject! (dt/container env) (constantly :y))
           (is (= [[:sub :x] [:unsub :x] [:sub :y]] @order))))))
    ))

(deftest defn-subscription-test
  (c/defn-subscription defn-subscription-test-1 ^:always-validate deliver! :- s/Keyword [arg]
    (deliver! arg)
    (fn [] nil))

  (testing "schema validation in deliver!"
    (when-not karma?
      (is (throws-like? #(dt/rendering
                          (defn-subscription-test-1 "foo")
                          (fn [env] nil))
                        "Input to deliver! does not match schema:")))))

(deftest def-item-test
  (testing "simple items"
    (c/def-item def-test-1
      "foo")

    (dt/rendering
     def-test-1
     (check-node
      (fn [n]
        (is (= (text n) "foo"))))))

  (testing "state schema validation"
    (c/def-item ^:always-validate def-test-3 :- s/Str
      (c/with-state-as a a))

    (dt/rendering
     def-test-3
     :state "foo"
     (fn [env]
       (is true)))

    (when-not karma?
      (is (throws-like? #(dt/rendering def-test-3 :state :foo (fn [env] nil))
                        "Input to state-of-def-test-3 does not match schema")))))

(deftest defn-item-test
  (testing "basics"
    (c/defn-item defn-test-1 "foo" [a :- s/Str]
      a)

    (is (=  "Ok" (rendered-text (defn-test-1 "Ok")))))

  (testing "dynamic items"
    (c/defn-item defn-test-2 [p]
      (c/with-state-as [a b :local "bar"]
        (dom/div a b p)))

    (is (= "foobarbaz" (rendered-text (defn-test-2 "baz")
                                      "foo"))))

  (testing "static items don't rerender"
    (c/defn-item defn-test-4 :static [rendered]
      (c/with-state-as _
        (swap! rendered inc)
        "foo"))

    (let [rendered (atom 0)
          [x inject!] (injector)]
      (dt/rendering
       (c/fragment (defn-test-4 rendered)
                   x)
       :state 0
       (fn [env]
         (is (= 1 @rendered))

         (inject! (dt/container env) inc)
         (is (= 1 @rendered))))))

  (testing "state schema validation"
    (c/defn-item ^:always-validate defn-test-3 :- s/Str [a :- s/Int]
      (str a))

    (dt/rendering
     (defn-test-3 42)
     :state "foo"
     (fn [env]
       (is true)))

    (when-not karma?
      (is (throws-like? #(dt/rendering (defn-test-3 42) :state :foo (fn [env] nil))
                        "Input to state-of-defn-test-3 does not match schema: \n\n\t [0;33m  [(named (not (string? :foo))"))))
  )

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
    (is (= (rendered-text (c/with-state-as foo foo) "Ok")
           "Ok"))

    (is (= (rendered-text (c/with-state-as foo :- s/Str foo) "Ok")
           "Ok"))

    (is (= (rendered-text (c/with-state-as [a b] (dom/div a b)) ["foo" "bar"])
           "foobar"))

    (is (= (rendered-text (c/with-state-as [a b :local "bar"] (dom/div a b)) "foo")
           "foobar")))

  (testing "schema validation"
    (when-not karma?
      (s/with-fn-validation
        (is (throws-like? #(dt/rendering (c/with-state-as foo :- s/Str foo) :state 42 (fn [env] nil))
                          "Input to fn"))))))

(deftest with-bind-test
  (dt/rendering
   (c/with-bind
     (fn [bind]
       (let [h (bind (fn [state _]
                       (update state :counter inc)))]
         (c/focus :counter
                  (dom/button {:onClick h}
                              (c/dynamic str))))))
   :state {:counter 0}
   (check-node
    (fn [n]
      (is (= "0" (text n)))

      (react-tu/Simulate.click n)
      (is (= "1" (text n)))))))

(deftest finish-state-test
  ;; when a state storage (local state) is removed in an unmount, but
  ;; a finalizer still refers to it, it may cause problems.
  (let [finished (atom nil)]
    (dt/rendering
     (c/with-state-as a
       (dom/button {:onClick (fn [st _] false)}
                   (c/dynamic str)
                   (when a
                     (c/local-state "foo"
                                    (c/cleanup (fn [st]
                                                 (reset! finished st)
                                                 (c/return)))))))
     :state true
     (check-node
      (fn [n]
        (is (= "true" (text n)))
    
        (react-tu/Simulate.click n)
        (is (= "false" (text n)))
        (is (= [false "foo"] @finished)))))))

(defn test-rerender [item state f]
  ;; Note: not relying on toplevel main/run for the update/rerender for now; that might be different, esp. in react 18.
  (let [[x inject!] (injector)]
    (dt/rendering
     (c/fragment
      (c/dynamic (fn [[item _]]
                   (c/focus lens/second item)))
      x)
     :state [item state]
     (fn [env]
       (f env (fn update! [item & [state]]
                (inject! (dt/container env)
                         (constantly [item state]))))))))

(deftest handle-action-rerender-test
  (c/defn-item handle-action-rerender-test-1 :static [called]
    (swap! called inc)
    (dom/div "foo"))
  (let [called (atom 0)
        item (fn []
               (-> (handle-action-rerender-test-1 called)
                   (c/handle-action (fn [] :foo))))]
    (test-rerender
     (item) 42
     (fn [env update!]
       (is (= 1 @called))
       (update! (item) 43)
       (is (= 1 @called))))))

(deftest focus-rerender-test
  (c/defn-item focus-rerender-test-1 [called]
    (swap! called inc)
    (dom/div "foo"))
  
  (let [called (atom 0)
        item (c/local-state "x" (c/focus lens/second (focus-rerender-test-1 called)))]
    (test-rerender
     item
     42
     (fn [env update!]
       (is (= 1 @called))
       ;; update with new state, but local-state+focus make f not being called again.
       (update! item 43)
       (is (= 1 @called))))))

(deftest recursive-item-test
  ;; a macro-related regression; wrong function was bound inside.
  (c/defn-item recursive-item-test-1 [level]
    (c/with-state-as st
      (assert (some? level))
      (dom/div (str level)
               (when (> level 1)
                 (recursive-item-test-1 (dec level))))))
  
  (is (= "21" (rendered-text (recursive-item-test-1 2)))))

(deftest dynamic-non-dynamic-test
  ;; regression for too hard/wrong optimization of 'removing' unused state.
  (let [tested (atom false)]
    (dt/rendering (c/focus (fn
                       ([x]
                        ;; should either not be called, but if then with the correct state.
                        (is (integer? x))
                        x)
                       ([x y]
                        (is false "should be called")
                        (is (integer? x))
                        (is (integer? y))
                        y))
                     ;; even though nil doesn't use state itself, focus must still see the right thing (or not be called)
                     nil)
                  :state 42
                  (fn [env] nil))))

(deftest recursion-test
  ;; regression for 'recursive items', where fns and arities got mixed up.
  (c/defn-item recursion-test-1 [a y]
    (c/with-state-as x
      (dom/div (when (> a 0)
                 (recursion-test-1 (dec a) x)))))
  (dt/rendering (recursion-test-1 5 nil)
                (fn [env]
                  ;; ok if it does not throw
                  (is true))))

