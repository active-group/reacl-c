(ns reacl-c.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.test-renderer :as tu]
            [reacl-c.test-util.core :as tuc]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [schema.core :as s :include-macros true]
            [clojure.string :as str]
            [reacl-c.test-util.perf :as perf]
            [cljs.test :refer (is deftest testing) :include-macros true]))

;; TODO: remove things already tested in main-browser-test... e.g. behavioral tests only of the higher level components; everything that depends on test-renderer

(deftest item-equality-test
  ;; items should be referentially equal
  (testing "fragment"
    (is (= (c/fragment) (c/fragment)))
    (is (= (c/fragment "foo") (c/fragment "foo")))
    ;; to simplify conditional rendering (via 'when'), nil is the same as an empty fragment
    (is (= nil (c/fragment)))
    (is (= nil c/empty)))
  (testing "dynamic"
    (let [f (fn [x] (c/fragment x))]
      (is (= (c/dynamic f) (c/dynamic f)))))
  (testing "focus"
    (is (= (c/focus :a "foo") (c/focus :a "foo"))))
  (testing "handle-action"
    (let [f (fn [state a])]
      (is (= (c/handle-action "foo" f) (c/handle-action "foo" f)))))
  (testing "add-state"
    (is (= (c/add-state :a :b "foo") (c/add-state :a :b "foo"))))
  (testing "keyed"
    (is (= (c/keyed "foo" :a) (c/keyed "foo" :a))))
  (testing "once"
    (is (= (c/once (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))) (c/once (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))))))
  (testing "with-async-actions"
    (is (= (c/with-async-actions :f :a) (c/with-async-actions :f :a))))
  (testing "monitor-state"
    (is (= (c/handle-state-change "foo" :f) (c/handle-state-change "foo" :f))))
  )

(deftest defn-subscription-test
  (testing "basic creation and equality"
    (let [x (atom nil)]
      (c/defn-subscription defn-subscription-test-1 deliver! [arg]
        (reset! x arg)
        (fn [] nil))

      (is (c/return)
          (-> (tu/env (defn-subscription-test-1 :arg))
              (tu/mount! nil)))
      (is (= :arg @x))

      (is (= (defn-subscription-test-1 :arg)
             (defn-subscription-test-1 :arg)))))

  (testing "synchronous delivery, and schema validation"
    (let [res (atom false)]
      (c/defn-subscription defn-subscription-test-2 ^:always-validate deliver! :- s/Int [arg]
        (reset! res nil)
        (try (tuc/preventing-error-log
              #(deliver! arg))
             (catch :default e
               (reset! res e)))
        (fn [] nil))

      (is (= (c/return :action 42)
             (-> (tu/env (defn-subscription-test-2 42))
                 (tu/mount!! nil))))
      (is (nil? @res))

      (-> (tu/env (defn-subscription-test-2 "42"))
          (tu/mount! nil))
      (let [e @res]
        (is (str/starts-with? (.-message e)
                              "Input to deliver! does not match schema:")))))
  )


(deftest with-async-messages-test
  (let [env (tu/env (c/with-ref (fn [ref]
                                  (c/with-async-messages
                                    (fn [send!]
                                      (c/fragment (-> (c/handle-message (fn [state msg]
                                                                          (c/return :state msg))
                                                                        "foo")
                                                      (c/refer ref))
                                                  (-> (c/once (f/constantly (c/return :action ::test)))
                                                      (c/handle-action (fn [_ _]
                                                                         (send! ref :msg)
                                                                         (c/return))))))))))]
    (is (= (c/return :state :msg)
           (tu/mount! env :st)))))

(deftest sync-messages-test
  (let [env (tu/env (c/with-ref (fn [ref]
                                  (c/with-async-messages
                                    (fn [send!]
                                      (c/fragment (-> (c/handle-message (fn [state msg]
                                                                          (c/return :state msg))
                                                                        "foo")
                                                      (c/refer ref))
                                                  (c/once (f/constantly (c/return :message [ref :msg])))))))))]
    (is (= (c/return :state :msg)
           (tu/mount! env :st)))))

(deftest map-messages-test
  (let [env (tu/env (c/map-messages (fn [msg] [:x msg])
                                    (c/handle-message (fn [state msg]
                                                        (c/return :state msg))
                                                      "foo")))]
    (tu/mount! env :st)
    (is (= (c/return :state [:x :msg])
           (tu/send-message! (tu/get-component env) :msg)))))

(deftest redirect-messages-test
  (let [env (tu/env (c/with-ref
                      (fn [ref]
                        (c/fragment
                         (c/redirect-messages ref
                                              (c/fragment "foo"
                                                          (-> (c/handle-message (fn [state msg]
                                                                                  (c/return :state msg))
                                                                                "bar")
                                                              (c/refer ref))))))))]
    (tu/mount! env :st)
    (is (= (c/return :state :msg)
           (tu/send-message! (tu/get-component env) :msg)))))

(deftest defn-messages-test
  (c/defn-item defn-messages-test-1 :- [s/Int] [x]
    (assert (int? x))
    (c/handle-message (fn [state m]
                        (conj state x m))
                      c/empty))
  (let [env (tu/env (defn-messages-test-1 42))]
    (tu/mount! env [])
    (is (= (c/return :state [42 13])
           (tu/send-message! env 13))))

  (c/def-item defn-messages-test-2
    (c/handle-message (fn [state m]
                        (conj state m))
                      c/empty))
  (let [env (tu/env defn-messages-test-2)]
    (tu/mount! env [])
    (is (= (c/return :state [13])
           (tu/send-message! env 13)))))

(deftest once-test
  ;; state dependant.
  (let [env (tu/env (c/once (f/partial c/return :action)
                            (f/partial c/return :action)))]
    (is (= (c/return :action ::up)
           (tu/mount!! env ::up)))

    (is (= (c/return)
           (tu/update!! env ::up)))

    (is (= (c/return :action ::new)
           (tu/update!! env ::new)))

    (tu/update!! env ::down)
    (is (= (c/return :action ::down)
           (tu/unmount!! env))))

  ;; state independant
  (let [env (tu/env (c/once (f/constantly (c/return :action ::up))
                            (f/constantly (c/return :action ::down))))]
    (is (= (c/return :action ::up)
           (tu/mount!! env true))) 

    (is (= (c/return)
           (tu/update!! env true)))

    (is (= (c/return)
           (tu/update!! env false))) 

    (is (= (c/return :action ::down)
           (tu/unmount!! env)))))

(deftest with-ref
  ;; important for with-ref, must be the same ref for a unique item, even if the state changes.
  (let [last-ref (atom nil)
        env (tu/env (c/with-ref (fn [ref]
                                  (reset! last-ref ref)
                                  c/empty)))]
    (let [_ (tu/mount!! env true)
          r1 @last-ref

          _ (tu/update!! env false)
          r3 @last-ref

          _ (tu/update!! env false)
          r2 @last-ref]

      (is (= r1 r2) "ref is same on same state")

      (is (= r1 r3) "ref is same on different states"))))

(deftest effect-test
  (c/defn-effect effect-test-1 [foo]
    (if (= foo :foo)
      42
      (c/return :state 21
                :action :test)))

  (c/defn-effect ^:always-validate effect-test-2 :- s/Int [foo :- s/Keyword]
    "err")
  
  (testing "basics, equality"
    (is (= (effect-test-1 :foo)
           (effect-test-1 :foo)))))

(deftest ref-let-test
  (let [env (tu/env (c/ref-let [it-1 (c/handle-message (fn [state msg]
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
                                (c/fragment it-1 it-2))))]
    (tu/mount! env nil)
    (is (= (c/return :state [:it-1 1])
           (tu/send-message! env 1)))
    (is (= (c/return :state [:it-2 2])
           (tu/send-message! env 2)))))

(deftest handle-action-test
  (testing "basics"
    (let [foobar (c/name-id "foobar")
          item (c/named foobar "foo")
          env (tu/env (c/handle-action item
                                       (fn [state a]
                                         (c/return :state (conj state a)))))]
    
      (tu/mount! env [])
      (is (some? (tu/find env item)))
    
      (is (= (c/return :state [:a])
             (tu/inject-action! (tu/find env item)
                                :a)))

      (tu/update! env [:a])
      (is (= (c/return :state [:a :b])
             (tu/inject-action! (tu/find env item)
                                :b)))))
  (testing "plain state return"
    (let [item (c/dynamic (f/constantly "bar"))
          env (tu/env (c/handle-action item
                                       conj))]
      (tu/mount! env [])
      (is (= (c/return :state [:a])
             (tu/inject-action! (tu/find env item)
                                :a))))))

(deftest try-catch-test
  (let [env (tu/env (c/try-catch (c/dynamic #(if (:throw? %)
                                               (throw (ex-info "Test" {:value :foo}))
                                               "Ok"))
                                 (c/dynamic (fn [[state error]]
                                              (c/fragment "Error" (pr-str error)
                                                          (if (:reset? state)
                                                            (c/once (f/constantly (c/return :state [{} nil])))
                                                            c/empty))))))]
    (tu/mount! env {})
    (is (some? (tu/find env "Ok")))

    (tuc/preventing-error-log
     (fn []
       (tu/update! env {:throw? true})))
    (is (some? (tu/find env "Error")))

    (is (= (c/return :state {})
           (tu/update! env {:throw? false :reset? true})))
    (is (some? (tu/find env "Ok")))
    ))

(deftest with-state-as-test
  (let [env (tu/env (c/with-state-as foo foo))]
    (tu/mount! env "Ok")
    (is (some? (tu/find env "Ok"))))

  (let [env (tu/env (c/with-state-as foo :- s/Str foo))]
    (tu/mount! env "Ok")
    (is (some? (tu/find env "Ok"))))

  (let [env (tu/env (c/with-state-as [a b] (c/fragment a b)))]
    (tu/mount! env ["foo" "bar"])
    (is (some? (tu/find env (c/fragment "foo" "bar")))))

  (let [env (tu/env (c/with-state-as [a b :local "bar"] (c/fragment a b)))]
    (tu/mount! env "foo")
    (is (some? (tu/find env (c/fragment "foo" "bar"))))))

(deftest defn-item-test
  (testing "simple items"
    (c/defn-item defn-test-1 "foo" [a :- s/Str]
      a)

    (is (= "foo" (:doc (meta #'defn-test-1))))
  
    (let [env (tu/env (defn-test-1 "Ok"))]
      (tu/mount! env "Ok")
      (is (some? (tu/find env "Ok")))))

  (testing "dynamic items"
    (c/defn-item defn-test-2 [p]
      {:pre [(string? p)]}
      (c/with-state-as [a b :local "bar"]
        (c/fragment a b p)))

    (let [env (tu/env (defn-test-2 "baz"))]
      (tu/mount! env "foo")
      (is (some? (tu/find env (c/fragment "foo" "bar" "baz")))))

    ;; and with-state-as is optimized (made static)
    (is (not (perf/find-first-difference (defn-test-2 "foo") (defn-test-2 "foo")))))

  (testing "static items"
    (c/defn-item defn-test-4 :static [rendered?]
      (reset! rendered? true)
      "foo")
    
    (let [rendered? (atom false)
          env (tu/env (defn-test-4 rendered?))]
      (tu/mount! env :bar)
      (is (some? (tu/find env "foo")))
      (is @rendered?)

      (reset! rendered? false)
      (tu/update! env :foo)
      (is (not @rendered?))))

  (testing "schema validation"
    (c/defn-item ^:always-validate defn-test-3 :- s/Str [a :- s/Int]
      (str a))

    (is (some? (defn-test-3 42)))

    (is (= "Input to defn-test-3 does not match schema: \n\n\t [0;33m  [(named (not (integer? :foo)) a)] [0m \n\n"
           (try (defn-test-3 :foo)
                nil
                (catch :default e
                  (.-message e)))))

    (is (some? (tu/mount! (tu/env (defn-test-3 42)) "foo")))

    (tuc/preventing-error-log
     (fn []
       (is (str/starts-with?
            (try (tu/mount! (tu/env (defn-test-3 42)) :foo)
                 false
                 (catch :default e
                   (.-message e)))
            "Input to state-of-defn-test-3 does not match schema: \n\n\t [0;33m  [(named (not (string? :foo))")))))

  (testing "regression with schemata"
    (c/defn-item defn-test-5 "foo" [x a :- s/Str y]
      a)

    (let [env (tu/env (defn-test-5 1 "Ok" 2))]
      (tu/mount! env "Ok")
      (is (some? (tu/find env "Ok"))))
    ))

(deftest def-item-test
  (testing "simple items"
    (c/def-item def-test-1
      "foo")

    (let [env (tu/env def-test-1)]
      (tu/mount! env nil)
      (is (some? (tu/find env "foo")))))

  (testing "dynamic items"
    (c/def-item def-test-2
      (c/with-state-as a
        a))

    (let [env (tu/env def-test-2)]
      (tu/mount! env "foo")
      (is (some? (tu/find env "foo")))))

  (testing "state schema validation"
    (c/def-item ^:always-validate def-test-3 :- s/Str
      (c/with-state-as a a))

    (is (some? (tu/mount! (tu/env def-test-3) "foo")))

    (tuc/preventing-error-log
     (fn []
       (is (str/starts-with?
            (try (tu/mount! (tu/env def-test-3) :foo)
                 false
                 (catch :default e
                   (.-message e)))
            "Input to state-of-def-test-3 does not match schema: \n\n\t [0;33m  [(named (not (string? :foo))"))))))

(deftest embed-returned-test
  (is (= (c/embed-returned [:a :b] lens/first (c/return :state :c))
         (c/return :state [:c :b])))

  (is (= (c/embed-returned [:a :b] lens/first (c/return))
         (c/return))))

(deftest lift-handler-test
  (is (= ((c/lift-handler lens/second (fn [st a] (c/return :state (conj st a))))
          [:a []]
          :foo)
         (c/return :state [:a [:foo]])))
  (is (= ((c/lift-handler lens/first (fn [st] :bar))
          [:a :foo])
         (c/return :state [:bar :foo]))))
