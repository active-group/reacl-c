(ns reacl-c.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.test-renderer :as tu] ;; TODO: get rid of this
            [reacl-c.test-util.core :as tuc]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [schema.core :as s :include-macros true]
            [clojure.string :as str]
            [reacl-c.test-util.perf :as perf]
            [cljs.test :refer (is deftest testing) :include-macros true]))

;; TODO: remove things already tested in main-browser-test... e.g. behavioral tests only of the higher level components; everything that depends on test-renderer

;;(s/set-fn-validation! true) ;; TODO: should work without this.

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

(defn throws-like? [thunk message]
  (try
    (thunk)
    false
    (catch :default e
      (str/starts-with? (.-message e) message))))

(deftest defn-subscription-test
  (c/defn-subscription ^:always-validate defn-subscription-test-1 deliver! [arg :- s/Keyword]
    (deliver! arg)
    (fn [] nil))
  
  (testing "basic creation and equality"
    (is (= (defn-subscription-test-1 :arg)
           (defn-subscription-test-1 :arg))))

  (testing "argument schema validation"
    (is (throws-like? #(defn-subscription-test-1 "foo")
                      "Input to defn-subscription-test-1 does not match schema"))))

(deftest subscription-test
  (let [ff (fn [deliver! a] (fn [] nil))]
    (is (= (c/subscription ff :foo)
           (c/subscription ff :foo)))))

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


(deftest effect-test
  (testing "equality"
    (c/defn-effect effect-test-1 [foo]
      42)

    (is (= (effect-test-1 :foo)
           (effect-test-1 :foo)))

    (is (= (c/effect (f/constantly nil))
           (c/effect (f/constantly nil)))))

  (testing "argument schema validation"
    (c/defn-effect ^:always-validate effect-test-2 :- s/Int [foo :- s/Keyword]
      "err")
  
    (is (throws-like? #(effect-test-2 "foo")
                      "Input to effect-test-2 does not match schema"))))

(deftest defn-item-parser-test
  (is (= ['test false nil nil '[a] 'body] (c/parse-defn-item-args 'test '[a] 'body)))
  (is (= ['test true nil nil '[a] 'body] (c/parse-defn-item-args 'test :static '[a] 'body)))
  (is (= ['test true nil "foo" '[a] 'body] (c/parse-defn-item-args 'test :static "foo" '[a] 'body)))
  (is (= ['test false 's "foo" '[a] 'body] (c/parse-defn-item-args 'test :- 's "foo" '[a] 'body)))
  (is (= ['test false 's nil '[a] 'body] (c/parse-defn-item-args 'test :- 's '[a] 'body)))
  (is (= ['test false nil "foo" '[a] 'body] (c/parse-defn-item-args 'test "foo" '[a] 'body))))

(deftest defn-item-test
  (testing "simple items"
    (c/defn-item defn-test-1 "foo" [a :- s/Str]
      a)

    (is (= (defn-test-1 "bla")
           (defn-test-1 "bla")))

    (is (= "foo" (:doc (meta #'defn-test-1)))))

  (testing "dynamic items"
    (c/defn-item defn-test-2 [p]
      {:pre [(string? p)]}
      (c/with-state-as [a b :local "bar"]
        (c/fragment a b p)))

    ;; and with-state-as is optimized (made static)
    (is (not (perf/find-first-difference (defn-test-2 "foo") (defn-test-2 "foo")))))

  (testing "argument schema validation"
    (c/defn-item ^:always-validate defn-test-3 [a :- s/Int]
      (str a))

    (is (some? (defn-test-3 42)))

    (is (throws-like? #(defn-test-3 :foo)
                      "Input to defn-test-3 does not match schema: \n\n\t [0;33m  [(named (not (integer? :foo)) a)] [0m \n\n")))

  (testing "regression with schemata"
    (c/defn-item ^:always-validate defn-test-5 "foo" [x a :- s/Str y]
      a)

    (is (some? (defn-test-5 1 "Ok" 2)))))

(deftest def-item-test
  (testing "simple items"
    (c/def-item def-test-1
      "foo")

    (let [env (tu/env def-test-1)]
      (tu/mount! env nil)
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

(deftest many-args-test
  (c/defn-item defn-item-many-args-test [& args]
    (apply c/fragment args))

  (s/defn defn-schema-many-args-test :- s/Int [& args]
    (apply + args))

  (is (= (apply c/fragment (repeat 50 c/empty))
         (apply c/fragment (repeat 50 c/empty))))

  (is (= 50
         (apply defn-schema-many-args-test (repeat 50 1))))

  ;; currently does not work with s/fn though:
  #_(is (= 50
         (apply (s/fn :- s/Int [& args :- [s/Int]] (apply + args)) (repeat 50 1))))

  ;; Note: currently only works when no schema annoations are used (until ClojureScript solves the MetaFn arity bug :-/)
  (is (= (apply defn-item-many-args-test (repeat 50 c/empty))
         (apply defn-item-many-args-test (repeat 50 c/empty)))))
