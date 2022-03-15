(ns reacl-c.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.core :as tuc]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [schema.core :as s :include-macros true]
            [clojure.string :as str]
            [reacl-c.test-util.perf :as perf]
            [cljs.test :refer (is deftest testing) :include-macros true]))

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
  (testing "keyed"
    (is (= (c/keyed "foo" :a) (c/keyed "foo" :a))))
  (testing "once"
    (is (= (c/once (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))) (c/once (f/constantly (c/return :action :a)) (f/constantly (c/return :action :b))))))
  (testing "with-async"
    (is (= (c/with-async :f :a) (c/with-async :f :a))))
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

(deftest subscription-deconstruct-test
  (c/defn-subscription ^:always-validate subscription-properties-test-2 deliver! :- s/Any [arg :- s/Keyword]
    (assert false)
    (fn [] nil))
  (c/defn-subscription subscription-properties-test-3 deliver! [arg]
    (assert false)
    (fn [] nil))
  (let [sub-1-f (fn [deliver! arg]
                  (assert false)
                  (fn [] nil))
        sub-1 (c/subscription sub-1-f
                              :foo)
        sub-2 (subscription-properties-test-2 :bar)
        sub-3 (subscription-properties-test-3 :baz)]

    (is (some? (c/subscription-deconstruct sub-1)))
    (is (some? (c/subscription-deconstruct sub-2)))
    (is (some? (c/subscription-deconstruct sub-3)))
    
    (is (= [nil sub-1-f [:foo]] (c/subscription-deconstruct sub-1)))
    ;; Note: the implementing function is something internal for defn-subscription (takes deliver! and arg)
    (is (= [subscription-properties-test-2 'impl [:bar]] (assoc (c/subscription-deconstruct sub-2) 1 'impl)))
    (is (= [subscription-properties-test-3 'impl [:baz]] (assoc (c/subscription-deconstruct sub-3) 1 'impl)))
    )
  )

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

    ;; and with-state-as or defn-item is optimized (made static)
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
