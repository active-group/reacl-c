(ns reacl-c.test-util.test-renderer-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.test-renderer :as tu]
            [active.clojure.functions :as f]
            [cljs.test :as t :refer (is deftest testing) :include-macros true]
            [cljs-async.core :as async]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :include-macros true :refer [defspec]]))

(defspec item-find-self-test {:seed 4712
                              :num-tests 200}
  (let [same (fn [pat item]
               (let [env (tu/env item)]
                 (tu/mount! env nil)
                 (some? (tu/find env pat))))]
    (prop/for-all [v item-gen/node-item]
                  (same v v))))


(deftest find-items-test
  (let [find (fn [pat item]
               (let [env (tu/env item)]
                 (tu/mount! env nil)
                 (some? (tu/find env pat))))]
    ;; can't find nothing :-/
    (is (find c/empty (dom/div)))
    (is (find c/empty (c/fragment (dom/div))))
    ;; can't find something in nothing :-/
    ;;(is (find c/empty c/empty))
    ;;(is (find c/empty (c/fragment c/empty)))

    ;; unfortunately not possible either (xpath needs a component at toplevel currently):
    ;;(is (find c/empty "foo"))
    (is (find (dom/div c/empty) (dom/div "foo")))

    (is (find (dom/div) (dom/div)))
    (is (find (dom/div) (c/fragment (dom/div))))
    (is (find (dom/div) (c/fragment (dom/div) (dom/span))))

    (is (find (dom/span) (c/fragment (dom/div) (dom/span))))

    ;; not easy to define which node a fragment should select for:
    #_(is (find (c/fragment (dom/div)) (dom/div)))
    #_(is (find (c/fragment (dom/div)) (c/fragment (dom/div))))
    #_(is (find (c/fragment (dom/div) (dom/span)) (c/fragment (dom/div) (dom/span))))
    
    (is (find (dom/div (c/fragment (dom/div) (dom/span))) (dom/div (c/fragment (dom/div) (dom/span)))))

    ;; partial dom matches:
    (is (find (dom/div) (dom/div {:id "x"})))
    (is (find (dom/div {:id "x"}) (dom/div {:id "x" :type "t"})))
    (is (find (dom/div {:class "x"}) (dom/div {:class "x y"})))
    (is (find (dom/div) (dom/div (dom/span))))
    (is (not (find (dom/div {:id "x"}) (dom/div))))

    (is (find (dom/div (dom/a)) (dom/div (dom/br) (dom/a))))

    ;; Note: is the case; but maybe shouldn't? (is (find (dom/div (dom/a) (dom/br)) (dom/div (dom/br) (dom/a))))

    (is (find (dom/div) (c/handle-message :f (dom/div))))

    ;; dynamic
    (is (find (c/dynamic (f/constantly (dom/div)) :a) (c/dynamic (f/constantly (dom/div)) :a)))

    ;; dom ignores others (= structural match)
    (is (find (dom/div) (c/dynamic (f/constantly (dom/div)))))
    (is (find (dom/div (dom/span)) (c/dynamic (f/constantly
                                               (dom/div (c/dynamic (f/constantly
                                                                    (dom/span))))))))
    (is (find (dom/div (dom/span) (dom/span)) (dom/div (dom/span) (dom/span))))
    (is (not (find (dom/div (dom/span) (dom/span)) (dom/div (dom/span)))))
    (is (find (dom/div "foo") (dom/div (c/dynamic (f/constantly "foo")))))

    ;; regression
    ;; FIXME:?
    #_(is (find (dom/div (c/with-ref (fn [r] c/empty))
                       (c/keyed (dom/span) "_"))
              (dom/div (c/with-ref (fn [r] c/empty))
                       (c/keyed (dom/span) "_"))))
    (is (find (dom/div (dom/span)) (dom/div (dom/span {:id "y"}))))
    ))

(deftest describe-failed-find-test
  (let [f (fn [pat item]
            (let [env (tu/env item)]
              (tu/mount! env nil)
              (let [r (tu/describe-failed-find env pat)]
                ;; (when r (println r))
                r)))]
    (is (some? (f (dom/div (dom/span)) (dom/div (dom/br)))))

    (is (some? (f (dom/div (dom/span {:id "x"})) (dom/div (dom/span {:id "y"})))))
    )
  )

(deftest mount-test
  (is (= (c/return)
         (-> (tu/env (dom/div))
             (tu/mount! :state))))
  (is (= (c/return :action ::act)
         (-> (tu/env (c/once (f/constantly (c/return :action ::act))))
             (tu/mount! :state)))))

(deftest update-test
  (let [e (tu/env (c/dynamic
                   (fn [state]
                     (if state (dom/div) (dom/span)))))]
    (tu/mount! e true)
    (is (tu/find e (dom/div)))
    (tu/update! e false)
    (is (tu/find e (dom/span)))))

(deftest full-update-test
  (let [e (tu/env (c/once (fn [state]
                            (if (< state 10)
                              (c/return :state (inc state))
                              (c/return)))))]
    (is (= (c/return :state 1) (tu/mount! e 0)))
    (is (= (c/return :state 2) (tu/update! e 1))) ;; only one update
    (is (= (c/return :state 10) (tu/update!! e 2)))) ;; all updates
  (let [e (tu/env (c/once (fn [state]
                            (c/return :state (inc state)))))]
    (tu/mount! e 0)
    (try (tu/update!! e 1)
         (is false)
         (catch :default e
           (is true)))))

(deftest unmount-test
  (is (= (c/return :action ::act)
         (let [e (tu/env (c/once (f/constantly (c/return)) (f/constantly (c/return :action ::act))))]
           (tu/mount! e :state)
           (tu/unmount! e)))))

(deftest send-message-test
  (is (= (c/return :action :msg)
         (let [e (tu/env (->> (dom/div)
                              (c/handle-message
                               (fn [state msg]
                                 (c/return :action msg)))))]
           (tu/mount! e :state)
           (tu/send-message! e :msg))))
  (is (= (c/return :state {:data :state2})
         (let [e (tu/env (c/focus :data (->> (dom/div)
                                             (c/handle-message
                                              (fn [state msg]
                                                (c/return :state msg))))))]
           (tu/mount! e {:data :state1})
           (tu/send-message! (tu/get-component e) :state2)))))

(deftest invoke-callback-test
  (let [e (tu/env (dom/div {:onclick (constantly (c/return :action :act1))}))
        as (fn [v]
             (tu/invoke-callback! (tu/find e (dom/div))
                                  :onclick #js {:type "click"}))]
    (tu/mount! e :state)
    (is (= (c/return :action :act1) (as "div")))
    (is (= (c/return :action :act1) (as (dom/div))))
    ))

(deftest inject-action-test
  (let [foobar (c/name-id "foobar")
        item (c/named foobar (dom/div))
        e (tu/env item)]
    (tu/mount! e :state)
    (is (= (c/return :action :act1)
           (tu/inject-action! (tu/find e item)
                              :act1)))))

(deftest inject-state-change-test
  (c/defn-item foobar [] (c/with-state-as state (dom/div (pr-str state))))
  (let [e (tu/env (foobar))]
    (tu/mount! e :state1)
    (is (= (c/return :state :state2)
           (tu/inject-state-change! (tu/find e (foobar))
                                    :state2)))))
