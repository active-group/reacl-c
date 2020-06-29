(ns reacl-c.test-util.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.core :as tu]
            [reacl-c.impl.reacl :as impl]
            [active.clojure.functions :as f]
            [cljs.test :refer (is deftest testing) :include-macros true]))

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

(deftest find-items-test-2
  (let [find (fn [pat item]
               (let [env (tu/env item)]
                 (tu/mount! env nil)
                 (tu/find env pat)))
        find-t (fn [pat item]
                 (when-let [x (find pat item)]
                   (.-type x)))
        ]
    ;; toplevel
    (is (= "div" (find-t (dom/div) (dom/div))))
    (is (= "div" (find-t (dom/div c/empty) (dom/div "foo"))))
    (is (= "div" (find-t (dom/div) (c/fragment (dom/div)))))

    ;; one level deep
    (is (= "span" (find-t (dom/span) (dom/div (dom/span)))))

    ;; partial matches
    (is (= "div" (find-t (dom/div {:id "x"}) (dom/div {:id "x" :type "t"}))))

    ;; structural matches (ignore invisible nodes)
    (is (= "div" (find-t (dom/div) (c/dynamic (f/constantly (dom/div))))))

    (is (= "reacl-c.impl.reacl/dynamic" (.-displayName (find-t (c/dynamic (f/constantly (dom/span)))
                                                               (c/dynamic (f/constantly (dom/span)))))))
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
  (is (= (c/return :state :state2)
         (let [e (tu/env (->> (dom/div)
                              (c/handle-message
                               (fn [state msg]
                                 (c/return :state msg)))))]
           (tu/mount! e :state1)
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
  (c/defn-dynamic foobar state [] (dom/div (pr-str state)))
  (let [e (tu/env (foobar))]
    (tu/mount! e :state1)
    (is (= (c/return :state :state2)
           (tu/inject-state-change! (tu/find e (foobar))
                                    :state2)))))

(deftest effect-utils-test
  (let [called-with (atom nil)
        called (atom false)]
    (c/defn-effect mk-eff0 [v]
      (reset! called v)
      (reset! called-with v)
      (c/return))

    (def eff1 (c/effect (fn []
                          (reset! called true)
                          (c/return))))

    (is (tu/effect? eff1))
    (is (= nil (tu/effect-args eff1)))

    (is (tu/effect? (mk-eff0 42)))
    (is (tu/effect? (mk-eff0 42) mk-eff0))
    (is (= (mk-eff0 42) (mk-eff0 42)))
    (is (tu/effect-args (mk-eff0 42) [42]))

    (let [env (tu/env (dom/div))]
      (tu/mount! env nil)
      
      (tu/execute-effect! env eff1)
      (is @called)

      (reset! called false)
      (tu/execute-effect! env (mk-eff0 42))
      (is @called)
      (is (= 42 @called-with)))))

(deftest test-subscriptions-test
  ;; one can disable subscriptions, and inject actions instead.
  (let [sub (c/subscription (fn [& args]
                              (assert false "should not be called")))

        set-state (fn [st a] (c/return :state (inc a)))
        env (tu/env (-> (c/focus :nest (-> (c/dynamic #(if % sub c/empty))
                                           (c/handle-action set-state)))
                        (tu/disable-subscriptions [sub])))]

    (tu/mount! env {:nest true})

    (is (= (c/return :state {:nest 42})
           (tu/inject-action! (tu/find env sub) 41)))))

(deftest disable-subscriptions-test
  ;; one can disable all or individual subscriptions.
  (c/defn-subscription disable-subscriptions-test-2 deliver! [a]
    (reset! a true)
    (fn []
      (reset! a false)))
  
  (let [sub-1 (c/subscription (fn [& args]
                                (assert false "should not be called")))
        
        sub-2-running? (atom false)
        sub-2 (disable-subscriptions-test-2 sub-2-running?)]

    (let [env (tu/env (-> (c/fragment sub-1 sub-2)
                          (tu/disable-subscriptions)))]
      (tu/mount! env nil)
      (is (not @sub-2-running?)))

    (let [env (tu/env (-> (c/fragment sub-1 sub-2)
                          (tu/disable-subscriptions [sub-1])))]
      (reset! sub-2-running? false)
      (tu/mount! env nil)
      (is @sub-2-running?))))

(deftest emulate-subscriptions
  (let [sub-1 (c/subscription (fn [& args]
                                (assert false "should not be called")))
        env (tu/env (-> (c/fragment sub-1)
                        (tu/emulate-subscriptions (fn [eff]
                                                    (assert (tu/subscribe-effect? eff sub-1))
                                                    ::result))))]
    (is (= (c/return :action ::result)
           (tu/mount! env nil)))))

(deftest subscribe-effect-properties-test
  ;; then getting a subscribe-effect in hand, one can look at the function and args the subscription was created from.
  ;; via subscribe-effect?, subscribe-effect-fn and subscribe-effect-args
  
  (let [get-sub-eff (fn [item]
                      (let [sub-eff (atom nil)]
                        (tu/mount! (tu/env (-> item
                                               (tu/emulate-subscriptions (fn [eff]
                                                                           (reset! sub-eff eff)
                                                                           c/no-effect))))
                                   nil)
                        @sub-eff))]
    ;; either directly via core/subscription
    (let [f (fn [& args]
              (assert false "should not be called"))
          sub (c/subscription f :foo)
          eff (get-sub-eff sub)]
      (is (tu/subscribe-effect? eff sub))
      (is (= [:foo] (tu/subscribe-effect-args eff)))
      (is (= f (tu/subscribe-effect-fn eff))))

    ;; or via defn-subscription
    (c/defn-subscription subscription-properties-sub-2 deliver! [arg]
      (assert false "should not be called"))

    (let [f (fn [& args]
              (assert false "should not be called"))
          sub (subscription-properties-sub-2 :foo)
          eff (get-sub-eff sub)]

      (is (tu/subscribe-effect? eff sub))
      (is (= [:foo] (tu/subscribe-effect-args eff)))
      (is (= subscription-properties-sub-2 (tu/subscribe-effect-fn eff))))))
