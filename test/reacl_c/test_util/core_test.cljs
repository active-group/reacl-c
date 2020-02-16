(ns reacl-c.test.test-util.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [reacl-c.test-util.core :as tu]
            [reacl-c.impl.reacl :as impl]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest find-items-test
  (let [find (fn [pat item]
               (let [env (tu/env item)]
                 (tu/mount! env nil)
                 (some? (tu/find env pat))))]
    (is (find c/empty (dom/div)))
    (is (find c/empty (c/fragment (dom/div))))
    ;; can't find something in nothing :-/
    (is (find c/empty c/empty))
    (is (find c/empty (c/fragment c/empty)))

    (is (find (c/fragment (dom/div)) (dom/div)))
    (is (find (c/fragment (dom/div)) (c/fragment (dom/div))))

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
    (is (find (c/dynamic (c/constantly (dom/div)) :a) (c/dynamic (c/constantly (dom/div)) :a)))

    ;; dom ignores others (= structural match)
    (is (find (dom/div) (c/dynamic (c/constantly (dom/div)))))
    (is (find (dom/div (dom/span)) (c/dynamic (c/constantly
                                               (dom/div (c/dynamic (c/constantly
                                                                    (dom/span))))))))
    (is (find (dom/div (dom/span) (dom/span)) (dom/div (dom/span) (dom/span))))
    (is (not (find (dom/div (dom/span) (dom/span)) (dom/div (dom/span)))))
    (is (find (dom/div "foo") (dom/div (c/dynamic (c/constantly "foo")))))

    ;; regression
    (is (find (dom/div {:- "Q", :b- "û"}
                       (dom/br (c/handle-message (c/constantly (c/return))
                                                 (dom/br))))
              (dom/div {:- "Q", :b- "û"}
                       (dom/br (c/handle-message (c/constantly (c/return))
                                                 (dom/br))))))
    ))

(deftest mount-test
  (is (= (c/return)
         (-> (tu/env (dom/div))
             (tu/mount! :state))))
  (is (= (c/return :action ::act)
         (-> (tu/env (c/once (c/constantly (c/return :action ::act))))
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
         (let [e (tu/env (c/once (c/constantly (c/return)) (c/constantly (c/return :action ::act))))]
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

(deftest subscription-utils-test
  (let [started (atom false)
        stopped (atom false)
        sub-f (fn [deliver!]
                (reset! started true)
                (fn []
                  (reset! stopped true)))
        sub (c/subscription sub-f)
        sub2 (c/subscription sub-f)
        env (tu/env sub)]
    (is (= sub sub2))
    
    (let [r (tu/mount! env nil)
          eff (first (:actions r))]
      (is (tu/subscribe-effect? eff))
      (is (tu/subscribe-effect? eff sub))
      (is (tu/subscribe-effect? eff sub2))

      ;; Nothing would happen on unmmount, if effect not executed.
      (tu/execute-effect! env eff)
      (is @started))
    
    (let [r (tu/unmount! env)
          eff (first (:actions r))]
      (is (tu/unsubscribe-effect? eff))
      (is (tu/unsubscribe-effect? eff sub))
      (is (tu/unsubscribe-effect? eff sub2))

      (tu/execute-effect! (doto (tu/env c/empty)
                            (tu/mount! nil))
                          eff)
      (is @stopped)))
  
  (do
    (c/defn-subscription sub-test-2 deliver! [a]
      (fn []))

    (is (= (sub-test-2 :foo) (sub-test-2 :foo)))
    
    (let [env (tu/env (c/dynamic #(if % (sub-test-2 :foo) c/empty)))]
      (let [r (tu/mount! env true)
            eff (first (:actions r))]
        (is (some? eff))

        (is (tu/subscribe-effect? eff (sub-test-2 :foo)))))))

(deftest subscription-result-test
  (let [sub (c/subscription (fn [& args]
                              (assert false "should not be called")))
        env (tu/env (c/dynamic #(if % sub c/empty)))]
    (let [r (tu/mount! env true)
          eff (first (:actions r))
          stopped? (atom false)]
      (is (some? eff))

      (is (tu/subscribe-effect? eff sub))

      (tu/subscription-start! env eff (fn [] (reset! stopped? true)))
      (is (= (c/return :action ::test)
             (tu/subscription-result! env eff ::test)))

      (is (not @stopped?))
      (let [r (tu/update! env false)]
        (tu/execute-effect! env (first (:actions r))))
      (is @stopped?))))

(deftest emulate-subscription-test
  (let [sub (c/subscription (fn [& args]
                              (assert false "should not be called")))

        emu (tu/subscription-emulator-env sub)
        
        env (tu/env (c/dynamic #(if % sub c/empty))
                    {:emulator (tu/subscription-emulator emu)})]

    (is (not (tu/subscription-emulator-running? emu)))

    (tu/mount! env true)
    (is (tu/subscription-emulator-running? emu))

    (is (= (c/return :action ::test)
           (tu/subscription-emulator-inject! emu ::test)))
    (is (= (c/return :action 42)
           (tu/subscription-emulator-inject! emu 42)))
    
    (tu/update! env false)
    (is (not (tu/subscription-emulator-running? emu)))

    (tu/mount! env true)
    (tu/unmount! env)
    (is (not (tu/subscription-emulator-running? emu)))))

