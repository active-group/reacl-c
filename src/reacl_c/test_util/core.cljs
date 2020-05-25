(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.generators :as gen]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl-c.test-util.xpath :as xpath]
            [reacl2.test-util.xpath :as rxpath])
  (:refer-clojure :exclude [resolve find contains? count]))

;; Note: just reusing rcore/test-util is not a good fit, esp. because
;; when/if xpath reveals much of the internals (dom class wrapper,
;; other classes that are an implementation detail). Maybe this should
;; be replaced by our own simulator.

(defn- ->ret [r]
  (assert (rcore/returned? r))
  ;; reacl return => reacl-c return
  (apply core/return
         (apply concat
                (let [s (rcore/returned-app-state r)]
                  (when-not (rcore/keep-state? s)
                    [:state s]))
                (map (fn [a]
                       [:action a])
                     (rcore/returned-actions r)))))

(defn combine-emulators
  "Combine two or more effect emualtors into one, to be used with [[env]]."
  [emu1 & more]
  (if (empty? more)
    emu1
    (let [emu2 (apply combine-emulators more)]
      (fn [env eff]
        (let [r (emu1 env eff)]
          (reduce (fn [rr act]
                    (base/merge-returned rr
                                         (if (base/effect? act)
                                           (emu2 env act)
                                           (core/return :action act))))
                  (assoc r :actions [])
                  (:actions r)))))))

(defn env
  "Returns a new test environment to test the behavior of the given item.
  Options map may include

  :emulator   a function (fn [env effect] (return ...)) that may handle
              effect action reaching the toplevel."
  [item & [options]]
  ;; Note: this tests items using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [this (atom nil)
        action-reducer (if-let [re (:emulator options)]
                         (fn [_ a]
                           (if (base/effect? a)
                             (impl/transform-return (re @this a))
                             (rcore/return :action a)))
                         (fn [_ a]
                           (rcore/return :action a)))
        class (rcore/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (rcore/return :message [(rcore/get-dom child) msg]))
                           render (-> (impl/instantiate (rcore/bind this) item)
                                      (rcore/refer child)
                                      (rcore/reduce-action action-reducer)))]
    (reset! this (r-tu/env class (-> options
                                     (dissoc :emulator))))
    @this))

(defn- get-root-component [env]
  (r-tu/get-component env))

(defn get-components [env]
  ;; we always wrap the "env" class around the main component.
  (let [env-comp (get-root-component env)]
    (array-seq (.-children env-comp))))

(defn get-component [env]
  (let [cs (get-components env)]
    (cond
      (empty? cs) (do (assert false "empty env") nil)
      (empty? (rest cs)) (first cs)
      :else
      (do (assert false "no unique toplevel component - is it a fragment?")
          nil))))

(defn- search-root [env]
  (if (r-tu/env? env)
    ;; Note: the root can have multiple children (if a fragment is used at toplevel)
    [(get-root-component env) rxpath/children]
    [env rxpath/self]))

;; TODO: helpers for when something cannot be found? (assert-exprs maybe?)

(defn find [env item]
  (let [[c base] (search-root env)]
    (rxpath/select c (rxpath/comp base rxpath/all (xpath/item item)))))

(defn find-named [env thing]
  (let [[c base] (search-root env)]
    (rxpath/select c (rxpath/comp base rxpath/all (xpath/named thing)))))

(defn find-all [env item]
  (let [[c base] (search-root env)]
    (rxpath/select-all c (rxpath/comp base rxpath/all (xpath/item item)))))

(defn find-all-named [env thing]
  (let [[c base] (search-root env)]
    (rxpath/select-all c (rxpath/comp base rxpath/all (xpath/named thing)))))

(defn describe-failed-find [env item]
  ;; this is still to be considered 'experimental'; hard to given a "good" tip
  
  (assert (nil? (find env item)))
  ;; TODO: if 'smaller-than' works correctly, then the limit of 1000 should not be necessary.
  ;; FIXME: how to set seed for sample-seq? might need to make our own via 'generate'.
  (loop [n 0
         smaller-list (take 1000 (gen/sample-seq (item-gen/smaller-than item)))]
    (if (or (empty? smaller-list) (= core/empty (first smaller-list)))
      nil ;; "Could not find out why."
      (if (find env (first smaller-list))
        ;; TODO: an item pretty print would be nice.
        ;; TODO: or use find-first-different between the first that can be found?
        (str "I could find the following item though: " (pr-str (first smaller-list)))
        (recur (inc n) (rest smaller-list))))))

(defn mount!
  "Mounts the item of the given test environment with the given
  state, and returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/mount! env state)))

(defn update!
  "Updates the state of the item of the given test environment, and
  returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/update! env state)))

(defn unmount!
  "Unmounts the item of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (->ret (r-tu/unmount! env)))

(defn push!
  "Apply the state change in the given 'return' value, if there is
  any, and merge the return value resulting from that - pushing the
  update cycle one turn."
  [env ret]
  (->ret (r-tu/push! env (impl/transform-return ret))))

(defn push!!
  "Recursively apply the state change in the given 'return' value,
  until the state does not change anymore."
  [env ret]
  (->ret (r-tu/push!! env (impl/transform-return ret))))

(defn update!!
  "Updates the state of the item of the given test environment, and
  if the state is changed in reaction to that, then keeps on updating
  it. Returns actions and the final changed state, if it was changed
  at all. Throws if there are more than *max-update-loops* recursions,
  which are a sign for bug in the item."
  [env state]
  (->> (update! env state)
       (push!! env)))

(defn mount!!
  "Like [[mount!]], but also recursively update the item to new states that are returned."
  [env state]
  (->> (mount! env state)
       (push!! env)))

(defn unmount!!
  "Like [[unmount!]], but also recursively update the item to new states that are returned."
  [env]
  (->> (unmount! env)
       (push!! env)))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  {:per [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string item gives weird reacl errors.
  (->ret (r-tu/send-message! comp msg)))

(defn send-message!!
  "Like [[send-message!]], but also recursively update the item to new states that are returned."
  [comp msg]
  {:per [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string item gives weird reacl errors.
  (->ret (r-tu/send-message!! comp msg)))

(defn- dom-node? [comp]
  (string? (.-type comp)))

(defn invoke-callback!
  "Invokes the function assiciated with the given `callback` of the
  given test component of a dom element (e.g. `:onclick`), with the
  given event object, and returns a changed app-state and actions from
  the toplevel item, in the form of a `reacl/return` value."
  [comp callback event]
  (assert (dom-node? comp) (str "Must be a dom node to invoke callbacks: " (pr-str comp)))
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn invoke-callback!!
  "Like [[invoke-callback!]], but also recursively update the item to new states that are returned."
  [comp callback event]
  (assert (dom-node? comp) (str "Must be a dom node to invoke callbacks: " (pr-str comp)))
  (->ret (r-tu/invoke-callback!! comp callback event)))

(defn- inject-return_ [comp ret f]
  {:pre [(some? comp)]}
  (let [comp (if (dom-node? comp)
               ;; When a dom item was selected, we have the raw dom node here;
               ;; to injecting a return, there must be a wrapper class
               ;; around it.
               ;; Also, it must be the corresponding dom wrapper class, because
               ;; otherwise the return could be different (e.g. if it's a
               ;; handle-action, returning from there would be really different)
               (let [p (.-parent comp)]
                 (when-not (and p (.-type p) (impl/dom-class-for-type (.-type comp)))
                   (assert false "The given node is a dom node without any event handlers attached. It's not possible to inject something there."))
                 p)
               comp)]
    (->ret (f comp (impl/transform-return ret)))))

(defn inject-return!
  "Does the things that would happen if the given component returned
  the given 'return' value in reaction to some discrete event, and
  returns what would be emitted (state and/or actions) from the tested
  item, in the form of a 'return' value."
  [comp ret]
  (inject-return_ comp ret r-tu/inject-return!))

(defn inject-return!!
  "Like [[inject-return!]], but also recursively update the item to new states that are returned."
  [comp ret]
  (inject-return_ comp ret r-tu/inject-return!!))

(defn inject-action!
  "Does the things that would happen if the given component emitted
  the given action, and returns what would be emitted (state and/or
  actions) from the tested item, in the form of a 'return' value."
  [comp action]
  (inject-return! comp (core/return :action action)))

(defn inject-action!!
  "Like [[inject-action!]], but also recursively update the item to new states that are returned."
  [comp action]
  (inject-return!! comp (core/return :action action)))

(defn inject-state-change!
  "Does the things that would happen if the given component emitted
  the given new state, and returns what would be emitted (state and/or
  actions) from the tested item, in the form of a 'return' value."
  [comp state]
  (inject-return! comp (core/return :state state)))

(defn inject-state-change!!
  "Like [[inject-state-change!]], but also recursively update the item to new states that are returned."
  [comp state]
  (inject-return!! comp (core/return :state state)))

;; TODO: inspect state of an 'inner component'?

(defn execute-effect!
  "Executed the given effect in the given test environment, and return
  toplevel changes as a 'return' value."
  [env eff]
  (assert (base/effect? eff) eff)
  (inject-return! (get-root-component env)
                  (let [[value ret] (base/run-effect! eff)]
                    ret)))

(def execute-effects-emulator execute-effect!) ;; ...bit of a silly name 'emulate by doing the real thing'.

(defn effect?
  "Returns true if the given action is an effect action, and
  optionally if it was created by the given effect function."
  [a & [eff-defn]]
  (and (base/effect? a)
       (or (nil? eff-defn)
           (= (base/effect-f a) eff-defn)
           (= (:reacl-c.core/effect-defn (meta a)) eff-defn))))

(defn effect-f
  "Returns the function implementing the effect behind the given effect action."
  [eff]
  (assert (effect? eff))
  (:f eff))

(defn effect-args
  "Returns the arguments for the function returned by [[effect-f]]."
  [eff]
  (assert (effect? eff))
  (:args eff))

(defn subscribe-effect?
  "Tests if the given effect, is one that is emitted by a subscription
  equal to the given one on mount. This can be useful in unit tests."
  ([eff]
   (effect? eff core/subscribe!))
  ([eff subs]
   (and (effect? eff core/subscribe!)
        (let [e-args (effect-args eff)
              reconstr (apply core/subscription (first e-args) (second e-args))]
          ;; the first arg is the subs-f, the second arg it's user args.
          ;; creating a new subscription with same args, should be an = item then.
          (or (= subs reconstr)
              ;; effects, as created by a defn-subscription, are slightly more wrapped (also named!):
              (when-let [real-sub (and (base/named? subs) (:e subs))]
                (= real-sub reconstr)))))))

(defn subscribe-effect-args
  "The arguments passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect? eff))
  (second (effect-args eff)))

(defn unsubscribe-effect?
  "Tests if the given effect, is one that is emitted by a subscription
  equal to the given one, on unmount. This can be useful in unit
  tests."
  ([eff]
   (effect? eff core/unsubscribe!))
  ([eff subs]
   (and (effect? eff core/unsubscribe!)
        (let [e-args (effect-args eff)
              reconstr (apply core/subscription (second e-args))]
          ;; the second arg is the subs-f and user args.
          ;; creating a new subscription with same args, should be an = item then.
          (or (= subs reconstr)
              (when-let [real-sub (and (base/named? subs) (:e subs))]
                (= real-sub reconstr)))))))

(defn unsubscribe-effect-args
  "The arguments passed to the subscription the given unsubscribe effect was generated from."
  [eff]
  (assert (unsubscribe-effect? eff))
  (rest (second (effect-args eff))))

(defn- find-ref [env ref]
  ;; Note: deref host would return the 'reacl component' instead of the test renderer component
  (let [comp (core/deref ref)]
    (let [tcomp (.find (get-root-component env)
                       (fn [ti]
                         (= (.-instance ti) comp)))]
      tcomp)))

(defn subscription-start!
  "Begins a simulated execution of the subscription given by the given
  effect that resulted from mounting
  it (see [[subscribe-effect?]]). The optional given `stop-fn!` will
  be called when the [[unsubscribe-effect?]] returned on unmount is
  executed."
  [env sub-eff & [stop-fn!]]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)
        comp (core/deref host)]
    (assert (some? comp) "Subscription not mounted or already unmounted?")
    (let [tcomp (find-ref env host)
          r (send-message! tcomp (core/->SubscribedMessage (or stop-fn! (fn [] nil))))]
      (assert (= (core/return) r)) ;; subscriptions don't expose anything.
      nil)))

(defn ^:no-doc subscription-start-return
  [sub-eff & [stop-fn!]]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)]
    (core/return :message [host (core/->SubscribedMessage (or stop-fn! (fn [] nil)))])))

(defn ^:no-doc subscription-result-return
  [sub-eff action]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)]
    ;; 'deliver!' only allowed in async context; this should do the same:
    (core/return :message [host (core/->SubscribedEmulatedResult action)])))

(defn subscription-result!
  "For the effect that resulted from mounting a
  subscription (see [[subscribe-effect?]]), inject the given action as
  a result of the subscription item, and return what the tested
  component in the given test environment returns."
  [env sub-eff action]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)]
    (->ret (r-tu/with-component-return (get-root-component env)
             (fn [comp]
               (deliver! action))))))

(defrecord ^:private SubscriptionEmulatorEnv [sub running])

(defn subscription-emulator-env
  "Create a subscription emulation environment for items like the
  given subscription."
  [sub]
  {:pre [(base/item? sub)]} ;; must even be a subscription item... if that could be tested?
  (map->SubscriptionEmulatorEnv {:sub sub
                                 :running (atom nil)}))

(defn subscription-emulator-inject!
  "Make the emulated subscription emit the given action."
  [subs-env action]
  (let [{sub :sub running :running} subs-env]
    (when-not (some? @running)
      (throw "Subscription not running? Forgot to set the :emulator in env?"))
    (if-let [[env eff] @running]
      (subscription-result! env eff action)
      (throw (ex-info "Subscription not mounted?" {:sub sub})))))

(defn subscription-emulator-running?
  "Checks if the subscription emulation environment is attached as
  the :emulator to a testing environment and a subscription is
  mounted."
  [subs-env]
  (let [{running :running} subs-env]
    (some? @running)))

(defn subscription-emulator
  "Returns the emulator function to be set as the :emulator option of
  a testing environemnt."
  [subs-env]
  (let [{sub :sub running :running} subs-env
        running! (fn [v] (reset! running v) nil)]
    (fn [env eff]
      (cond
        (subscribe-effect? eff sub)
        (do (running! [env eff])
            (subscription-start-return eff (fn [] (running! nil))))

        (unsubscribe-effect? eff sub)
        (do (execute-effect! env eff)
            (running! nil)
            (core/return))

        :else (core/return :action eff)))))


(defn preventing-error-log
  "Prevents a log message about an exception during the evaluation of
  `thunk`, which occurs even when the error is handled in an error
  boundary."
  [thunk]
  ;; React does some fancy things with the browsers error
  ;; handler in DEV, and respects 'default prevented' in
  ;; that it does not log the error then (or is it the browser?)
  (let [eh (fn [ev]
             (.preventDefault ev))]
    (js/window.addEventListener "error" eh)
    ;; and this suppressed the 'The above error occurred' log msg from React.
    (let [pre js/console.error]
      (set! js/console.error (fn [& args] nil))
      (try (thunk)
           (finally
             (set! js/console.error pre)
             (js/window.removeEventListener "error" eh))))))
