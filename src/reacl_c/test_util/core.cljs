(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl-c.test-util.xpath :as xpath])
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

(defn get-component [env]
  ;; we always wrap the "env" class around the main component.
  (let [env-comp (get-root-component env)]
    (first (array-seq (.-children env-comp)))))

(defn- to-component [env]
  (if (r-tu/env? env)
    (get-component env)
    env))

;; TODO: helpers for when something cannot be found? (assert-exprs maybe?)

(defn find [env item]
  (xpath/select (to-component env) (xpath/comp xpath/all item)))

(defn find-all [env item]
  (xpath/select-all (to-component env) (xpath/comp xpath/all item)))


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

(def ^:dynamic *max-update-loops* 100)

(defn push!
  [env ret]
  (if (not= base/keep-state (:state ret))
    (base/merge-returned ret (update! env (:state ret)))
    ret))

(defn push!!
  [env ret]
  (loop [r ret
         state base/keep-state
         n 1]
    (when (> n *max-update-loops*)
      (throw (ex-info "Item keeps on updating. Check any [[once]] items, which should eventually reach a fixed state." {:intermediate-state state})))
    (if (not= state (:state r))
      (recur (push! env r) (:state r) (inc n))
      r)))

(defn update!!
  "Updates the state of the item of the given test environment, and
  if the state is changed in reaction to that, then keeps on updating
  it. Returns actions and the final changed state, if it was changed
  at all. Throws if there are more than *max-update-loops* recursions,
  which are a sign for bug in the item."
  [env state]
  (push!! env (update! env state)))

(defn mount!! [env state]
  (push!! env (mount! env state)))

(defn unmount!! [env]
  (push!! env (unmount! env)))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  {:per [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string item gives weird reacl errors.
  (->ret (r-tu/send-message! comp msg)))

(defn- dom-node? [comp]
  (string? (.-type comp)))

(defn invoke-callback! [comp callback event]
  (assert (dom-node? comp) (str "Must be a dom node to invoke callbacks: " (pr-str comp)))
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-return! [comp ret]
  (let [comp (if (dom-node? comp)
               ;; When a dom item was selected, we have the raw dom node here;
               ;; to injecting a return, there must be a wrapper class
               ;; around it.
               ;; Also, it must be the corresponding dom wrapper class, because
               ;; otherwise the return could be different (e.g. if it's a
               ;; handle-action, returning from there would be really different)
               (let [p (.-parent comp)]
                 (when-not (and (.-type p) (impl/dom-class-for-type (.-type comp)))
                   (assert false "The given node is a dom node without any event handlers attached. It's not possible to inject something there.")))
               comp)]
    (->ret (r-tu/inject-return! comp (impl/transform-return ret)))))

(defn inject-action! [comp action]
  (inject-return! comp (core/return :action action)))

(defn inject-state-change! [comp state]
  (inject-return! comp (core/return :state state)))

(defn execute-effect!
  "Executed the given effect in the given test environment."
  [env eff]
  (assert (base/effect? eff))
  (inject-return! (get-root-component env)
                  (apply (:f eff) (:args eff))))

(defn effect? [a & [eff-defn]]
  (and (base/effect? a)
       (or (nil? eff-defn)
           (= (:f a) eff-defn)
           (= (:reacl-c.core/effect-defn (meta a)) eff-defn))))

(defn effect-f [eff]
  (assert (effect? eff))
  (:f eff))

(defn effect-args [eff]
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

(defn subscription-start!
  "Begins a simulated execution of the subscription given by the given
  effect that resulting from mounting
  it (see [[subscribe-effect?]]). The optional given `stop-fn!` will
  be called when the [[unsubscribe-effect?]] returned on unmount is
  executed."
  [env sub-eff & [stop-fn!]]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)
        comp (core/deref host)]
    (assert (some? comp) "Subscription not mounted or already unmounted?")
    ;; Note: deref host would return the 'reacl component' instead of the test renderer component; need to search for it:
    (let [tcomp (.find (get-root-component env)
                       (fn [ti]
                         (= (.-instance ti) comp)))]
      (let [r (send-message! tcomp (core/->SubscribedMessage (or stop-fn! (fn [] nil))))]
        (assert (= (core/return) r)) ;; subscriptions don't expose anything.
        nil))))

(defn subscription-start-return
  [env sub-eff & [stop-fn!]]
  (assert (effect? sub-eff core/subscribe!))
  ;; = (effect subscribe! f args deliver! host)
  (let [[f args deliver! host] (effect-args sub-eff)]
    (core/return :message [host (core/->SubscribedMessage (or stop-fn! (fn [] nil)))])))

(defn subscription-result-return
  [env sub-eff action]
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
        (do (subscription-start! env eff (fn [] (running! nil)))
            (running! [env eff])
            (core/return))

        (unsubscribe-effect? eff sub)
        (do (execute-effect! env eff)
            (core/return))

        :else (core/return :action eff)))))


(defn preventing-error-log
  "Prevents a log message about an exception during the evaluation of
  `thunk`, which occurs event when the error is handled in an error
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
