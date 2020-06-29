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
            [reacl2.test-util.xpath :as rxpath]
            [active.clojure.functions :as f])
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

(defn env
  "Returns a new test environment to test the behavior of the given item."
  [item & [options]]
  ;; Note: this tests items using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [this-env (atom nil)
        ;; Note: this basically replicates impl/toplevel - can't reuse
        ;; that easily, because it would create another level (maybe
        ;; with a change to get-components?)
        action-reducer (fn [_ a]
                         (if (base/effect? a)
                           ;; execute effect, ignoring it's value
                           (let [[v ret] (base/run-effect! a)]
                             (impl/transform-return ret))
                           ;; or return actions
                           (rcore/return :action a)))
        class (rcore/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (rcore/return :message [(rcore/get-dom child) msg]))
                           render (-> (impl/instantiate (rcore/bind this) item)
                                      (rcore/refer child)
                                      (rcore/reduce-action action-reducer)))]
    (reset! this-env (r-tu/env class options))
    @this-env))

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

(defn with-env-return [env f]
  (->ret (r-tu/with-component-return (get-root-component env)
           (fn [comp]
             (f)))))

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

(defn effect?
  "Returns true if the given action is an effect action, and
  optionally if it was created by the given effect function."
  [a & [eff-defn]]
  (and (base/effect? a)
       (or (nil? eff-defn)
           (and (base/simple-effect? a) (= (base/effect-f a) eff-defn))
           (= (:reacl-c.core/effect-defn (meta a)) eff-defn))))

(defn simple-effect?
  "Returns true for effects created by [[core/effect]]
  or [[core/defn-effect]], but false for those created
  from [[core/seq-effects]]."
  [a & [eff-defn]]
  (and (effect? a eff-defn)
       (base/simple-effect? a)))

(defn effect-f
  "Returns the function implementing the effect behind the given simple effect action. See [[simple-effect?]]."
  [eff]
  (assert (base/simple-effect? eff))
  (:f eff))

(defn effect-args
  "Returns the arguments for the function returned by [[effect-f]]."
  [eff]
  (assert (base/simple-effect? eff))
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

(defn subscribe-effect-fn
  "The function passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect? eff))
  (let [f (first (effect-args eff))
        m (meta f)]
    ;; if it was created from a defn-subscription (instead of plain subscription), then return that function instead.
    (if (clojure.core/contains? m core/subscription-from-defn-meta-key)
      (core/subscription-from-defn-meta-key m)
      f)))

(defn subscribe-effect-args
  "The arguments passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect? eff))
  (second (effect-args eff)))

(defn ^:no-doc subscribe-effect-host
  [eff]
  (assert (subscribe-effect? eff))
  (let [[_ _ _ host] (effect-args eff)]
    host))

(let [h (fn [f state eff]
          (cond
            (subscribe-effect? eff)
            (let [action (f eff)]
              (if (or (nil? action)
                      (= action eff))
                (core/return :action eff)
                (core/return :message [(subscribe-effect-host eff)
                                       (core/->SubscribedEmulatedResult action)])))
            :else (core/return :action eff)))]
  (defn emulate-subscriptions
    "Returns an item, that for all subscriptions done in `item`,
  evaluates `(f subscription-effect)`. The
  function [[subscription-effect?]] can be used to check which kind of
  subscription effect it is. If it evaluates to `nil` or the same
  effect that was passed in, it is just passed on. If it evaluates to
  a different action, that action is emitted from the subscription
  itself, i.e. emulating a (synchronous) result of the subscription."
    [item f]
    ;; Note: subscription-effects will/should never be in a compose-effect.
    (core/handle-effect item
                        (f/partial h f))))

(let [disable-all-subs (fn [eff]
                         (cond
                           (subscribe-effect? eff) core/no-effect
                           :else eff))
      disable-subs (fn [subs eff]
                     (cond
                       (some #(subscribe-effect? eff %) subs) core/no-effect
                       :else eff))]
  (defn disable-subscriptions
    "Returns an item like `item`, but where all subscriptions (or the
  the given subscriptions) are not executed when emitted from `item`."
    ([item]
     (core/map-effects item disable-all-subs))
    ([item subs]
     (core/map-effects item (f/partial disable-subs subs)))))

(defn- find-ref [env ref]
  ;; Note: deref host would return the 'reacl component' instead of the test renderer component
  (let [comp (core/deref ref)]
    (let [tcomp (.find (get-root-component env)
                       (fn [ti]
                         (= (.-instance ti) comp)))]
      tcomp)))

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
