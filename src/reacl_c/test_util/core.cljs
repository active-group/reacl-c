(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.impl.tu-reacl :as impl]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.generators :as gen]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [find]))

(defn env
  "Returns a new test environment to test the behavior of the given item."
  [item & [options]]
  (impl/env item options))

(defn get-components [env]
  (impl/get-components env))

(defn get-component [env]
  (let [cs (get-components env)]
    (cond
      (empty? cs) (do (assert false "empty env") nil)
      (empty? (rest cs)) (first cs)
      :else
      (do (assert false "no unique toplevel component - is it a fragment?")
          nil))))

;; TODO: helpers for when something cannot be found? (assert-exprs maybe?)

(defn- ensure-some [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) (first lst)
    :else (throw (js/Error. "More than one node matches the given path."))))

(defn find-all [env item]
  (impl/find-all env item))

(defn find [env item]
  (ensure-some (find-all env item)))

(defn find-all-named [env thing] ;; TODO: remove?
  (impl/find-all-named env thing))

(defn find-named [env thing] ;; TODO: remove?
  (ensure-some (find-all-named env thing)))

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
  (impl/with-env-return env f))

(defn mount!
  "Mounts the item of the given test environment with the given
  state, and returns actions and maybe a changed state."
  [env state]
  (impl/mount! env state))

(defn update!
  "Updates the state of the item of the given test environment, and
  returns actions and maybe a changed state."
  [env state]
  (impl/update! env state))

(defn unmount!
  "Unmounts the item of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (impl/unmount! env))

(defn push!
  "Apply the state change in the given 'return' value, if there is
  any, and merge the return value resulting from that - pushing the
  update cycle one turn."
  [env ret]
  (let [st (base/returned-state ret)]
    (if (not (= base/keep-state st))
      (base/merge-returned ret (update! env st))
      ret)))

(def ^:dynamic *max-update-loops* 100)

(defn push!!
  "Recursively apply the state change in the given 'return' value,
  until the state does not change anymore."
  [env ret]
  (loop [r ret
         state base/keep-state
         n 1]
    (when (> n *max-update-loops*)
      (throw (ex-info "Component keeps on updating. Check the livecylcle methods, which should eventually reach a fixed state." {:intermediate-state state})))
    (let [st (base/returned-state r)]
      (if (not= state st)
        (recur (push! env r) st (inc n))
        r))))

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

(defn invoke-callback!
  "Invokes the function assiciated with the given `callback` of the
  given test component of a dom element (e.g. `:onclick`), with the
  given event object, and returns a changed app-state and actions from
  the toplevel item, in the form of a `reacl/return` value."
  [comp callback event]
  (impl/invoke-callback! comp callback event))

(defn invoke-callback!!
  "Like [[invoke-callback!]], but also recursively update the item to new states that are returned."
  [comp callback event]
  (impl/invoke-callback!! comp callback event))

(defn inject-return!
  "Does the things that would happen if the given component returned
  the given 'return' value in reaction to some discrete event, and
  returns what would be emitted (state and/or actions) from the tested
  item, in the form of a 'return' value."
  [comp ret]
  (impl/inject-return! comp ret))

(defn inject-return!!
  "Like [[inject-return!]], but also recursively update the item to new states that are returned."
  [comp ret]
  (impl/inject-return!! comp ret))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  (impl/send-message! comp msg))

(defn send-message!!
  "Like [[send-message!]], but also recursively update the item to new states that are returned."
  [comp msg]
  (impl/send-message!! comp msg))

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
  (inject-return! (get-component env)
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

(defn- subscribe-effect-1? [eff]
  (effect? eff core/subscribe!))

(defn subscribe-effect-fn
  "The function passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect-1? eff))
  (first (effect-args eff)))

(defn subscribe-effect-args
  "The arguments passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect-1? eff))
  (second (effect-args eff)))

(defn- subscribe-effect-subscription [eff]
  (assert (subscribe-effect-1? eff))
  (let [subs-f (subscribe-effect-fn eff)
        subs-args (subscribe-effect-args eff)]

    ;; but if subs if created via a call to to a defn-subscription fn, then it will look different
    (if-let [f (core/subscription-from-defn-meta-key (meta eff))]
      (= subs (apply f subs-args))
      (apply core/subscription subs-f subs-args))))

(defn subscribe-effect?
  "Tests if the given effect, is one that is emitted by a subscription
  equal to the given one on mount. This can be useful in unit tests."
  ([eff]
   (subscribe-effect-1? eff))
  ([eff subs]
   (and (effect? eff core/subscribe!)
        (let [subs-f (subscribe-effect-fn eff)
              subs-args (subscribe-effect-args eff)]
          ;; the first arg is the subs-f, the second arg it's user args.
          ;; creating a new subscription with same args, should be an = item then.
          (or (= subs (apply core/subscription subs-f subs-args))
              ;; but if subs if created via a call to to a defn-subscription fn, then it will look different
              (if-let [f (core/subscription-from-defn-meta-key (meta eff))]
                (= subs (apply f subs-args))
                false))))))

(defn ^:no-doc subscribe-effect-host
  [eff]
  (assert (subscribe-effect? eff))
  (let [[_ _ _ host] (effect-args eff)]
    host))

(defn- subscription-f-args [sub]
  ;; this is quite hacky, because subscription are not first-class items, we match/extract the function and args from the items constructed in core/subscription.
  ;; but as long as there is a test for it, we just have to keep that in sync with the impl.
  (let [sub (if (base/named? sub)
              ;; subs from defn-subscription are also named:
              (base/named-e sub)
              sub)]
    (and (base/with-async-return? sub)
         (let [[a1 _] (base/with-async-return-args sub) ;; async-actions => async-return
               [_ _ f args] (:args a1)] ;; a1 = Partial fn with args [action-mapper defn-f f args]   (could replace it with some other Ifn to not depend on active-clojure internals)
           (assert (ifn? f))
           (when (ifn? f)
             [f args])))))

(defn- subscription-1?
  [v]
  (and (base/item? v)
        (some? (subscription-f-args v))))

(defn subscription-f
  "Returns the function implementing the given subscription item."
  [sub]
  (assert (subscription-1? sub))
  (first (subscription-f-args sub)))

(defn subscription-args
  "Returns extra arguments for the function implementing the given subscription item. See [[subscription-f]]"
  [sub]
  (assert (subscription-1? sub))
  (second (subscription-f-args sub)))

(defn subscription?
  "Returns whether `item` is a subscription item, optinally also
  checking if it was created by the given `defn-subscription` function
  or with the given function `f` as its implementation."
  ([v]
   (and (base/item? v)
        (some? (subscription-f-args v))))
  ([v f]
   (assert (ifn? f))
   (and (subscription? v)
        (or (= f (subscription-f v))
            ;; uuuh, if f is acually a 'defn-subscription' fn, then calling is does no harm, but if it isn't?
            ;; being pragmatic here, and just look if it is a 'named'
            ;; thingy too and that nobody passes something very different in as `f`.
            (and (some? (core/meta-name-id f))
                 (= v (apply f (subscription-args v))))))))


(defn ^{:arglists '([subscribe-effect f & args]
                    [subscribe-effect subscription])
        :no-doc true   ;; or even private? map-subscription should cover all usecases.
        }
  replace-subscription
  "Given the subscribe effect of a subscription item, this returns a
  modified effect that subscribes to the given subscription
  instead. Alernatively, a function `f ` and args, that implement a
  subscription can be given, corresponding
  to [[core/subcription]]. Use this in a [[core/map-effects]] item, to
  mock subscribe effects in a test setup."
  [subscribe-effect f & args]
  
  (assert (subscribe-effect? subscribe-effect))
  (let [[f args] (or (subscription-f-args f)
                     [f args])]
    (update subscribe-effect :args
            (fn [[_ _ deliver! host action-mapper]]
              ;; Note: action-mapper contains the schema validation when specified in a defn-subscription.
              [f args deliver! host identity]))))

(let [pre (fn [f eff]
            (if-let [r (and (subscribe-effect? eff)
                            (f (subscribe-effect-subscription eff)))]
              (replace-subscription eff r)
              eff))]
  (defn map-subscriptions
  "Returns an item that replaces subscriptions in `item`. Note that
  `item` itself remains unchanged; instead, whenever a subscription is
  activated in `item`, `f` will be called with that subscription, and
  if it returns a different subscription, then that is activated
  instead. If it returns nil, then the original subscription will be
  actived.

  You can use [[subscription?]] and [[subscription-args]], if the
  replacement depends on details of the subscription or for mapping a
  whole group of subscriptions."
    [item f]
    (core/map-effects item (f/partial pre f))))

;; Note: I think we can remove emaulate-subscriptions and disable-subscriptions: map-subscription and utils are superiour.

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

(defn preventing-error-log ;; TODO: -> other namespace; add a test fixture for it?
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
