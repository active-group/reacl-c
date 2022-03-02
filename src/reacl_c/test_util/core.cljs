(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs-async.core :as async]
            [cljs-async.cljs.core :as async-cljs]))

(letfn [(conj-into [atom a]
          (core/effect swap! atom conj a))]
  (defn collect-actions
    "Returns an item that is like `item`, but will capture all emitted actions and
  conj them into the given atom instead."
    [item atom]
    (core/map-actions item (f/partial conj-into atom))))

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
  (base/effect-f eff))

(defn effect-args
  "Returns the arguments for the function returned by [[effect-f]]."
  [eff]
  (assert (base/simple-effect? eff))
  (base/effect-args eff))

(defn run-effect!
  "Returns a tuple `[value ret]`. If an effect returns a [[reacl-c.core/return]]
  value, then 'value' is the returned state, and 'ret' everything else.
  For any other value, 'ret' is empty."
  [eff]
  (base/run-effect! eff))

(defn- subscribe-effect-1? [eff]
  (effect? eff core/subscribe!))

(defn- subscribe-effect-fn
  "The function passed to the subscription the given subscribe effect was generated from."
  [eff]
  (assert (subscribe-effect-1? eff))
  (first (effect-args eff)))

(defn- subscribe-effect-args
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
      (apply f subs-args)
      (apply core/subscription subs-f subs-args))))

(defn- subscribe-effect?
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

(defn- subscription-f-args [sub]
  (when-let [[defn-f f args] (core/subscription-deconstruct sub)]
    [f args]))

(defn- subscription-1?
  [v]
  (some? (core/subscription-deconstruct v)))

(defn subscription?
  "Returns whether `item` is a subscription item, optinally also
  checking if it was created by the given `defn-subscription` function
  or with the given function `f` as its implementation."
  ([v]
   (some? (core/subscription-deconstruct v)))
  ([v f]
   (assert (ifn? f))
   (when-let [[s-defn-f s-f s-args] (core/subscription-deconstruct v)]
     (or (= f s-f)
         (= f s-defn-f)))))

(defn subscription-f
  "Returns the function implementing the given subscription item."
  [sub]
  (assert (subscription? sub))
  (second (core/subscription-deconstruct sub)))

(defn subscription-args
  "Returns extra arguments for the function implementing the given subscription item. See [[subscription-f]]"
  [sub]
  (assert (subscription? sub))
  (second (rest (core/subscription-deconstruct sub))))

(defn ^{:arglists '([subscribe-effect f & args]
                    [subscribe-effect subscription])
        :private true ;; use map-subscriptions
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
    (lens/overhaul subscribe-effect base/effect-args
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

(defn run-subscription!
  "Calls the function implementing the given subscription with the given
  `deliver!` function, returning the stop function for it."
  [sub deliver!]
  (let [[f args] (subscription-f-args sub)]
    (apply f deliver! args)))

(defn- run-subscription-async [sub]
  ;; starts the subscription and returns [stop-fn, value-promise]
  ;; where value-promise is promise of [value, next-value-promise] and
  ;; so on.
  (let [first-value (async-cljs/promise)
        next-value (atom first-value)
        stop! (run-subscription! sub (fn deliver! [action]
                                       (let [p @next-value
                                             n (async-cljs/promise)]
                                         (reset! next-value n)
                                         (async-cljs/deliver p [action (async-cljs/async-deref n)]))))]
    [stop! (async-cljs/async-deref first-value)]))

(defn- subscription-results-next [akku done? x]
  (let [[v next-value] x
        lst (swap! akku conj v)]
    (if (done? lst)
      lst
      (-> next-value
          (async/then (partial subscription-results-next akku done?))))))

(defn subscription-results
  "Runs the given subscription, asynchronously waiting for at least the
  given number of actions emitted by it (or the given timeout
  elapses), and then stops the subscription. Returns a promise of the
  sequence of actions."
  [sub num-actions & [timeout-ms]]
  (assert (number? num-actions))
  (let [akku (atom [])
        done? (fn [lst]
                (>= (count lst) num-actions))
        
        [stop! first-value] (run-subscription-async sub)
        values (-> first-value
                   (async/then (partial subscription-results-next akku done?)))]
    (-> (if timeout-ms
          (async/race values
                      (-> (async/timeout timeout-ms)
                          (async/then (fn [_]
                                        ;; on timeout, return what we have to far.
                                        @akku))))
          values)
        (async/finally stop!))))

(defn preventing-error-log
  "Prevents a log message about an exception during the evaluation of
  `thunk`, which occurs even when the error is handled in an error
  boundary. If `thunk` returns a promise, then error logs are enabled
  after that completes instead."
  [thunk]
  ;; React does some fancy things with the browsers error
  ;; handler in DEV, and respects 'default prevented' in
  ;; that it does not log the error then (or is it the browser?)
  (let [eh (fn [ev]
             (.preventDefault ev))]
    (js/window.addEventListener "error" eh)
    ;; and this suppressed the 'The above error occurred' log msg from React.
    (let [pre js/console.error
          restore-sync? (atom true)
          restore (fn []
                    (set! js/console.error pre)
                    (js/window.removeEventListener "error" eh))]
      (set! js/console.error (fn [& args] nil))
      (try (let [x (thunk)]
             (if (async/promise? x)
               (do (reset! restore-sync? false)
                   (async/finally x restore))
               x))
           (finally
             (when @restore-sync?
               (restore)))))))

;; TODO
#_(def validate-schemas-async
  (at/simple-async-fixture
   (fn [init-done]
     (if (s/fn-validation?)
       (init-done (fn [done] (done)))
       (do (s/set-fn-validation! true)
           (init-done (fn [done]
                        (s/set-fn-validation! false)
                        (done))))))))
