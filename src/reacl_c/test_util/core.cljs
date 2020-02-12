(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [clojure.data :as data]
            [active.clojure.lens :as lens]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath])
  (:refer-clojure :exclude [resolve]))

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
  "Combine two or more effect emualtors into one."
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

(defn invoke-callback! [comp callback event]
  ;; TODO: enable this on dom class items?! then we can remove the xpath case for the raw dom element.
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-return! [comp ret]
  (->ret (r-tu/inject-return! comp (impl/transform-return ret))))

(defn inject-action! [comp action]
  ;; Note: for dom tags in an xpath, the user will find the native element; so actions cannot be injected into them :-/
  ;; Rule: dom element => invoke-callback; non-dom items => inject-action + inject-state-change.
  ;; TODO: document that, if it cannot be changed.
  (inject-return! comp (core/return :action action)))

(defn inject-state-change! [comp state] ;; TODO: rename inject-change! (= reacl/tu)
  (inject-return! comp (core/return :state state)))

(defn execute-effect!
  "Executed the given effect in the given test environment."
  [env eff]
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


(def ^:private dummy-ref (reify base/Ref
                           (-deref-ref [this] (throw (ex-info "References must only be dereferenced in handlers, not during rendering." {})))))

(defn- dummy-return! [v]
  (throw (ex-info "Asynchronous return delivery must only be done asynchronously, not during rendering." {:value v})))

(defn- yank [state lens]
  ;; TODO: add a way in reacl or core, to do a manual focus (respecting keywords and numbers; or add numbers to active.clojure?)
  (if (integer? lens)
    (first (drop lens state))
    (lens/yank state lens)))

(defn ^:no-doc resolve-1-shallow
  "Shallowly replaces all dynamic items with the items they resolve to for the given state."
  [item state]
  {:post [#(not (instance? base/Dynamic %))
          #(not (instance? base/WithRef %))
          #(not (instance? base/WithAsyncReturn %))]}
  (if (string? item)
    item
    (condp instance? item
      ;; the dynamics
      base/Dynamic (apply (:f item) state (:args item))
      base/WithRef (apply (:f item) dummy-ref (:args item))
      base/WithAsyncReturn (apply (:f item) dummy-return! (:args item))

      item)))

(defn ^:no-doc resolve-*
  [item state resolve-dyn]
  (if (string? item)
    item
    (let [w (fn []
              (update item :e #(resolve-* % state resolve-dyn)))
          wc (fn []
               (update item :children (fn [es]
                                        (mapv #(resolve-* % state resolve-dyn) es))))]
      (condp instance? item
        ;; the dynamics
        base/Dynamic (resolve-dyn item state)
        base/WithRef (resolve-dyn item state)
        base/WithAsyncReturn (resolve-dyn item state)

        ;; the wrappers
        base/Focus (update item :e #(resolve-* % (yank state (:lens item)) resolve-dyn))
        base/LocalState (update item :e #(resolve-* % [state (:initial item)] resolve-dyn))
        
        base/HandleAction (w)
        base/SetRef (w)
        base/Keyed (w)
        base/Named (w)
        base/ErrorBoundary (w) ;; and the error fn?
        base/HandleMessage (w)

        base/Fragment (wc)
        dom/Element   (wc)

        ;; the leafs
        base/Once item))))

(defn ^:no-doc resolve-1
  "Replaces one level of dynamicity from the given item, or just returns it if there is none."
  [item state]
  (resolve-* item state resolve-1-shallow))

(defn ^:no-doc resolve-deep
  "Deeply replaces all dynamic items with the items they resolve to for the given state."
  [item state]
  (resolve-* item state (fn [item state]
                          (resolve-deep (resolve-1 item state) state))))

(defn- map-diff [m1 m2]
  (let [[only-1 only-2 _] (data/diff m1 m2)]
    [only-1 only-2]))

(defn seq-diff [s1 s2]
  ;; TODO: something else, like [_ _ x _] ...?
  (let [[only-1 only-2 _] (data/diff s1 s2)]
    [only-1 only-2]))

(defn ^:no-doc find-first-difference [item1 item2 & [path]]
  ;; returns [path differences] path=[item ...] and differences {:name [left right]}
  (let [t1 (type item1)
        t2 (type item2)
        path (or path [])
        w (fn [e1 e2 e-k res-k]
            (let [path (conj path (type e1))]
              (if (= (e-k item1) (e-k item2))
                (find-first-difference (:e item1) (:e item2) path)
                [path {res-k [(e-k item1) (e-k item2)]}])))]
    (cond
      (= item1 item2) nil
      
      (not= t1 t2) [path {:types [t1 t2]}]

      ;; dynamics
      (or (= t1 base/Dynamic) (= t1 base/WithRef) (= t1 base/WithAsyncReturn))
      (let [path (conj path t1)]
        (if (= (:f item1) (:f item2))
          [path {:arguments (seq-diff (:args item1) (:args item2))}]
          [path {:function [(:f item1) (:f item2)]}]))

      ;; wrappers
      (or (= t1 base/HandleAction) (= t1 base/ErrorBoundary) (= t1 base/HandleStateChange) (= t1 base/HandleMessage))
      (w item1 item2 :f :function)

      (= t1 base/SetRef)
      (w item1 item2 :ref :reference)

      (= t1 base/Focus)
      (w item1 item2 :lens :lens)
      
      (= t1 base/Named)
      (let [id1 (:name-id item1)
            id2 (:name-id item2)]
        (assert (base/name-id? id1))
        (assert (base/name-id? id2))
        (if (= id1 id2)
          (find-first-difference (:e item1) (:e item2) (conj path (base/name-id-name id1)))
          (if (= (base/name-id-name id1) (base/name-id-name id2))
            [(conj path base/Named) {:name-id [id1 id2]}]
            [(conj path base/Named) {:name [(base/name-id-name id1) (base/name-id-name id2)]}])))
      
      (= t1 base/Keyed)
      (w item1 item2 :key :key)

      ;; containers
      (or (= t1 base/Fragment) (= t1 dom/Element))
      (let [cs1 (:children item1)
            cs2 (:children item2)
            in-path path
            path (conj path (if (= t1 dom/Element) (:type item1) t1))]
        (cond
          (and (= t1 dom/Element) (not= (:type item1) (:type item2)))
          [in-path {:tag [(:type item1) (:type item2)]}]

          (not= (count cs1) (count cs2))
          ;; could look for keys, if there is an additional in front or back, etc. here.
          [path {:child-count [(count cs1) (count cs2)]}]

          (and (= t1 dom/Element) (not= (:attrs item1) (:attrs item2)))
          [path {:attributes (map-diff (:attrs item1) (:attrs item2))}]

          (and (= t1 dom/Element) (not= (:events item1) (:events item2)))
          [path {:events (map-diff (:events item1) (:events item2))}]

          (and (= t1 dom/Element) (not= (:ref item1) (:ref item2)))
          [path {:ref [(:ref item1) (:ref item2)]}] ;; the native/raw ref.
              
          :else
          (reduce (fn [res [idx [c1 c2]]]
                    (or res
                        (find-first-difference c1 c2 (conj path idx))))
                  nil
                  (map-indexed vector (map vector cs1 cs2)))))

      ;; leafs
      (= t1 base/Once)
      (if (not= (:ret item1) (:ret item2))
        [path {:once [(:ret item1) (:ret item2)]}]
        [path {:once-cleanup [(:cleanup-ret item1) (:cleanup-ret item2)]}])
      
      :else ;; string?!
      [path {:not= [item1 item2]}])))

(defn ^:no-doc resolve-differences
  "Resolve up to the first difference, returns nil if equal, or a tuple of resolved items."
  [item state]
  ;; Resolving more and more levels of dynamicity, until we find an impure item (or there is nothing dynamic left)
  (loop [e1 (resolve-1 item state)
         e2 (resolve-1 item state)]
    (if (not= e1 e2)
      [e1 e2]
      (let [e1_ (resolve-1 e1 state)
            e2_ (resolve-1 e2 state)]
        (if (and (= e1 e1_)
                 (= e2 e2_))
          nil ;; fully resolved, no differences
          (recur e1_ e2_) ;; search deeper
          )))))

(defn ^:no-doc check-pure
  "Checks if the given item always resolves to the same for the
  given state, i.e. that rendering has no side effects."
  [item state]
  ;; Note: ideal performance also depends on our reacl implementation; but for what the user can do, this is even a better test.
  (nil? (resolve-differences item state)))

(defn ^:no-doc check-minimal
  "Checks if the given item resolves to something different, given the two different states."
  [item s1 s2]
  ;; Note: only makes sense if check-pure is true
  (assert (not= s1 s2) "States must be different to check.")
  (let [e1 (resolve-deep item s1)
        e2 (resolve-deep item s2)]
    (not= e1 e2)))

(defn- performance-check*
  [item state-seq bad-example-f non-ideal-example-f]
  (assert (base/item? item))
  (assert (not (empty? state-seq)))
  (loop [state-seq state-seq
         prev-state nil
         minimal? true]
    (if (not (seq state-seq))
      (if minimal? :ideal :good)
      (let [s1 (first state-seq)]
        (if (check-pure item s1)
          (if (and minimal? (some? prev-state) (not= (first prev-state) s1))
            (let [minimal? (check-minimal item (first prev-state) s1)]
              (when (not minimal?)
                (non-ideal-example-f item (first prev-state) s1))
              (recur (rest state-seq)
                     [s1]
                     minimal?))
            (recur (rest state-seq)
                   [s1]
                   minimal?))
          (do (bad-example-f item s1)
              :bad))))))

(defn performance-check
  "For all the given states, this checks that for the same state, the
  item renders to the equal item; i.e. rendering has no side
  effects. If that it true, it also checks that for different states,
  it renders to different items; i.e. the state is minimal for this
  item. Note that this test makes most sense for 'dynamic'
  items. Returns :bad, :good, :ideal depending on these results."
  [item state-seq]
  (performance-check* item state-seq
                      (fn [& args])
                      (fn [& args])))

(defn verify-performance [level item state-seq]
  ;; TODO: cljs.test checker variant of this?
  (assert (base/item? item))
  (assert (or (= level :good) (= level :ideal)))
  (assert (or (= level :good) (not (empty? (rest (distinct state-seq))))) "To test test for :ideal performance, at least two different state values are needed.")
  (let [bad-example (atom nil)
        non-ideal-example (atom nil)
        
        actual (performance-check* item state-seq
                                   (fn [item state]
                                     ;; will currently be called at most once.
                                     (reset! bad-example [item state]))
                                   (fn [item state-1 state-2]
                                     ;; will be called multiple time; keep only one for now.
                                     (reset! non-ideal-example [item state-1 state-2])))]
    (cond
      (= actual :bad)
      (let [[item state] @bad-example ;; item = always the main item currently.
            diff (let [[e1 e2] (resolve-differences item state)]
                   (find-first-difference e1 e2))
            ;; Meaning: with this state, item resolved differently on repeated calls, with at least one difference at the specified place
            ;; = (some (fn) or other side effect?)
            cause {:state state
                   :different-at diff}]
        [:bad [cause]])
      
      (and (= level :ideal) (= actual :good))
      (let [[item state-1 state-2] @non-ideal-example
            ;; Meaning: with these states that are different, the item resolved to the same thing. The state could be made smaller.
            ;; TODO: we could try mutation testing; reduce the state, while still resolving to same (and without errors); then say "this part of the state might be unused".
            cause {:state-1 state-1
                   :state-2 state-2
                   :state-diff (data/diff state-1 state-2)
                   :resolved (resolve-deep item state-1)}]
        [:good [cause]])

      :else nil)))

(defn verify-performance! [level item state-seq]
  (when-let [[actual causes] (verify-performance level item state-seq)]
    (cond
      (= actual :bad)
      (let [[path diff] (:different-at (first causes))]
        (throw (ex-info (str "Performance should be " level ", but was actually :bad."
                             ;; TODO: could make the diff even more human-readable...
                             " What makes it bad is at " (pr-str path) " where this differs: " (pr-str diff))
                        (first causes))))
      (= actual :good)
      (throw (ex-info (str "Performance should be :ideal, but was actually only :good.")
                      (first causes)))

      :else (assert false actual))))
