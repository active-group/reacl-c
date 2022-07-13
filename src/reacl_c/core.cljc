(ns reacl-c.core
  "Functions, primitive values and some macros to define new items."
  (:require [reacl-c.base :as base]
            [reacl-c.dom-base :as dom-base]
            [clojure.set :as set]
            [schema.core :as s]
            #?(:clj [schema.macros :as sm])
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [cljs.analyzer :as ana])
  
  (:refer-clojure :exclude [deref empty refer]))

;; Rationale:
;; The basic building block is en Item (base/E), which is roughly
;; equivalent to a Reacl class with app-state but without any
;; arguments resp. all arguments bound (which makes composition a lot
;; easier). But because every
;; item is highly 'customized' by user arguments, it would be hard
;; to retain any of the rerendering optimizations by Reacl/React (and
;; any local-state). Second, items could be represented as a [class
;; args] tuple; but to detach the definition from the implementation,
;; they are instead represented as [type args], and the implementing
;; Reacl class is added via extend-type in 'impl/reacl'.

;; Additions to think about:
;; - getSnapshotBeforeUpdate ?
;; - a (bind item (fn [component] => item)) to 'bridge' state to a lower part?
;; - (derived-local-state init (fn [outer inner] => inner) item)  ? or same without outer?

(defn item?
  "Returns true if is an item, and false otherwise."
  [v]
  (base/item? v))

(defn fragment
  "Returns a container item consisting of the given child items."
  [& children]
  {:pre [(base/assert-item-list "fragment" children)]}
  (base/make-fragment children))

(def ^{:doc "An invisible item with no behavior."} empty (fragment))

(defn with-ref
  "Creates an item identical to the one returned from `(f ref &
  args)`, where `ref` is a fresh *reference*. A reference should be
  assigned to one of the items below via [[refer]]. You can use it
  as the target of a `(return :message [target msg])` for example."
  [f & args]
  (base/make-with-ref f args))

(declare with-refs)
(let [h0 (fn [_ f args] (apply f nil args))
      h1 (fn [r f args] (apply f (list r) args))
      hn_cont (fn [rs r0 f args]
                (apply f (cons r0 rs) args))
      hn (fn [r0 n f args]
           (with-refs (dec n)
             hn_cont r0 f args))]
  (defn with-refs
    "Returns an item that calls f with a list of `n` references and any remaining args. See [[with-ref]]."
    [n f & args]
    {:pre [(>= n 0)
           (ifn? f)]}
    (cond
      (= 0 n) (with-ref h0 f args)
      (= 1 n) (with-ref h1 f args)
      :else
      (with-ref hn n f args))))

(defn refer
  "Returns an item identical to the given item, but with the given
  reference assigned. Note that the returned item cannot be used more
  than once. See [[with-ref]] for a description of references.

  This should only be used for messaging, but prefer ref-let for that.
  To get access to native DOM elements, set the `:ref` attribute of
  items in the [[reacl-c/dom]] namespace.
  "
  [item ref]
  {:pre [(base/item? item)]}
  ;; Note: This special tratment for dom items is deprecated; removed in 0.11
  (if (dom-base/element? item)
    (dom-base/set-ref item ref)
    (base/make-refer item ref)))

(let [c (fn [refs items f args]
          (apply f (map refer items refs) args))]
  (defn ref-let*
    "Returns an item, that calls f with a list and the given arguments,
  where the list consists of the given items, but modified in a way
  that makes them usable as message targets."
    [items f & args]
    (with-refs (count items)
      c items f args)))

(defmacro ref-let
  "A macro that defines some names to refer to the given items, which allows the names to be used as message target in the body.

  For example, to create an item that redirects messages to one of its children based on some criteria, you can write:

  ```
  (ref-let [child-1 (my-item-1 ...)
            child-2 (my-item-2 ...)]
    (handle-message (fn [_ msg]
                       (if (is-for-child-1? msg)
                         (return :message [child-1 msg])
                         (return :message [child-2 msg])))
      (div child-1 child-2)))
  ```
"
  [bindings & body]
  (assert (even? (count bindings)))
  (let [[names items] (let [l (partition-all 2 bindings)]
                        [(map first l) (map second l)])]
    `(ref-let* (list ~@items)
               ;; must create a fresh function here :-/
               (fn [[~@names]]
                 ~@body))))

(defn deref
  "Returns a runner specific value, which might be a native dom
  element backing an item at runtime for example. See [[with-ref]] for
  a description of references."
  [ref]
  ;; TODO: needs more to access to actually access the native dom; move this to 'main namespace'?
  ;; TODO: move to dom.cljs? Should at least be clear that it makes only sense for dom elements.
  (base/-deref-ref ref))

(defn dynamic
  "Returns a dynamic item, which looks and behaves like the item
  returned from `(f state & args)`, which is evaluated each time the
  state changes."
  [f & args]
  {:pre [(ifn? f)]}
  (base/make-dynamic f args))

(defn focus
  "Returns an item that focuses the outer state, to the part of it
  that `item` shall see, via the given *lens*. Otherwise behaves and
  looks the same."
  [lens item]
  {:pre [(base/item? item)]}
  (if (= lens lens/id)
    item
    (base/make-focus item lens)))

(defn static
  "Returns an item that is always like `(f & args)`, independant of
  state changes. The item returned by `f` must not access state nor
  change it."
  [f & args]
  {:pre [(ifn? f)]}
  (base/make-static f args))

(defn embed-state
  "Embeds the state of the given item into a part of the state of the
  resulting item, via the given *lens*."
  [item lens]
  (focus lens item))

(def ^{:arglists '([:state state :action action :message [target message]])
       :doc "Creates a value to be used for example in the function
       passed to [[handle-action]]. All arguments are optional:

- `:state`   means that the item changes its state to the given value
- `:action`  means that the given action is emitted by the item
- `:message` means that the given message is sent to the given target (a reference or a item with a reference assigned).

If no `:state` option is used, the state of the item will not
change. `:state` must occur at most once, `:message` and `:action` can
be specified multiple times.
"}
  return
  (fn [& args]
    (assert (even? (count args)) "Expected an even number of arguments.")
    (loop [args (seq args)
           state base/keep-state
           actions (transient [])
           messages (transient [])]
      (if (empty? args)
        (base/make-returned state (persistent! actions) (persistent! messages))
        (let [arg (second args)
              nxt (nnext args)]
          (case (first args)
            (:state) (do (when-not (= base/keep-state state)
                           (assert false (str "A :state argument to return must be specified only once.")))
                         (recur nxt arg actions messages))
            (:action) (recur nxt state (conj! actions arg) messages)
            (:message) (let [[target msg] arg]
                         (assert (some? target) "Missing target for message.")
                         (assert (base/message-target? target) "Target must be a reference created by with-ref or an item created by refer.")
                         (recur nxt state actions (conj! messages [target msg])))
            (do (assert (contains? #{:state :action :message} (first args)) (str "Invalid argument " (first args) " to return."))
                (recur nxt state actions messages))))))))

(def ^{:arglists '([v])
       :doc "Returns whether the given value is a [[return]] value."}
  returned? base/returned?)

(def ^{:arglists '([& rets])
       :doc "Merges multiple [[return]] values into one. Actions and
       messages are concatenated. If more than one contains a new
       state, the right-most state value is used."}
  merge-returned base/merge-returned)

(def ^{:doc "A lens over a list of the actions contained in a [[return]] value."}
  returned-actions base/returned-actions)

(def ^{:doc "A lens over a list of the messages contained in a [[return]] value, as tuples `[target message]`."}
  returned-messages base/returned-messages)

(def ^{:doc "A lens over the state contained in a [[return]] value. No
  state is represented by the unique value [[keep-state]]."}
  returned-state base/returned-state)

(def ^{:doc "A unique value for the state in a [[return]] value, representing that the state should not be changed."}
  keep-state base/keep-state)

(defn embed-returned
  "Given a [[return]] value on a part of the given state, as specified
  by the given lens, this returns an equivalent [[return]] value with
  a change to the given `state` value.

  For example:

  ```
  (embed-returned [:a :b] lens/first (return :c))
  =
  (return [:c :b])
  ```

  See also [[lift-handler]]."
  [state lens ret]
  (cond-> ret
    (not= base/keep-state (base/returned-state ret))
    (base/merge-returned ret
                         (return :state (lens/shove state lens (base/returned-state ret))))))

(defn ^:no-doc as-returned [v]
  (if (base/returned? v) v (return :state v)))

(let [h* (fn [lens h state & args]
           (let [v (apply h (lens/yank state lens) args)
                 ret (if (base/returned? v) v (return :state v))]
             (embed-returned state lens ret)))]
  (defn lift-handler
    "Given a handler function `h`, which takes a state as the first
  argument, this returns a handler function that works on a larger
  state, by extracting the smaller state via the given lens, and
  embedding a returned state back into a return value on the larger
  state."
    [lens h]
    (f/partial h* lens h)))

(defn handle-message
  "Handles the messages sent to the the resulting item (either
  via [[reacl-c.main/send-message!]] or [[return]]), by calling `(f state message)`,
  which must return a [[return]] value. The resulting item
  otherwise looks and behaves exactly like the given one."
  [f item]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-message f item))

(let [h (fn [ref handle-msg f args]
          (fragment (-> (handle-message handle-msg empty)
                        (refer ref))
                    (apply f ref args)))]
  (defn with-message-target
    "Returns an item like ´(f target & args)`, where `target` is a
  reference that can be used as a message target, which are then
  handled by a call to `(handle-msg state msg)`, which must return
  a [[return]] value."
    ;; useful when sending messages upwards.
    [handle-msg f & args]
    (with-ref h handle-msg f args)))

(defn- no-effect? [action]
  (not (base/effect? action)))

(defn handle-action
  "Handles actions emitted by given item, by evaluating `(f state action)` for each
  of them. That must return the result of calling [[return]] with
  either a new state, and maybe one or more other actions (or the
  given action unchanged). "
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-action item f no-effect?))

(defn handle-effect
  "Handles effect actions emitted by given item, by evaluating `(f
  state action)` for each of them. That must return a [[return]]
  value."
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-action item f base/effect?))

(defn- keep-if-nil [f v]
  (let [vv (f v)]
    (if (nil? vv)
      v
      vv)))

(defn- action-or-nil [f state a]
  (return :action (keep-if-nil f a)))

(defn map-actions
  "Returns an item that emits actions `(f action)`, for each
  action emitted by `item`, and otherwise looks an behaves exacly
  the same. If `(f action)` is nil, then the original action is kept,
  allowing for `f` to be a map of the actions to replace."
  [item f]
  {:pre [(ifn? f)]}
  ;; OPT: if f is a map, we could create a predicate from the keys...?
  (handle-action item (f/partial action-or-nil f)))

(defn- effect-mapper [f eff]
  (keep-if-nil f
               ;; if composed, recurse into the composition
               (if (base/composed-effect? eff)
                 ;; Note: in the most general way, this might not make sense (the
                 ;; composition might expect a certain type of result from the
                 ;; previous parts), but this logic has in mind to allow
                 ;; `handle-effect-result` to use a composition, and the user to
                 ;; replace something inside with a const-effect - and so emulating
                 ;; something in a test.
                 (base/map-composed-effect eff (f/partial effect-mapper f))
                 eff)))

(defn map-effects
  "Returns an item that emits actions `(f effect)`, for each effect
  action` emitted by `item`, and otherwise looks an behaves exacly the
  same. If `(f effect)` is nil, then the original effect action is
  kept, allowing for `f` to be a map of the effects to replace. This
  also works for composed effects with [[seq-effects]]
  or [[par-effects]], such that the effects used in there are mapped
  too."
  [item f]
  {:pre [(ifn? f)]}
  (handle-effect item (f/partial action-or-nil (f/partial effect-mapper f))))

(let [h (fn [ref state msg]
          (return :message [ref msg]))]
  (defn redirect-messages
    "Return an item like the given one, but that handles all messages
  sent to it by redirecting them to the item specified by the given
  reference."
    [ref item]
    {:pre [(base/ref? ref)
           (base/item? item)]}
    (handle-message (f/partial h ref) item)))

(let [h (fn [ref f args]
          (redirect-messages ref (apply f ref args)))]
  (defn forward-messages
    "Returns an item like `(f ref & args)` where `ref` is a reference
    to which any messages sent to the returned item is forwarded
    to. You must use [[refer]] to define which item that is further
    down in the item."
    [f & args]
    {:pre [(ifn? f)]}
    ;; useful when sending messages downwards.
    (with-ref h f args)))

(let [h (fn [f ref state msg]
          (return :message [ref (f msg)]))
      wr (fn [ref f item]
           (handle-message (f/partial h f ref) (refer item ref)))]
  (defn map-messages
    "Returns an item like the given one, that transforms all messages sent to
  it though `(f msg)`, before they are forwarded to `item`."
    [f item]
    {:pre [(ifn? f)
           (base/item? item)]}
    (with-ref wr f item)))

(defn name-id
  "Generates a fresh unique value that can be used to generate named
  items via [[named]]. Note that calling this twice with the same
  name returns different values."
  [s]
  {:pre [(string? s)]}
  (base/make-name-id s))

(defn ^:no-doc named*
  [name-id validate-state! item]
  {:pre [(base/item? item)
         (base/name-id? name-id)
         (or (nil? validate-state!) (ifn? validate-state!))]}
  (base/make-named name-id item validate-state!))

(defn named
  "Returns an item that looks and works exactly like the given item,
  but with has a user defined name, that appears and can be used in
  testing and debugging utilities. Use [[name-id]] to generate a
  unique name object. See [[def-named]] and [[defn-named]] for more
  convenient ways to create named items."
  [name-id item]
  {:pre [(base/item? item)
         (base/name-id? name-id)]}
  (named* name-id nil item))

(defn- id-merge [m1 m2]
  (reduce-kv (fn [r k v]
               (if (= (get r k) v)
                 r
                 (assoc r k v)))
             m1
             m2))

(defn- select-keys* [m keys]
  ;; like clojure/select-keys, but missing keys get a nil value.
  (reduce (fn [m [k v]]
            (assoc m k v))
          {}
          (map (fn [k]
                 [k (get m k nil)])
               keys)))

(defn local-state
  "Returns an item which looks like the given item, with state `outer`,
  where the given item must take a tuple state `[outer inner]`, and
  `initial` is an intial value for the inner state, which can then be
  changed by the item independantly from the outer state."
  [initial item]
  {:pre [(base/item? item)]}
  (base/make-local-state item initial))

(defn add-state
  "Adds new state that the given item can access, via a lens on the
  the tuple of states `[outer inner]`, where the initial value for
  `inner` state is `initial`. Note that the resulting item has only
  `outer` as its state."
  [initial lens item] ;; aka extend-state?
  (local-state initial (focus lens item)))

(defn hide-state
  "Hides a part of the state of the given item, via a lens that
  reduces the the tuple of states `[outer inner]`, where the initial
  value for `inner` state is `initial`. The resulting item has only
  `outer` as its state."
  [item initial lens]
  ;; Note: yes, it's actually the same as 'add-state' ;-)
  (add-state initial lens item))

(defn isolate-state
  "Hides the state of the given item as a local state, resulting in an
  item with an arbitrary state that is inaccessible for it."
  [initial-state item]
  (static (f/partial add-state initial-state lens/second item)))

(defn keyed
  "Adds an arbitrary identifier to the given item, which will be used
  to optimize rendering of it in a list of children of a container
  item."
  [item key]
  {:pre [(base/item? item)]}
  (base/make-keyed item key))

(defn lifecycle
  "Returns an invisible item, that calls `init` each time the item is
  used at a place in the component hierarchy, including every change
  of state or the `init` function itself subsequently. The `finish`
  function is called when the item is no longer used at that
  place. Both functions must return a [[return]] value specifying what
  to do."
  [init finish]
  {:pre [(or (nil? init) (ifn? init))
         (or (nil? finish) (ifn? finish))]}
  (base/make-lifecycle init finish))

(let [init (fn [init-f [state local]]
             (let [r (init-f state)
                   new (assoc local :init r)]
               (if (= local new)
                 (return)
                 (return :state [state new]
                         :action r))))
      finish (fn [cleanup-f [state local]]
               (return :action (cleanup-f state)))
      pass-act (fn [state act]
                 ;; Note: act is always the result of either init-f or cleanup-f
                 act)
      no-cleanup nil]
  (defn once
    "Returns an item that evaluates `(f state)` and emits the [[return]]
  value that it must return initially. On subsequent state updates,
  `f` is called too, but the returned [[return]] value is only emitted
  if it different than last time. In other words, the same [[return]]
  value is emitted only once. The optional `(cleanup-f state)` is
  evaluated, when the item is removed from the item tree afterwards.

  Note that if you return a modified state, you must be careful to not
  cause an endless loop of updates."
    [f & [cleanup-f]]
    {:pre [(ifn? f)
           (or (nil? cleanup-f) (ifn? cleanup-f))]}
    (-> (local-state {}
                     (lifecycle (f/partial init f)
                                (if (some? cleanup-f)
                                  (f/partial finish cleanup-f)
                                  no-cleanup)))
        (handle-action pass-act))))

(defn init
  "An invisible item that 'emits' the given [[return]] value once as
  an initialization."
  [ret]
  (once (f/constantly ret)))

(defn cleanup
  "Returns an item that evaluates `(f state)` when it is being removed
  from the item tree, and emits the [[return]] value that that must
  return."
  [f]
  (lifecycle nil f))

(defn finalize
  "An invisible item that 'emits' the given [[return]] value once as
  a cleanup after the item was used somewhere in the component tree."
  [ret]
  (cleanup (f/constantly ret)))

(defn handle-state-change
  "Returns an item like the given item, but when a state change is
  emitted by `item`, then `(f prev-state new-state)` is evaluated,
  which must return a [[return]] value. By careful with this, as item
  usually expect that their changes to the state are eventually
  successful."
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-state-change item f))

(let [h (fn [f args old new]
          (apply f old new args)
          (return :state new))]
  (defn monitor-state
    "When e changes its state, `(f old-state new-state & args)` is
  evaluted for side effects. Note that this is only called when the
  item changes its state 'by itself', not if the state was changed
  somewhere upwards in the item tree an is only passed down to the
  resulting item." ;; TODO: change 'side effect' to an effect?
    [item f & args]
    {:pre [(base/item? item)
           (ifn? f)]}
    (handle-state-change item (f/partial h f args))))

;; Note: low-level feature which is a bit dangerous to use (not check
;; if item is mounted); user should use subscriptions.
(defn ^:no-doc with-async-return [f & args]
  {:pre [(ifn? f)]}
  (base/make-with-async-return f args))

;; Note: low-level feature which is a bit dangerous to use (not check
;; if item is mounted); user should use subscriptions.
(let [send! (fn [return! target msg]
              (return! (return :message [target msg])))
      h (fn [return! f args]
          (apply f (f/partial send! return!) args))]
  (defn ^:no-doc with-async-messages [f & args]
    {:pre [(ifn? f)]}
    (with-async-return h f args)))

;; Note: low-level feature which is a bit dangerous to use (not check
;; if item is mounted); user should use subscriptions.
(let [h (fn [return! action]
          (return! (return :action action)))
      g (fn [return! f args]
          (apply f (f/partial h return!) args))]
  (defn ^:no-doc with-async-actions [f & args]
    {:pre [(ifn? f)]}
    (with-async-return g f args)))

(defn effect
  "Return an effect action, which, when run, calls the given function
  with the given arguments. The result of that function is ignored,
  unless you use [[handle-effect-result]], or return a [[return]]
  value with new actions or messages."
  [f & args]
  (base/make-effect f args))

(defn const-effect
  "An effect that does nothing, with the given value as its result."
  [v]
  (effect identity v))

(def no-effect
  "An effect action that does nothing and returns nil."
  (const-effect nil))

(defn seq-effects
  "Sequentially compose two or more effects. The first argument must
  be an effect action, and the following must be functions that are
  called with the result of the previous one and must return a new
  effect action."
  [eff & fs]
  {:pre [(base/effect? eff)
         (every? ifn? fs)]}
  ;; Note: this could be defined as a simple fn and effect, but in the test environment we want to see the details.
  (if (empty? fs)
    eff
    (base/make-composed-effect eff (first fs) (rest fs))))

(let [g (fn [f args v]
          (apply f v args))]
  (defn fmap-effect
    "Returns an effect like `eff`, whose result is f applied to the
  result of the given effect."
    [eff f & args]
    (seq-effects eff (f/comp const-effect (f/partial g f args)))))

(let [first-eff (fn [eff]
                  (fmap-effect eff list))
      next-eff (fn [eff]
                 (f/partial fmap-effect eff cons))]
  (defn par-effects
    "Compose effects, which are run in 'parallel' (i.e. in no particular
  order), into one effect that results in a sequence of the results of
  the individual effects."
    [eff & effs]
    {:pre [(base/effect? eff)
           (every? base/effect? effs)]}
    ;; Note: not really parallelized yet, but will be run in one update cycle.
    (apply seq-effects (first-eff eff) (map next-eff effs))))

(defn- send-effect-result [host result]
  (return :message [host result]))

(let [wr (fn [ref eff]
           (init (return :action (seq-effects eff (f/partial effect send-effect-result ref)))))]
  (defn handle-effect-result
    "Runs the given effect once, feeding its result into `(f state
  result)`, which must return a [[return]] value.

  Deprecated: Use [[execute-effect]] instead."
    [f eff]
    {:pre [(base/effect? eff)
           (ifn? f)]}
    (with-message-target f
      wr eff)))

(defn execute-effect
  "Returns an item that executes the given effect once, optionally
  feeding its result into `(f state result)`, which must return
  a [[return]] value.
  
  Note that you can execute an effect also by using `(return :action
  effect)`, if the result of the effect is irrelevant.
  "
  [eff & [f]]
  (handle-effect-result (or f (f/constantly (return)))
                        eff))

(defrecord ^:no-doc SubscribedMessage [stop! sync-actions])

(defrecord ^:no-doc SubscribedEmulatedResult [action])

(defn ^:no-doc subscribe! [f args async-deliver! host action-mapper]
  (let [sync (atom [])
        deliver! (comp (fn [a]
                         (if (some? @sync)
                           (swap! sync conj a)
                           (async-deliver! (return :action a))))
                       action-mapper)
        stop! (apply f deliver! args)
        sync-actions @sync]
    (reset! sync nil)
    (assert (ifn? stop!) "Subscription must return a stop function.")
    (return :message [host (SubscribedMessage. stop! sync-actions)])))

(def ^:no-doc subscription-from-defn-meta-key ::subscription-from-defn)

(defn- subscribe-effect [f deliver! args host action-mapper defn-f]
  (assert (ifn? f))
  (-> (effect subscribe! f args deliver! host action-mapper)
      (vary-meta assoc subscription-from-defn-meta-key defn-f)))

(defn- unsubscribe! [stop! [f & args]]
  (assert (some? stop!) ;; should not be called if nil
          (str "No stop function to unsubscribe from subscription: " f (if (empty? args) "." (str ", when called with " (pr-str args) "."))))
  (when stop!
    (stop!))
  (return))

(defn- unsubscribe-effect [stop! f args]
  ;; Note: f and args only here to enable a test with [[unsubscribe-effect?]]
  (effect unsubscribe! stop! (cons f args)))

(let [store-sub (fn [f args state msg]
                  (cond
                    (instance? SubscribedMessage msg)
                    (do (assert (:stop! msg) ;; TODO: exn, or just a warning?
                                (str "Subscription did not return a stop function: " f (if (empty? args) "." (str ", when called with " (pr-str args) "."))))
                        (apply base/merge-returned
                               (return :state {:subscribed [(:stop! msg) f args]})
                               (map #(return :action %) (:sync-actions msg))))

                    ;; added for test-util emulation of subscriptions.
                    (instance? SubscribedEmulatedResult msg)
                    (return :action (:action msg))

                    :else
                    (do (assert false (str "Unexpected message: " (pr-str msg)))
                        (return))))
      do-unsub (fn [f state]
                 (let [{[stop! s-f s-args] :subscribed} state]
                   (if stop! ;; not sure why is this not set sometimes? immediate unmounts?
                     (return :action (unsubscribe-effect stop! s-f s-args))
                     (return))))
      do-sub (fn [host deliver! action-mapper defn-f f args state]
               (let [{[stop! s-f s-args] :subscribed} state]
                 (if (or (not= f s-f) (not= args s-args))
                   (merge-returned
                    ;; unsubscribe before resubscribe when f or args change
                    (if (some? stop!)
                      (return :state {:subscribed nil}
                              :action (unsubscribe-effect stop! s-f s-args))
                      (return))
                    (return :action (subscribe-effect f deliver! args host action-mapper defn-f)))
                   (return))))
      controller (fn [host deliver! action-mapper defn-f f args]
                   (fragment (-> (handle-message (f/partial store-sub f args)
                                                 (fragment))
                                 (refer host))
                             (lifecycle
                              (f/partial do-sub host deliver! action-mapper defn-f f args)
                              (f/partial do-unsub f))))
      stu (fn [deliver! action-mapper defn-f f args]
            (isolate-state {:subscribed nil}
                           (with-ref controller deliver! action-mapper defn-f f args)))]

  (defn ^:private subscription*
    [action-mapper defn-f f & args]
    (with-async-return stu action-mapper defn-f f args))

  (defn ^:no-doc subscription-deconstruct
    [item]
    (let [item (if (base/named? item) ;; items created via defn-subscription may be named.
                 (base/named-e item)
                 item)]
      ;; Note: must correspond to what subscription* does.
      (and (base/with-async-return? item)
           (= stu (base/with-async-return-f item))
           (let [[_ defn-f f args] (base/with-async-return-args item)]
             [defn-f f args]))))
  )

(defn subscription
    "Returns an item that emits actions according to the given
  function `f`. Each time the retuned item is used, `f` will be called
  with a side-effectful `deliver!` function which takes the action to
  emit, and `f` must return a `stop` function of no arguments. You can
  call `deliver!` immediately once or multiple times, and you may also
  do some kind of registration at an asynchronous library or native
  browser api, and use `deliver!` to inform your application about the
  results later, once or multiple times. But when the `stop` function
  is called, you must prevent any more calls to `deliver!`."
    [f & args]
    {:pre [(ifn? f)]}
    (apply subscription* identity nil f args))

(defn handle-error
  "Creates an error boundary around the given item. When the rendering
  of `e` throws an exception, then `(f state error)` is evaluated, and must
  result in an [[return]] value. Note that exceptions in functions
  like [[handle-action]], are not catched by this. See [[try-catch]]
  for a higher level construct to handle errors."
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-error item f))

(let [set-error (fn [state error]
                  (return :state [state error]))
      dyn (fn [[state error] try-e catch-e]
            (if (some? error)
              catch-e
              (-> (focus lens/first try-e)
                  (handle-error set-error))))]
  (defn try-catch
    "Returns an item that looks an works the same as the item
  `try-item`, until an error is thrown during its rendering. After
  that `catch-item` is rendered instead, with a state of the combined
  outer state and the error - `[state-of-e error]`. The `catch-item`
  will usually be interactive, for example, displaying the error (and
  the relevant part of the state) to the user, and offer a button to
  reset the error to `nil` and maybe fix the state, after which
  `try-item` is showed again."
    [try-item catch-item]
    {:pre [(base/item? try-item)
           (base/item? catch-item)]}
    (-> (dynamic dyn try-item catch-item)
        (hide-state nil lens/id))))

(let [df (fn [state e validate!]
           ;; state passed down!
           (validate! state :down)
           e)
      mf (fn [old new validate!]
           ;; state passed up!
           (validate! new :up))]
  (defn validation-boundary
    "Returns an item that forms a state validation boundary around the given item,
  where `(validate! state :up)` is evaluated for side effects when a
  state change is flowing out of then item upwards, and `(validate!
  state :down)` is evaluated for side effects when a new state is
  being pushed down."
    [item validate!]
    {:pre [(base/item? item)
           (ifn? validate!)]}
     ;; Note: dynamic adds it to render; could make a little earlied
     ;; via 'validate clause'; but probably not worth here (as
     ;; instantiation is delayed anyway)
    (-> (dynamic df item validate!)
        (monitor-state mf validate!))))

(defn ^:no-doc with-validation-meta [tgt src]
  ;; copy schema's :always-validate
  (with-meta tgt (merge (meta tgt) (select-keys (meta src) [:always-validate]))))

(defmacro ^:no-doc state-validator [name state-schema?]
  (let [name_st (symbol (str "state-of-" name))]
    (when state-schema?
      `(s/fn ~(-> name_st
                  (with-validation-meta name))
         [state# :- ~state-schema?]
         nil))))

(defmacro def-item
  "A macro to define items. This is not much different than the
  standard `def` macro of Clojure, but it attaches the name of the var
  to the item, which can be helpful in testing and debugging
  utilities (see [[named]]).

```
(def-item submit-button
  (dom/button {:type \"submit\"}))
```

 Additionally, a schema can be specified similar
  to [[core.schema/def]]. That schema does specify which state values
  are allowed for the defined item:

```
(def-item checkbox :- s/Bool
  (dom/input {:type \"checkbox\" ...}))
```
"
  [name & item]
  (let [[state-schema? item] (if (= ':- (first item))
                               [(second item) (rest (rest item))]
                               [nil item])
        name_ (str *ns* "/" name)]
    `(let [id# (name-id ~name_)
           validate# (state-validator ~name ~state-schema?)]
       (def ~name
         (named* id# validate# ~@item)))))

(defn ^:no-doc meta-name-id [v]
  ;; Note: deprecated, and not set for all named-fns because of clojurescripts MetaFn arity problem (see defn-named++)
  (::name-id (meta v)))

(defn- maybe-docstring [candidate & more]
  (if (string? candidate)
    (cons candidate more)
    (cons nil (cons candidate more))))

(defn- arity [args]
  ;; args may contain destructuring and schema annotations.
  ;; var-args may have an annotation too [x & args :- schema]

  ;; result: n fixed args, negative n = at least n-1, but variadic (-2 = 1 + many)
  (first
   (reduce (fn [[res schema?] a]
             (cond
               (= '& a) (reduced [(- (inc res)) nil]) ;; done 
               (= ':- a) [res true] ;; schema annotation; skip next too.
               :else (if schema?
                       [res false]
                       [(inc res) false])))
           [0 false]
           args)))

(defn ^:no-doc arity-checker [name arity]
  (if (= arity -1)
    (fn [n-args] nil)
    (fn [n-args]
      (when (if (< arity 0)
              (< n-args (dec (- arity)))
              (not= n-args arity))
        (throw (ex-info (str "Wrong number of args (" n-args ") passed to " name) {:function name :arity arity}))))))

(defn- has-schemata? [args]
  (some #{':-} args))

(defmacro ^:no-doc defn+
  "Internal utility macro.

  Creates something like

  (def name
    (mod-fn
      (fn [& args]
         (@create (apply @opt-wrapper (fn [@wrapper-args & args] @body) args)))))
  "
  [create mod-fn opt-wrapper wrapper-args name result-schema? docstring? args & body]
  (let [name_ (clojure.core/name name)
        name (vary-meta name assoc
                        :doc (or docstring? (:doc (meta name)))
                        :arglists `'(~args))]
    `(let [check-arity# (arity-checker ~name_ ~(arity args))
           ;; Note: s/fn has clojurescripts MetaFn arity problem; so use it only if user wants it.
           f# ~(if (or result-schema? (has-schemata? wrapper-args) (has-schemata? args))
                 `(s/fn ~name ~@(when result-schema? [:- result-schema?]) [~@wrapper-args ~@args] ~@body)
                 `(fn ~name [~@wrapper-args ~@args] ~@body))
           check-args-schema# ~(if (has-schemata? args)
                                 `(s/fn ~name [~@args] nil)
                                 `(constantly nil))]
       (def ~name
         (~mod-fn (fn [& args#]
                    (assert (do (check-arity# (count args#))
                                (apply check-args-schema# args#)
                                true))               
                    (~@create (apply ~@opt-wrapper f# args#))))))))

(def ^:no-doc stable-name-id
  ;; Note: must only be used for names from source-code, like with defn-item.
  (memoize name-id))

(defmacro ^:no-doc defn-named++
  "Internal utility macro.

  Creates something like

  (defn-named name [& args]
    (apply @opt-wrapper (fn [@wrapper-args & args] @body) args))
"
  [set-meta-name-id? opt-wrapper wrapper-args name  docstring? state-schema? args & body]
  (let [name_ (str *ns* "/" name)
        id (gensym "id")]
    `(let [~id (stable-name-id ~name_)
           validate# (state-validator ~name ~state-schema?)]
       (defn+ [named* ~id validate#] ~(if set-meta-name-id?
                                        `(fn [f#] (vary-meta f# assoc ::name-id ~id))
                                        `identity)
         ~opt-wrapper ~wrapper-args ~name nil ~docstring? ~args ~@body))))

(defmacro ^:no-doc defn-named+
  [& args]
  `(defn-named++ false ~@args))

(defn- maybe-schema-arg [candidate & more]
  (if (and (not-empty more) (= ':- (first more)))
    (list* (list candidate (first more) (second more)) (rest (rest more)))
    (list* (list candidate) more)))

(defn ^:no-doc subscription-from-defn [fn action-mapper f & args]
  (apply subscription*
         action-mapper
         fn
         f
         args))

(defmacro defn-subscription
  "A macro to define the integration of an external source of actions,
  that needs an imperative way to 'inject' actions into an
  application. This could be an interval timer, for example:

```
(defn-subscription interval-timer deliver! [ms]
  (let [id (.setInterval js/window (fn [] (deliver! (js/Date.))) ms)]
    (fn []
      (.clearInterval js/window id))))
```

With this definition, you can use `(interval-timer 1000)` as an
  item in your application. That item will be invisible, but
  will emit a JavaScript `Date` object as an action every second.

Note that `deliver!` can be called directly in the body of
  `defn-subscription` to emit some actions immediately, or it can be called later from
  an *asynchronous context*.  Also note that the body is evaluated as
  soon as the subscription item is mounted into your application, and
  that it must result in a function with no arguments, which is called
  when the item is removed from the application afterwards.

A schema annotation is possible after the name of the deliver
  function, to document and validate the action values emitted by the
  subscription item:

```
(defn-subscription window-width ^:always-validate deliver! :- s/Int []
  (let [id (.setInterval js/window (fn [] (deliver! js/window.innerWidth)) 100)]
    (fn []
      (.clearInterval js/window id))))
```
  "
  [name deliver! args & body]
  (let [[[deliver! _ action-schema?] args & body] (apply maybe-schema-arg deliver! args body)
        [docstring? deliver! args & body] (apply maybe-docstring deliver! args body)]
    (assert (symbol? deliver!) "Expected a name for the deliver function before the argument vector.")
    `(let [action-mapper# ~(if (some? action-schema?)
                            `(s/fn ~deliver! [action# :- ~action-schema?] action#)
                            `identity)]
       (defn-named++ true [subscription-from-defn ~name action-mapper#] [~deliver!] ~name ~docstring? nil ~args ~@body))))

(defn ^:no-doc effect-from-defn [fn eff]
  ;; Note: must be public, because used in macro expansion of defn-effec.
  (vary-meta eff assoc ::effect-defn fn))

(defn ^:no-doc opt-return-state-schema [s]
  (s/conditional base/returned? (base/return-schema s s/Any s/Any)
                 :else s))

(defmacro defn-effect
  "A macro similar to defn, that defines a new effect.

```
(defn-effect my-effect [arg]
  (change-the-world! args))
```

Calling it returns an effect action, which can be returned by an item
  as an action. The body of the effect then executed later, when side
  effects on some external entity are safe.
 "
  [name args & body]
  (let [[[name _ result-schema?] args & body] (apply maybe-schema-arg name args body)
        [docstring? args & body] (apply maybe-docstring args body)
        result-schema? (when (some? result-schema?)
                         `(opt-return-state-schema ~result-schema?))]
    `(defn+ [effect-from-defn ~name] identity [effect] [] ~name ~result-schema? ~docstring? ~args ~@body)))

(defn- parse-binding-form [bf & more]
  (cond
    ;; [a b :local ""] :- T
    (and (vector? bf) (= 4 (count bf)) (= :local (nth bf 2)))
    (let [[a b _ init] bf
          [x & body] (apply maybe-schema-arg [a b] more)]
      {:dynamic (vec x)
       :local init
       :body body})

    ;; x or x :- T
    :else
    (let [[x & body] (apply maybe-schema-arg bf more)]
      {:dynamic (vec x)
       :body body})))


(defmacro with-state-as
  "Returns an item that can look differently depending on its current state.

The basic form is

```
(with-state-as foo
  (dom/div (pr-str foo)))
```

where `foo` is bound to the current state of the returned item.
The binding form can optionally be followed by a schema.core annotation:

```
(with-state-as foo :- s/Str
  (dom/div foo))
```

The binding form can also use destructuring

```
(with-state-as {name :name}
  (dom/div name))
```

If the binding form is a vector, the second part of the
  state can be declared to be local to the body item of the form:

```
(with-state-as [outer inner :local \"\"]
  (dom/div (str outer \"->\" inner)))
```

Note that the state of the inner item (the `div` in this case), will
  be a tuple (see [[local-state]]) of both outer and inner state, and
  that the state of the item created by `with-state-as` is just the first
  part of that tuple. The expression after `:local` is the initial
  value of the local state.
"
  [binding-form & body]
  ;; TODO: forgetting to write :local should give a helpful error msg.
  (let [p (apply parse-binding-form binding-form body)]
    (cond
      (contains? p :local)
      `(local-state ~(:local p)
                    (dynamic (s/fn ~(:dynamic p) ~@(:body p))))
      
      :else
      `(dynamic (s/fn ~(:dynamic p) ~@(:body p))))))

(defn ^:no-doc local-dynamic [init f & args]
  (local-state init (apply dynamic f args)))

(defn- resolve* [env symbol]
  (:name (ana/resolve-var env symbol)))

(defn- var-sym [var]
  (when var
    (symbol (str (:ns (meta var)))
            (name (:name (meta var))))))

#?(:clj
   (let [with-state-as-sym (var-sym #'with-state-as)]
     (defn- with-state-as-sym? [env expr]
       (and (symbol? expr)
            (= with-state-as-sym
               (if (sm/cljs-env? env)
                 (resolve* env expr)  ;; cljs only
                 (var-sym (resolve env expr))) ;; clj only
               )))))

(defn- maybe-with-state-as-expr [env body]
  #?(:clj (and (not-empty body)
               (let [candidate (last body)]
                 (when (and (list? candidate)
                            (<= 2 (count candidate))
                            (with-state-as-sym? env (first candidate)))
                   [(butlast body) (apply parse-binding-form (rest candidate))]))))
  ;; just to make the compiler happy - macro expansion is not done in cljs, is it?
  #?(:cljs (throw (ex-info "ClojureScript macro??" {}))))

(defn ^:no-doc local-dynamic+p [prelude-fn init f & args]
  (apply prelude-fn args)
  (apply local-dynamic init f args))

(defn ^:no-doc dynamic+p [prelude-fn f & args]
  (apply prelude-fn args)
  (apply dynamic f args))

(defn- maybe-static [cand & rest]
  (if (= :static cand)
    (list* true rest)
    (list* false (cons cand rest))))

(defn ^:no-doc parse-defn-item-args [name & args]
  (let [[static? & args] (apply maybe-static args)]
    (if static?
      (let [[docstring? params & body] (apply maybe-docstring args)]
        (list* name true nil docstring? params body))
      (let [[[name _ state-schema?] & args] (apply maybe-schema-arg name args)
            [docstring? params & body] (apply maybe-docstring args)]
        (list* name false state-schema? docstring? params body)))))

(defmacro defn-item
  "A macro like [[clojure.core/defn]] to define abstractions over
  items, with optional schema annotations similar to [[schema.core/defn]].

```
(defn-item col [w :- s/Int & content]
  (apply dom/div {:class (str \"col-\" w)} content))
```

  The schema of the state that the returned item expects can also be
  specified like this:

```
(defn-item username :- s/Str []
  (with-state-as name
    (dom/div \"Username:\" name)))
```

  Note that this does not specify the schema of the return value of
  the defined function, which is always an item.

  Another alternative defines the returned items to be [[static]],
  which means they do not depend on their state:

```
(defn-item menu-item :static [href]
  (dom/a {:href href}))
```

  In all cases, the name of the var is attached to the returned items, which
  can be helpful in testing and debugging utilities (see [[named]])."
  [name params & body]
  (let [[name static? state-schema? docstring? params & body] (apply parse-defn-item-args name params body)]
    ;; TODO: proper and helpful error messages:
    (assert (or (not static?) (not state-schema?))) ;; either or
    (assert (or (nil? docstring?) (string? docstring?)))
    (assert (vector? params))
    (if static?
      `(defn-named+ [(f/comp static f/partial)] nil ~name ~docstring? nil ~params ~@body)
      ;; Optimization: if the body is a [[with-state-as]] expression, then lift that up to make it static (non-generative):
      (if-let [[prelude p] (maybe-with-state-as-expr &env body)]
        (let [body (:body p)
              prelude-fn `(s/fn ~params ~@prelude)]
          (cond
            (contains? p :local)
            `(defn-named+ [local-dynamic+p ~prelude-fn ~(:local p)] ~(:dynamic p) ~name ~docstring? ~state-schema? ~params ~@body)
          
            :else
            `(defn-named+ [dynamic+p ~prelude-fn] ~(:dynamic p) ~name ~docstring? ~state-schema? ~params ~@body)))
      
        ;; else
        `(defn-named+ nil nil ~name ~docstring? ~state-schema? ~params ~@body)))))


(defrecord ^{:private true :rtd-record? true} CallHandler [id f args])

(defn- unique-id []
  #?(:cljs (random-uuid))
  #?(:clj (Object.)))

(def ^:private make-unique-id
  (handle-effect-result (fn [_ uuid]
                          (return :state uuid))
                        (effect unique-id)))

(let [bound (fn [id h inner-state & args]
              (return :action (CallHandler. id h args)))
      binder (fn [id h]
               (when (some? h)
                 (f/partial bound id h)))
      handler (fn [id state a]
                (if (and (instance? CallHandler a)
                         (= id (:id a)))
                  (apply (:f a) state (:args a))
                  (return :action a)))
      dyn (fn [[_ id] f]
            (focus lens/first
                   (-> (f (f/partial binder id))
                       (handle-action (f/partial handler id)))))]
  (defn with-bind
    "Returns an item that will call `f` with a `bind` function, which
  should return an item then. The `bind` function can then be used, to
  bind an event handler function to operate on the state of the
  returned item, even if it is used on a dom element with a different
  state, or it can also be triggered directly via [[call]] from a
  handler function with a different state.

  Note: [[reacl-c.dom/defn-dom]] does this binding automatically, so
  you usually don't have to use this yourself."
    [f]
    {:pre [(ifn? f)]}
    (local-state (base/make-initializer unique-id nil)
                 (dynamic dyn f))))

(defn call
  "Calls an event handler function `f`, which should be a function
  returned from calling the `bind` function of a [[with-bind]]
  item. Note that this returns a special [[return]] value, which you
  must return from another event handler, and which you can combine
  with local state changes via [[merge-returned]]."
  [f & args]
  ;; f = result of (binder h) in with-bind above - TODO: assert that?
  (if (some? f)
    (apply f nil args)
    (return)))
