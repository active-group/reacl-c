(ns reacl-c.core
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [clojure.set :as set]
            [schema.core :as s]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [deref partial constantly empty comp]))

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
;; - simplify local-state + dynamic somehow?

(defn fragment
  "Returns a container item consisting of the given child items."
  [& children]
  {:pre [(base/assert-item-list "fragment" children)]}
  (base/make-fragment children))

(def ^{:doc "An invisible item with no behavior."} empty (fragment))

(defn with-ref
  "Creates an item identical to the one returned from `(f ref &
  args)`, where `ref` is a fresh *reference*. A reference should be
  assigned to one of the items below via [[set-ref]]. You can use it
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

(defn set-ref
  "Returns an item identical to the given item, but with the given
  reference assigned. Note that the returned item cannot be used more
  than once. See [[with-ref]] for a description of references."
  [item ref]
  {:pre [(base/item? item)]}
  ;; Note: when setting a ref on a dom item, we don't want the
  ;; class/component, but the 'raw' dom element. Pass the ref down to
  ;; get that.
  (if (dom/element? item)
    (dom/set-ref item ref)
    (base/make-set-ref item ref)))

(let [c (fn [refs items f args]
          (apply f (map set-ref items refs) args))]
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
  ;; TODO: add to docs: for highest performance, use ref-let*
  (assert (even? (count bindings)))
  (let [[names items] (let [l (partition-all 2 bindings)]
                        [(map first l) (map second l)])]
    `(ref-let* (list ~@items)
               ;; must create a fresh function here :-/
               (fn [[~@names]]
                 ~@body))))

(defn deref
  "Returns an runner specific value, which might be a native dom
  element backing an item at runtime for example. See [[with-ref]] for
  a description of references."
  [ref]
  ;; TODO: needs more to access to actually access the native dom; move this to 'browser namespace'?
  ;; TODO: allow for set-ref items?
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
  {:pre [(base/item? item)
         #_(base/lens? lens)]}
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
                         (assert (base/message-target? target) "Target must be a reference created by with-ref or an item created by set-ref.")
                         (recur nxt state actions (conj! messages [target msg])))
            (do (assert (contains? #{:state :action :message} (first args)) (str "Invalid argument " (first args) " to return."))
                (recur nxt state actions messages))))))))

(defn send-message!
  "Sends a message to a running application, i.e. `app` must be the
  value returned from [[reacl-c.browser/run]] for example. This can be
  used together with [[handle-message]] in situations where the
  application is not running standalone, but integrated in a different
  framework."
  [app msg]
  {:pre [(satisfies? base/Application app)]}
  (base/-send-message! app msg))

(defn handle-message
  "Handles the messages sent to the the resulting item (either
  via [[send-message!]] or [[return]]), by calling `(f state message)`,
  which must return a [[return]] value. The resulting item
  otherwise looks and behaves exactly like the given one."
  [f item]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-handle-message f item))

(let [h (fn [ref handle-msg f args]
          (fragment (-> (handle-message handle-msg empty)
                        (set-ref ref))
                    (apply f ref args)))]
  (defn with-message-target
    "Returns an item like ´(f target & args)`, where `target` is a
  reference that can be used as a message target, which are then
  handled by a call to `(handle-msg state msg)`, which must return
  a [[return]] value."
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

(defn action-or-nil [f state a]
  (let [aa (f a)]
    (return :action (if (nil? aa) a aa))))

(defn map-actions
  "Returns an item that emits actions `(f action)`, for each
  action emitted by `item`, and otherwise looks an behaves exacly
  the same. If `(f action)` is nil, then the original action is kept,
  allowing for `f` to be a map of the actions to replace."
  [item f]
  {:pre [(ifn? f)]}
  ;; OPT: if f is a map, we could create a predicate from the keys...?
  (handle-action item (f/partial action-or-nil f)))

(defn map-effects
  "Returns an item that emits actions `(f effect)`, for each effect
  action` emitted by `item`, and otherwise looks an behaves exacly the
  same. If `(f effect)` is nil, then the original effect action is
  kept, allowing for `f` to be a map of the effects to replace."
  [item f]
  {:pre [(ifn? f)]}
  (handle-effect item (f/partial action-or-nil f)))

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

(let [h (fn [f ref state msg]
          (return :message [ref (f msg)]))
      wr (fn [f item ref]
           (handle-message (f/partial h f ref) (set-ref item ref)))]
  (defn map-messages
    "Returns an item like the given one, that transforms all messages sent to
  it though `(f msg)`, before they are forwarded to `item`."
    [f item]
    {:pre [(ifn? f)
           (base/item? item)]}
    (with-ref (f/partial wr f item))))

(defn name-id
  "Generates a fresh unique value that can be used to generate named
  items via [[named]]. Note that calling this twice with the same
  name returns different values."
  [s]
  {:pre [(string? s)]}
  (base/make-name-id s))

(defn named
  "Returns an item that looks and works exactly like the given item,
  but with has a user defined name, that appears and can be used in
  testing and debugging utilities. Use [[name-id]] to generate a
  unique name object. See [[def-named]] and [[defn-named]] for more
  convenient ways to create named items."
  [name-id item]
  {:pre [(base/item? item)
         (base/name-id? name-id)]}
  (base/make-named name-id item))

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

(defn add-merged-state ;; TODO: remove?!
  "Adds new map or record fields that the given item sees as it's
  state, by merging the the given initial record or hash-map with the
  state of the resulting item. The given item can then update any
  field, but the fields from `initial are 'removed' from the outer
  state. Note that any fields not in the `initial` value are put in
  the outer state. If there are duplicate fields, the inner state
  'wins'."
  [initial item]
  (add-state initial lens/merge item))

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

#_(defn- lift-static-return [v]
  (if (base/returned? v)
    (if (= base/keep-state (:state v))
      (f/constantly v)
      (throw (ex-info "The 'return' value used here must not contain a state update. Use a function instead." {:value v})))
    v))

(defn lifecycle
  "Returns an invisible item, that calls `init` each time the item is
  used at a place in the component hierarchy, including every change
  of state or the `init` function itself subsequently. The `finish`
  function is called when the item is no longer used at that
  place. Both functions must return a [[return]] value specifying what
  to do."
  [init finish]
  (base/make-lifecycle init finish))

(let [add-inner (fn [returned outer inner]
                  (let [s (base/returned-state returned)]
                    (base/merge-returned returned
                                         (return :state
                                                 (if (= s base/keep-state)
                                                   [outer inner]
                                                   [s inner])))))
      init (fn [f [state done]]
             (let [v (f state)]
               (if (not= v done)
                 (add-inner v state v)
                 (return))))
      finish (fn [cleanup-f [state done]]
               (add-inner (cleanup-f state) state nil))
      no-cleanup (f/constantly (return))]
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
    (local-state nil
                 (lifecycle (f/partial init f)
                            (if cleanup-f
                              (f/partial finish cleanup-f)
                              no-cleanup)))))

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
  (lifecycle (f/constantly (return)) f))

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
  resulting item."
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
    ;; Note: not really parallelized yet, but will be run in one update cycle.
    (apply seq-effects (first-eff eff) (map next-eff effs))))

(defn- send-effect-result [host result]
  (return :message [host result]))

(let [wr (fn [ref eff]
           (init (return :action (seq-effects eff (f/partial effect send-effect-result ref)))))]
  (defn handle-effect-result
    "Runs the given effect once, feeding its result into `(f state
  result)`, which must return a [[return]] value."
    [f eff]
    {:pre [(base/effect? eff)
           (ifn? f)]}
    (with-message-target f
      wr eff)))

(defrecord ^:no-doc SubscribedMessage [stop!])

(defrecord ^:no-doc SubscribedEmulatedResult [action])

(defn ^:no-doc subscribe! [f args deliver! host]
  (let [stop! (apply f deliver! args)]
    (assert (ifn? stop!) "Subscription must return a stop function.")
    (return :message [host (SubscribedMessage. stop!)])))

(defn- subscribe-effect [f deliver! args host]
  (assert (ifn? f))
  (effect subscribe! f args deliver! host))

(defn- unsubscribe! [stop! _]
  (stop!)
  (return))

(defn- unsubscribe-effect [stop! f args]
  ;; Note: f and args only here to enable a test with [[unsubscribe-effect?]] below.
  (effect unsubscribe! stop! (cons f args)))

(let [store-sub (fn [f args state msg]
                  (cond
                    (instance? SubscribedMessage msg)
                    (return :state {:f f :args args :stop! (:stop! msg)})

                    ;; added for test-util emulation of subscriptions.
                    (instance? SubscribedEmulatedResult msg)
                    (return :action (:action msg))

                    :else
                    (do (assert false (str "Unexpected message:" (pr-str msg)))
                        (return))))
      msgs (fn [deliver! f args host]
             (fragment (-> (handle-message (f/partial store-sub f args)
                                           (fragment))
                           (set-ref host))
                       (init (return :action (subscribe-effect f deliver! args host)))))
      dyn (fn [deliver! {f :f args :args stop! :stop!}]
            (if (some? stop!)
              (cleanup (f/constantly (return :action (unsubscribe-effect stop! f args))))
              (with-ref (f/partial msgs deliver! f args))))
      stu (fn [f args deliver!]
            ;; Note: by putting f and args in the local state, we get an automatic 'restart' when they change.
            (isolate-state {:f f
                            :args args
                            :stop! nil}
                           (dynamic (f/partial dyn deliver!))))]
  (defn subscription
    "Returns an item that asynchronously emits actions according to
  the given function `f`. For that `f` will be called with a
  side-effectful `deliver!` function which takes the action to emit,
  and `f` must return a `stop` function of no arguments. You may do
  some kind of registration at an asynchronous library or native
  browser api, and use `deliver!` to inform your application about the
  results, once or multiple times. But when the `stop` function is
  called, you must prevent any more calls to `deliver!`."
    [f & args]
    (with-async-actions (f/partial stu f args))))

(defn error-boundary
  "Creates an error boundary around the given item. When the rendering
  of `e` throws an exception, then `(f error)` is evaluated, and must
  result in an [[return]] value. Note that exceptions in functions
  like [[handle-action]], are not catched by this. See [[try-catch]]
  for a higher level construct to handle errors."
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/make-error-boundary item f))

(let [set-error (fn [state error]
                  (return :state [state error]))
      dyn (fn [[state error] try-e catch-e]
            (if (some? error)
              catch-e
              (-> (focus lens/first try-e)
                  (error-boundary (f/partial set-error state)))))]
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

(let [df (fn [e validate! state]
           ;; state passed down!
           (validate! state :down)
           e)
      mf (fn [validate! old new]
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
    (-> (dynamic (f/partial df item validate!))
        (monitor-state (f/partial mf validate!)))))

#_(let [h (fn [state conds-items default-item]
          (or (some (fn [[cond-f item]]
                      (and (cond-f state)
                           item))
                    (partition-all 2 conds-items))
              default-item
              (throw (ex-info "No condition matched the state, and no default item given." {:state state}))))]
  (defn dynamic-cond* [conds-items & [default-item]]
    (dynamic h conds-items default-item)))

#_(defmacro dynamic-cond [& clauses]
  (let [clauses_ (partition-all 2 clauses)
        [clauses_ dflt] (if (= :else (first (last clauses_)))
                          [(drop-last clauses_) (second (last clauses_))]
                          [clauses_ nil])]
    `(dynamic-cond* ~(mapcat (fn [[test item]]
                               `[~test
                                 ~item])
                             clauses_)
                    ~dflt)))

(defmacro def-named
  "A macro to define a named item. This is the same as Clojures
  `def`, but in addition assigns its name to the item which can be
  used by testing and debugging utilities."
  [name item]
  (let [name_ (str *ns* "/" name)]
    `(let [id# (name-id ~name_)]
       (def ~name
         (named id# ~item)))))

(defn ^:no-doc meta-name-id [v]
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

(defmacro ^:no-doc defn+
  "Internal utility macro."
  [create mod-fn opt-wrapper wrapper-args name args & body]
  (let [[docstring? args & body] (apply maybe-docstring args body)
        name_ (clojure.core/name name)
        name (vary-meta name assoc
                        :doc (or docstring? (:doc (meta name)))
                        :arglists `'(~args))]
    `(let [check-arity# (arity-checker ~name_ ~(arity args))
           f# (s/fn ~name [~@wrapper-args ~@args] ~@body)
           check-args-schema# (s/fn ~name [~@args] nil)]
       (def ~name
         (~mod-fn (fn [& args#]
                    (assert (do (check-arity# (count args#))
                                (apply check-args-schema# args#)
                                true))               
                    (~@create (apply ~@opt-wrapper f# args#))))))))

(defmacro ^:no-doc defn-named+
  "Internal utility macro."
  [opt-wrapper wrapper-args name args & body]
  (let [name_ (str *ns* "/" name)]
    `(let [id# (name-id ~name_)]
       (defn+ [named id#] (fn [f#] (vary-meta f# assoc ::name-id id#))
         ~opt-wrapper ~wrapper-args ~name ~args ~@body))))

(defmacro defn-named
  "A macro to define an abstract item. This is the same as Clojures
  `defn`, but in addition assigns its name to the returned item which can be
  used by testing and debugging utilities."
  [name args & body]
  `(defn-named+ nil nil ~name ~args ~@body))

(defn- maybe-schema-arg [candidate & more]
  (if (and (not-empty more) (= ':- (first more)))
    (list* (list candidate (first more) (second more)) (rest (rest more)))
    (list* (list candidate) more)))

(defmacro defn-dynamic
  "A macro to define a new abstract dynamic item. For example, given

```
(defn-dynamic greeting state [arg]
  (dom/div (str arg \" \" state)))
```

  You can create a new dynamic item by calling `(greeting \"Hello\")`, which looks exactly like

```
(dom/div (str \"Hello\" \" \" \"world\")
```

  when the current state of the item is `state`, and changes whenever the state changes."
  [name state args & body]
  (let [[docstring? state args & body] (apply maybe-docstring state args body)
        [statev args & body] (apply maybe-schema-arg state args body)]
    `(defn-named+ [dynamic] ~statev ~name ~@(when docstring? [docstring?]) ~args ~@body)))

(defmacro def-dynamic
  "A macro to define a new dynamic item. For example, given

```
(def-dynamic current-state state
  (dom/span \"Current state is: \" (pr-str state)))
```

  then `current-state` is an item that shows the current state as it
  changes over time. This is similar to [[defn-dynamic]] but without the
  arguments."
  [name state & body]
  (let [[statev & body] (apply maybe-schema-arg state body)]
    `(def-named ~name (dynamic (s/fn [~@statev] ~@body)))))

(defmacro defn-static
  "Defines `name` to a function, returning a [[static]] item like the
  item define by the function body. The static item is independant of
  the outside state, and depends only on the argument values. Compared
  to an ordinary function, this can greatly increase performance, as
  the body is only evaluated when the arguments change."
  [name args & body]
  (let [[docstring? args & body] (apply maybe-docstring args body)]
    `(defn-named+ [(f/comp static f/partial)] nil ~name ~@(when docstring? [docstring?]) ~args
       ~@body)))

(defmacro def-static
  "Defines `name` to be a [[static]] item that is always like `item`,
  independant of the state."
  [name item]
  `(def-named ~name (static (f/constantly item))))

(defn- subscription-from-defn [fn f & args]
  (apply subscription f args))

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

Note that `deliver!` must never be called directly in the body of
  `defn-subscription`, but only later, from an *asynchronous context*.
  Also note that the body is evaluated as soon as the subscription
  item is mounted into your application, and that it must result in
  a function with no arguments, which is called when the item is
  removed from the application afterwards.
 "
  [name deliver! args & body]
  (let [[docstring? deliver! args & body] (apply maybe-docstring deliver! args body)]
    (assert (symbol? deliver!) "Expected a name for the deliver function before the argument vector.")
    `(defn-named+ [subscription-from-defn ~name] [~deliver!] ~name ~@(when docstring? [docstring?]) ~args ~@body)))

(defn ^:no-doc effect-from-defn [fn eff]
  ;; Note: must be public, because used in macro expansion of defn-effec.
  (vary-meta eff assoc ::effect-defn fn))

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
  ;; TODO: allow a schema on the return value
  `(defn+ [effect-from-defn ~name] identity [effect] [] ~name ~args ~@body))
