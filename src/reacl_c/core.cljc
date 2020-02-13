(ns reacl-c.core
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [clojure.set :as set]
            [schema.core :as s]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [deref partial constantly empty]))

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

;; TODO: need for getDerivedStateFromProps, getSnapshotBeforeUpdate ?

(defn fragment
  "Returns a container item consisting of the given child items."
  [& children]
  {:pre [(every? base/item? children)]}
  (base/->Fragment children))

(def ^{:doc "An invisible item with no behavior."} empty (fragment))

(defn with-ref
  "Creates an item for which `(f ref & args)` is called when it is
  rendered, which should return an item, and where `ref` is a fresh
  *reference*. A reference should be assigned to one of the items
  below via [[set-ref]]. You can use it as the target of
  a `(return :message [target msg])` for example."
  [f & args]
  (base/->WithRef f args))

;; TODO: add this? is this safer than with-ref... which is hard to use correctly. Or add (handle-message ref ...)
#_(declare set-ref)
#_(defn with-self-ref [f]
  (with-ref (fn [ref]
              (-> (f ref)
                  (set-ref ref)))))

(defn set-ref
  "Returns an item identical to the given item, but with the given
  reference assigned. Replaces the reference previously assigned to
  it. See [[with-ref]] for a description of references."
  [item ref]
  ;; Note: when setting a ref on a dom item, we don't want the
  ;; class/component, but the 'raw' dom element. Pass the ref down to
  ;; get that.
  (if (dom/element? item)
    (dom/set-ref item ref)
    (base/->SetRef item ref)))

(defn deref
  "Returns an implementation specific value, usable as a target in
  messages sending or to access the native dom
  elements. See [[with-ref]] for a description of references."
  [ref]
  (base/-deref-ref ref))

(defn ^:no-doc dynamic [f & args]
  {:pre [(ifn? f)]}
  (base/->Dynamic f args))

(def ^{:doc "The *identity lens* that does not modify the yanked of
shoved values."} id-lens
  (fn ([v] v)
    ([_ v] v)))

(def ^{:doc "A lens over the first item of a vector."} first-lens
  (fn
    ([[a _]] a)
    ([[_ b] a] [a b])))

(def ^{:doc "A lens over the second item of a vector."} second-lens
  (fn
    ([[_ b]] b)
    ([[a _] b] [a b])))

(defn focus
  "Returns an item that focuses the outer state, to the part of it
  that `item` shall see, via the given *lens*. Otherwise behaves and
  looks the same."
  [lens item]
  {:pre [(base/item? item)
         (base/lens? lens)]}
  (if (= lens id-lens)
    item
    (base/->Focus item lens)))

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
- `:message` means that the given message is sent to the given target reference.

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
        (base/->Returned state (persistent! actions) (persistent! messages))
        (let [arg (second args)
              nxt (nnext args)]
          (case (first args)
            (:state) (do (when-not (= base/keep-state state)
                           (assert false (str "A :state argument to return must be specified only once.")))
                         (recur nxt arg actions messages))
            (:action) (recur nxt state (conj! actions arg) messages)
            (:message) (let [[target msg] arg]
                         (assert (some? target) "Missing target for message.")
                         (assert (base/ref? target) "Target must be a reference created by with-ref.")
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
  via [[send-message!]] or [[return]]), by calling `(f message)`,
  which must return a [[return]] value. The resulting item
  otherwise looks and behaves exactly like the given one."
  [f item]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/->HandleMessage f item))

;; TODO: add a (with-handle-message (fn [send!] (div ...)) (fn [msg] ...)) ?

(defn handle-action
  "Handles actions emitted by given item, by evaluating `(f action)` for each
  of them. That must return the result of calling [[return]] with
  either a new state, and maybe one or more other actions (or the
  given action unchanged). "
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/->HandleAction item f))

(let [h (fn [f a] (return :action (f a)))]
  (defn map-actions
    "Returns an item that emits actions `(f action)`, for each
  `action` emitted by `item`, and otherwise looks an behaves exacly
  the same."
    [item f]
    (handle-action item (f/partial h f))))

(def partial f/partial)
(def constantly f/constantly)

(let [h (fn [ref msg]
          (return :message [ref msg]))]
  (defn ^:no-doc redirect-messages [ref item]
    {:pre [(satisfies? base/Ref ref)
           (base/item? item)]}
    (handle-message item (f/partial h ref))))

(let [h (fn [f ref msg]
          (return :message [ref (f msg)]))
      wr (fn [f item ref]
           (handle-message (set-ref item ref) (f/partial h f ref)))]
  (defn ^:no-doc map-messages
    "Returns an item like `e`, that transforms all messages sent to
  it though `(f msg)`, before they are forwarded to `e`."
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
  (base/->NameId s))

(defn named
  "Returns an item that looks and works exactly like the given item,
  but with has a user defined name, that appears and can be used in
  testing and debugging utilities. Use [[name-id]] to generate a
  unique name object. See [[def-named]] and [[defn-named]] for more
  convenient ways to create named items."
  [name-id item]
  {:pre [(base/item? item)
         (base/name-id? name-id)]}
  (base/->Named name-id item))

(defn- id-merge [m1 m2]
  (reduce-kv (fn [r k v]
               (if (= (get r k) v)
                 r
                 (assoc r k v)))
             m1
             m2))

(def ^{:doc "A lens over a tuple of maps or records, that yields a
merged map of both. If both maps or records have fields of the same
name, only the value of the second part of the tuple is used and updated on
a change."}  merge-lens
(fn
  ([[s1 s2]] (merge s1 s2))
  ([[s1 s2] ns]
   ;; Note: if s1/s2 are records, then this restores that:
   ;; id-merge makes the result be identical? if all updated keys are =
   (let [k1 (set (keys s1))
         k2 (set (keys s2))]
     [(id-merge s1 (select-keys ns (set/difference k1 k2)))
      (id-merge s2 (select-keys ns k2))]))))

(defn ^:no-doc local-state [initial item]
  {:pre [(base/item? item)]}
  (base/->LocalState item initial))

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

(defn hide-merged-state
  "Hides a part of the state of an item, which must be a map or
  record. The hidden part is specified by the given initial value,
  which can also be a map or record type. The resulting item has the
  same state as the given item, except that the keys in `initial` are
  removed."
  [item initial]
  (add-state initial merge-lens item))

(defn- isolate-lens
  ([[outer inner]] inner)
  ([[outer inner] new-inner] [outer new-inner]))

(defn isolate-state
  "Hides the state of the given item completely, resulting in an
  item with an arbitrary state that is inaccessible for it."
  [initial-state item]
  (add-state initial-state isolate-lens item))

(defn keyed
  "Adds an arbitrary identifier to the given item, which will be used
  to optimize rendering of it in a list of children of a container
  item."
  [item key]
  {:pre [(base/item? item)]}
  (base/->Keyed item key))

#_(defn- lift-static-return [v]
  (if (base/returned? v)
    (if (= base/keep-state (:state v))
      (f/constantly v)
      (throw (ex-info "The 'return' value used here must not contain a state update. Use a function instead." {:value v})))
    v))

(defn once  ;; akin to a 'React effect'.
  "An item that evaluates `(f state)` and emits the [[return]] value
  that it must return. On subsequent state updates, `f` is called too,
  but the returned [[return]] value is only emitted if it different
  than last time. The optional `(cleanup-f state)` is evaluated, when
  the item is removed from the item tree afterwards.

  Note that if you return a modified state, you must be careful to not
  cause an endless loop of updates."
  [f & [cleanup-f]]
  {:pre [(ifn? f)
         (or (nil? cleanup-f) (ifn? cleanup-f))]}
  (base/->Once f cleanup-f))

(defn ^:no-doc handle-state-change
  "An item like the given item, but when a state change is emitted by
  `item`, then `(f prev-state new-state)` is evaluated, which must
  return a [[return]] value. By careful with this, as item usually
  expect that their changes to the state are eventually successful."
  [item f]
  {:pre [(base/item? item)
         (ifn? f)]}
  (base/->HandleStateChange item f))

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

(defn ^:no-doc with-async-return [f & args]
  {:pre [(ifn? f)]}
  (base/->WithAsyncReturn f args))

(let [send! (fn [return! target msg]
              (return! (return :message [target msg])))
      h (fn [return! f args]
          (apply f (f/partial send! return!) args))]
  (defn ^:no-doc with-async-messages [f & args]
    {:pre [(ifn? f)]}
    (with-async-return h f args)))

(let [h (fn [return! action]
          (return! (return :action action)))
      g (fn [return! f args]
          (apply f (f/partial h return!) args))]
  (defn ^:no-doc with-async-actions [f & args]
    {:pre [(ifn? f)]}
    (with-async-return g f args)))

(defn ^:no-doc effect [f & args]
  (base/->Effect f args))

(defrecord ^:no-doc SubscribedMessage [stop!])

(defrecord ^:no-doc SubscribedEmulatedResult [action])

(defn- subscribe! [f args deliver! host]
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

(let [store-sub (fn [f args msg]
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
                       (once (f/constantly (return :action (subscribe-effect f deliver! args host))))))
      dyn (fn [deliver! {f :f args :args stop! :stop!}]
            (if (some? stop!)
              (once (f/constantly (return)) (f/constantly (return :action (unsubscribe-effect stop! f args))))
              (with-ref (f/partial msgs deliver! f args))))
      stu (fn [f args deliver!]
            ;; Note: by putting f and args in the local state, we get an automatic 'restart' when they change.
            (isolate-state {:f f
                            :args args
                            :stop! nil}
                           (dynamic (f/partial dyn deliver!))))]
  (defn subscription
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
  (base/->ErrorBoundary item f))

(let [set-error (fn [state error]
                  (return :state [state error]))
      dyn (fn [[state error] try-e catch-e]
            (if (some? error)
              catch-e
              (-> (focus first-lens try-e)
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
        (hide-state nil id-lens))))

(let [df (fn [e validate! state]
           ;; state passed down!
           (validate! state :down)
           e)
      mf (fn [validate! old new]
           ;; state passed up!
           (validate! new :up))]
  (defn validation-boundary
    "Creates a state validation boundary around the given item,
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

(defn- subscription-from-defn [fn f & args]
  ;; Wanted to add some meta data for testing; but unfortunately (vary-meta f ..) is never = (vary-meta f ...)
  #_(apply subscription (vary-meta f
                                 assoc ::subscription-defn fn)
           args)
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

(defn- effect-from-defn [fn eff]
  (vary-meta eff assoc ::effect-defn fn))

(defmacro defn-effect
  "A macro similar to defn, that defines a new effect.

```
(defn-effect my-effect [arg]
  (change-the-world! args)
  (return)
```

Calling it returns an effect action, which can be returned by an item
  as an action. The body of the effect then executed later, when side
  effects on some external entity are safe.
 "
  [name args & body]
  ;; TODO: allow nil to be returned?
  `(defn+ [effect-from-defn ~name] identity [effect] [] ~name ~args ~@body))
