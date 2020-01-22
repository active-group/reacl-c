(ns reacld.core
  (:require [reacld.base :as base]
            [clojure.set :as set]))

;; Rationale:
;; The basic building block is en Element (base/E), which is merely
;; equivalent to a Reacl class with app-state but without any
;; arguments (which makes composition a lot easier). But because every
;; element is highly 'customized' by user arguments, it would be hard
;; to retain any of the rerendering optimizations by Reacl/React (and
;; any local-state). Second, elements could be represented as a [class
;; args] tuple; but to detach the definition from the implementation,
;; they are instead represented as [type args], and the implementing
;; Reacl class is added via extend-type in 'impl/reacl'.

(defn ^:no-doc with-async-actions [f & args]
  (base/->WithAsyncActions f args))

(defn fragment
  "Creates an element with a number of child elements, that is
  otherwise invisible, i.e. the child elements are 'propagated' to the
  parent element."
  [& children]
  (base/->Fragment children))

(defn ^:no-doc dynamic [f & args]
  (base/->WithState f args))

(def ^{:doc "The *identity lens* that does not modify the yanked of
shoved values."} id-lens
  (fn ([v] v)
    ([_ v] v)))

(def ^{:doc "A lens over the first element of a vector."} first-lens
  (fn
    ([[a _]] a)
    ([[_ b] a] [a b])))

(def ^{:doc "A lens over the second element of a vector."} second-lens
  (fn
    ([[_ b]] b)
    ([[a _] b] [a b])))

(defn focus
  "Restricts the state of `e` to a part of the state of the resulting
  element, or translates it into a different form, via the given
  *lens*."
  [e lens]
  (if (= lens id-lens)
    e
    (base/->Focus e lens)))

(def ^{:arglists '([:state state :action action :message message])
       :doc "Creates a value to be used for example in the function
       passed to [[handle-action]]. All arguments are optional, and
       `:action` and `:message` may be specified more than one, but
       `:state` only once. At some points where such a *return value*
       is expected, not all options may be valid, but if they are, then

- `:state`   means that the element changes its state to the given value
- `:action`  means that the given action is emitted by the element
- `:message` means that the given message is sent to the element.

If not `:state` option is used, the state of the element will not change.
"}
  return
  (fn [& args]
    (assert (even? (count args)) "Expected an even number of arguments.")
    (loop [args (seq args)
           state nil
           actions (transient [])
           messages (transient [])]
      (if (empty? args)
        (base/->Returned state (persistent! actions) (persistent! messages))
        (let [arg (second args)
              nxt (nnext args)]
          (case (first args)
            (:state) (do (when-not (nil? state)
                           (assert false (str "A :state argument to return must be specified only once.")))
                         (recur nxt [arg] actions messages))
            (:action) (recur nxt state (conj! actions arg) messages)
            (:message) (recur nxt state actions (conj! messages arg))
            (do (assert (contains? #{:state :action :message} (first args)) (str "Invalid argument " (first args) " to return."))
                (recur nxt state actions messages))))))))

(defn send-message!
  "Sends a message to a running application, i.e. `app` must be the
  value returned from [[reacld.browser/run]] for example. This can be
  used together with [[handle-message]] in situations where the
  application is not running standalone, but integrated in a different
  framework."
  [app msg]
  (base/-send-message! app msg))

(defn handle-message
  "Handles the messages sent to the the resulting element (either
  via [[send-message!]] or [[return]]), by calling `(f state message &
  args)`, which must return a [[return]] value. If `(return :message
  msg)` is used, that message `msg` is sent downwards to `e`. The resulting
  element otherwise looks and behaves exactly like `e`."
  [e f & args]
  (base/->HandleMessage e f args))

(defn handle-action
  "Handles actions emitted by e, by evaluating `(f state action &
  args)` for each of them. That must return the result of
  calling [[return]] with either a new state, and maybe one or more
  other actions (or the given action unchanged). "
  [e f & args]
  (base/->HandleAction e f args))

(let [h (fn [st a f args]
          (return :action (apply f st a args)))]
  (defn map-dynamic-actions
    "Returns an element that emits actions `(f state action & args)`,
  for each `action` emitted by `e` and the current state of the
  element, and otherwise looks an behaves exacly like `e`."
    [e f & args]
    (handle-action h e f args)))

(let [h (fn [_ a f args] (apply f a args))]
  (defn map-actions
    "Returns an element that emits actions `(f action & args)`, for
  each `action` emitted by `e`, and otherwise looks an behaves exacly
  like `e`."
    [e f & args]
    (map-dynamic-actions e h f args)))

(defrecord ^:private SetStateAction [new-state])

(let [set (fn [state a]
            (if (instance? SetStateAction a)
              (return :state (:new-state a))
              (return :action a)))]
  (defn ^:no-doc with-state-setter [f & args]
    ;; TODO 'args' not really neeeded, if f is called immediately.
    ;; TODO: must that action be unique to 'this state'? Could it interfere with others? or use a message then?
    (-> (apply f ->SetStateAction args)
        (handle-action set))))

(let [h (fn [set-state f args]
          (apply dynamic f set-state args))]
  (defn interactive [f & args]
    (with-state-setter h f args)))

(defn named [e name]
  (base/->Named e name))

(defn- id-merge [m1 m2]
  (reduce-kv (fn [r k v]
               (if (identical? (get r k) v)
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
   ;; id-merge makes the result be identical? if all updated keys are identical?
   (let [k1 (set (keys s1))
         k2 (set (keys s2))]
     [(id-merge s1 (select-keys ns (set/difference k1 k2)))
      (id-merge s2 (select-keys ns k2))]))))

(defn- local-state [e initial]
  (base/->LocalState e initial))

(defn add-state
  "Adds new state that the element `e` can access, via a lens on the
  the tuple of states `[outer inner]`, where the initial value for
  `inner` state is `initial`. Note that the resulting element has only
  `outer` as its state."
  [initial lens e] ;; aka extend-state?
  (local-state (focus e lens) initial))

(defn hide-state
  "Hides a part of the state of an element `e`, via a lens that
  reduces the the tuple of states `[outer inner]`, where the initial
  value for `inner` state is `initial`. The resulting element has only
  `outer` as its state."
  [e initial lens]
  ;; Note: yes, it's actually the same as 'add-state' ;-)
  (add-state initial lens e))

(defn hide-merged-state
  "Hides a part of the state of an element, which must be a map or
  record. The hidden part is specified by the given initial value,
  which can also be a map or record type. The resulting element has
  the same state as `e`, except that the keys in `initial` are
  removed."
  [e initial]
  (add-state initial merge-lens e))

(defn- isolate-lens
  ([[outer inner]] inner)
  ([[outer inner] new-inner] [outer new-inner]))

(defn isolate-state
  "Hides the state of the element `e` completely, resulting in an
  element with an arbitrary state that is inaccessible for `e`."
  [initial-state e]
  (add-state initial-state isolate-lens e))

(defn keyed
  "Adds an arbitrary identifier for `e`, which will be used to
  optimize rendering of it in a list of children of a container
  element."
  [e key]
  (base/->Keyed e key))

(defn when-mounted
  "After the element `e` is mounted into the application, `(f
  component & args)` is called, where `component` is an implementation
  specific runtime representation of `e`, and which must return an
  action that is emitted by the resulting element."
  [e f & args]
  ;; FIXME: allow all 'return's? add current state to the call to f?
  (base/->WhenMounted e f args))

(defn when-unmounting
  "After the element `e` is unmounted from the application, `(f
  component & args)` is called, where `component` is an implementation
  specific runtime representation of `e`, and which must return an
  action that is emitted by the resulting element."
  [e f & args]
  (base/->WhenUnmounting e f args))

(defn after-update
  "After the element `e` is updated after it is mounted into the
  application, e.g. with a new state, `(f component & args)` is
  called, where `component` is an implementation specific runtime
  representation of `e`, and which must return an action that is
  emitted by the resulting element."
  [e f & args]
  (base/->AfterUpdate e f args))

(defn monitor-state
  "When e changes its state, `(f old-state new-state & args)` is
  evaluted, and must return an action that is emitted by the resulting
  element. Note that this is only called when e changes its self 'by
  itself', not if the state was changes somewhere upwards in the
  element tree an is only passed down to the resulting element."
  ;; TODO: all 'return'? document that only truthy values are emitted?
  [e f & args]
  (base/->MonitorState e f args))

(defrecord ^:private Mount [node f args])
(defrecord ^:private Unmount [node f args])

(let [handle (fn [[state mstate] a]
               (condp instance? a
                 Mount
                 (return :state [state (apply (:f a) (:node a) (:args a))])
                 
                 Unmount
                 (do (apply (:f a) (:node a) mstate (:args a))
                     (return))

                 :else (return :action a)))]
  (defn ^:no-doc while-mounted [e mount! unmount! & args]
    (-> e
        (when-mounted ->Mount mount! args)
        (when-unmounting ->Unmount unmount! args)
        (handle-action handle)
        (hide-state nil id-lens))))

(letfn [(mount [_ deliver! f args]
          (apply f deliver! args))
        (unmount [_ stop! deliver! f args]
          (stop!))
        (stu [deliver! f args]
          (while-mounted (fragment) ;; TODO: fragment, or wrap an existing element?
                         mount
                         unmount
                         deliver! f args))]
  (defn ^:no-doc subscribe
    [f & args]
    (with-async-actions stu f args)))

;; TODO: need for getDerivedStateFromProps, getSnapshotBeforeUpdate ?
;; TODO: an 'effect queue' utility for triggering something like 'ajax/post' (state that makes it 'pending' maybe, then a subscription to handle its result)

(defn error-boundary
  "Creates an error boundary around the element `e`. When the
  rendering of `e` throws an exception, then `(f error & args)` is
  evaluated, and must result in an action which is then emitted from
  the resulting element. Note that exceptions in functions
  like [[handle-action]], are not catched by this. See [[try-catch]]
  for a higher level construct to handle errors."
  [e f & args]
  (base/->ErrorBoundary e f args))

(defrecord ^:private ErrorAction [error])

(let [set-error (fn [[state _] act]
                  (condp instance? act
                    ErrorAction (return :state [state (:error act)])
                    (return :action act)))
      dyn (fn [[state error] try-e catch-e]
            (if (some? error)
              catch-e
              (-> (focus try-e first-lens)
                  (error-boundary ->ErrorAction)
                  (handle-action set-error))))]
  (defn try-catch
    "Returns an element that looks an works the same as the element
  `try-e`, until an error is thrown during its rendering. After that
  `catch-e` is rendered instead, with a state of the combined outer
  state and the error - `[state-of-e error]`. The element `catch-e`
  will usually be interactive, for example, displaying the error (and
  the relevant part of the state) to the user, and offer a button to reset the
  error to `nil` and maybe fix the state, after which `try-e` is showed
  again."
    [try-e catch-e]
    (-> (dynamic dyn try-e catch-e)
        (hide-state nil id-lens))))

(let [df (fn [state e validate!]
           ;; state passed down!
           (validate! state :down)
           e)
      mf (fn [old new validate!]
           ;; state passed up!
           (validate! new :up)
           (return))]
  (defn validation-boundary
    "Creates a state validation boundary around the element `e`,
  where `(validate! state :up)` is evaluated for side effects when a
  state change is flowing out of `e` upwards, and `(validate!
  state :down)` is evaluated for side effects when a new state is
  being pushed down to `e`."
    [e validate!]
     ;; Note: dynamic adds it to render; could make a little earlied
     ;; via 'validate clause'; but probably not worth here (as
     ;; instantiation is delayed anyway)
    (-> (dynamic df e validate!)
        (monitor-state mf validate!))))

(defmacro ^:no-doc fn+ [all-args args & body]
  ;; generates a simplified param vector, in order to bind all args also to
  ;; 'all-args', regardless of destructuring.
  (let [[fargs vargs] (partition-by #(= '& %) args)
        fparams (map (fn [_] (gensym "a")) fargs)
        vparam (when (not-empty vargs) (gensym "a"))
        params (vec (concat fparams (when vparam ['& vparam])))
        all (if vparam `(concat ~(vec fparams) ~vparam) (vec fparams))]
    `(fn ~params
       (let [~all-args ~all
             ~args ~all-args]
         ;; Note: this looks unneccessary, but enables the use of {:pre ...}, which must be the first expr in a fn.
         ((fn [] ~@body))))))

(defmacro ^:no-doc defn+ [name all-args args & body]
  ;; generates a simplified param vector, in order to bind all args also to
  ;; 'all-args', regardless of destructuring.
  `(def ~(vary-meta name assoc
                    :arglists `'(~args))
     (fn+ ~all-args ~args ~@body)))

(defn- maybe-get-precond [body]
  (if-let [expr (-> (not-empty body)
                      (first)
                      (get :pre))]
    [{:pre expr}]))

(defmacro defn-dynamic
  "A macro to define a new abstract dynamic element. For example, given

```
(defn-dynamic greeting state [arg]
  (dom/div (str arg \" \" state)))
```

  You can create a new dynamic element by calling `(greeting \"Hello\")`, which looks exactly like

```
(dom/div (str \"Hello\" \" \" \"world\")
```

  when the current state of the element is `state`, and changes whenever the state changes."
  [name state args & body]
  (let [precond (maybe-get-precond body)]
    `(let [f# (fn [~state ~@args]
                ~@body)]
       (defn+ ~name args# ~args
         ~@precond
         (-> (apply reacld.core/dynamic f# args#)
             (named ~(str *ns* "/" name)))))))

(defmacro def-dynamic
  "A macro to define a new dynamic element. For example, given

```
(def-dynamic current-state state
  (dom/span \"Current state is: \" (pr-str state)))
```

  then `current-state` is an element that shows the current state as it
  changes over time. This is similar to [[defn-dynamic]] but without the
  arguments."
  [name state & body]
  `(let [f# (fn [~state]
              ~@body)]
     (def ~name
       (-> (reacld.core/dynamic f#)
           (named ~(str *ns* "/" name))))))

(defmacro defn-interactive
  "A macro to simplify using plain dom elements as interactive
  elements that define how the user sees and can change the state. For
  example, to use a `dom/input` to let the user edit a plain string
  and a placeholder text, you can define

```
(defn-interactive textbox value set-value [placeholder]
  (dom/input {:type \"text\" :placeholder placeholder
              :value value
              :onchange (fn [ev] (set-value (.. ev -target -value))}))
```
"

  [name state set-state args & body]
  (let [precond (maybe-get-precond body)]
    `(let [f# (fn [~state ~set-state ~@args]
                ~@body)]
       (defn+ ~name args# ~args
         ~@precond
         (-> (apply reacld.core/interactive f# args#)
             (named ~(str *ns* "/" name)))))))

(defmacro def-interactive
  "A macro similar to [[defn-interactive]], for concrete elements without arguments. For example:

```
(def-interactive textbox value set-value
  (dom/input {:type \"text\"
              :value value
              :onchange (fn [ev] (set-value (.. ev -target -value))}))
```
"
  [name state set-state & body]
  `(let [f# (fn [~state ~set-state]
              ~@body)]
     (def ~name
       (-> (reacld.core/interactive f#)
           (named ~(str *ns* "/" name))))))

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
  element in your application. That element will be invisible, but
  will emit a JavaScript `Date` object as an action every second.

Note that `deliver!` must never be called directly in the body of
  `defn-subscription`, but only later, from an *asynchronous context*.
  Also note that the body is evaluated as soon as the subscription
  element is mounted into your application, and that it must result in
  a function with no arguments, which is called when the element is
  removed from the application afterwards.
 "
  [name deliver! args & body]
  ;; TODO: rename; 'external'? 'primitive'?
  (let [precond (maybe-get-precond body)]
    `(let [f# (fn [~deliver! ~@args]
                ~@body)]
       (defn+ ~name args# ~args
         ~@precond
         (-> (apply reacld.core/subscribe f# args#)
             (named ~(str *ns* "/" name)))))))

