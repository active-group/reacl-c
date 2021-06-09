(ns reacl-c.main.wc
  "Functions to define items as a web component."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

;; Note: extending existing elements, as well as having custom
;; elements with child markup, is probably not what people want to do
;; here - it's the reacl-c item that does the rendering; One might
;; want to use a different web-component library for that.

(defrecord ^:private WebComponent [item initial-state connected disconnected adopted observed-attributes attribute-changed properties methods shadow-init])

(defn base
  "Returns a basic web component, that renders the given item with the
  given initial state, which defaults to `nil`. More features can be
  added with [[attribute]], [[property]], [[method]] and more
  functions in this package."
  [item & [initial-state]]
  (WebComponent. item initial-state nil nil nil nil nil {} {} nil))

(defn- lift [item]
  (if (instance? WebComponent item)
    item
    (base item)))

(defn- comp-handlers [p f state & args]
  (let [r1 (c/as-returned (apply p state args))]
    (let [st2 (if (= c/keep-state (c/returned-state r1))
                state
                (c/returned-state r1))]
      (c/merge-returned r1 (apply f st2 args)))))

(defn- conc [obj attr f]
  (update (lift obj) attr (fn [p]
                            (if (some? p)
                              (f/partial comp-handlers p f)
                              f))))

(defn connected
  "Adds a handler for the connected callback to the given web
  component. The given function `f` will be called with the current
  state and must return a new state or a [[reacl-c.core/return]]
  value."
  [wc f]
  (assert (some? f))
  (conc wc :connected f))

(defn disconnected
  "Adds a handler for the disconnected callback to the given web
  component. The given function `f` will be called with the current
  state and must return a new state or a [[reacl-c.core/return]]
  value."
  [wc f]
  (conc wc :disconnected f))

(defn adopted
  "Adds a handler for the adopted callback to the given web
  component. The given function `f` will be called with the current
  state and must return a new state or a [[reacl-c.core/return]]
  value."
  [wc f]
  (conc wc :adopted f))

(let [when-f (fn [attr f state changed-attr old-value new-value]
               (if (= attr changed-attr)
                 (f state old-value new-value)
                 (c/return)))]
  (defn attribute-changed
    "Adds a handler that is called when the given attribute changes on the
  given web component. The given function `f` will be called with the
  current state, the old and the new value of the attribute, and must
  return a new state or a [[reacl-c.core/return]] value. Note that for
  initial value of the attribute, and change with an old value of
  `nil` will be triggered.

  See [[attribute]] for a simple way to include an attribute value
  into the state of a component."
    [wc attr f]
    (assert (string? attr))
    (-> wc
        (conc :attribute-changed (f/partial when-f attr f))
        (update :observed-attributes #(conj (or %1 #{}) %2) attr))))

(let [f (fn [lens state old new]
          (lens/shove state lens new))]
  (defn attribute
    "Adds a custom attribute to the given web component, that is
  automatically reflected in the state of the component. The attribute name
  can be a keyword, which is then also used to reflect the attribute
  value in a map state under that key. Alternatively, a lens can be
  specified to put the attribute value in a different kind of state."
    [wc attr & [state-lens]]
    (assert (or (string? attr) (keyword? attr)))
    (assert (or (keyword? attr) (some? state-lens)))
    ;; Hmm... can/should the attribute change when the state changes? Is that reasonable?
    (let [lens (or state-lens (when (keyword? attr) attr))]
      (attribute-changed wc (if (keyword? attr) (name attr) attr) (f/partial f lens)))))

(defn ^:no-doc raw-property [wc property descriptor]
  (assert (string? property))
  (update (lift wc) :properties assoc property descriptor))

(defn data-property
  "Adds a data property the the given web component, with the given
  default value. Options can be `:writable`, `:configurable` and
  `:enumerable` according to `js/Object.setProperty`."
  [wc property value & [options]]
  (raw-property wc property (assoc options :value value)))

(defn accessor-property
  "Adds an accessor property the the given web component, with the given
  getter and optional setter functions. The `get` function is called
  on the current state of the component and must return the current
  property value, and the `set` function is called on the current
  state and the new value, and must return a new state or
  a [[reacl-c.core/return]] value. Options can be `:configurable` and
  `:enumerable` according to `js/Object.setProperty`."
  [wc property get & [set options]]
  (raw-property wc property (assoc options :get get :set set)))

(let [get (fn [lens state]
            (lens/yank state lens))
      set (fn [lens state value]
            (lens/shove state lens value))]
  (defn property
    "Adds a property to the given web component, that directly reflects a
  value in its current state. The given `property` can be a keyword,
  which is then used to get and associate the property value in a map
  state. Alternatively a lens can be specified for other kinds of
  state values. If the option `:read-only?` is set to true, no setter
  is defined for the property. Other options can be `:configurable` and
  `:enumerable` according to `js/Object.setProperty`."
    [wc property & [lens options]]
    (assert (or (string? property) (keyword? property)))
    (assert (or (keyword? property) (some? lens)))
    (let [lens (or lens
                   (when (keyword? property) property))]
      (accessor-property wc
                         (if (keyword? property) (name property) property)
                         (f/partial get lens)
                         (when-not (:read-only? options)
                           (f/partial set lens))
                         (dissoc options :read-only?)))))

(defn method
  "Adds a custom method to the given web component.

  When the method is called by the user of the web component, `f` will
  be called with the current state of the component, a special
  `return` function, and then any other arguments passed to it. It
  must then return a new state, or a [[reacl-c.core/return]] value. To
  actually return a result to the caller of the method itself, return
  the result of apply the special `return` function as an action. For example:

  ```
  (method wc \"foo\"
    (fn [state return arg-1]
      (c/return :state (update state :called inc)
                :action (return (* arg-1 arg-1)))))
  ```
  
  This would update the state of the component, and return the square
  of the first argument passed to the method invocation.

  See [[accessor-method]] and [[mutator-method]] for simplified versions of this."
  [wc method f]
  (assert (string? method))
  (update (lift wc) :methods assoc method f))

(let [g (fn [f state return & args]
          (c/return :action (return (apply f state args))))]
  (defn accessor-method
    "Add a method to the given component, which returns a value based on
  the current state of the component. When the method is called by the
  user of the component, `f` is called with the current state and any
  additional arguments of the method call, and must return the result
  of method call. The state of the component cannot be changed.
  
  See [[method]] if you need to return both a result and change the
  state of the component."
    [wc method f]
    (method wc method (f/partial g f))))

(let [g (fn [f state return & args]
          (apply f state args))]
  (defn mutator-method
    "Add a method to the given component, which only changes the state of
  the component and always returns `nil`. When the method is called by
  the user of the component, `f` is called with the current state and
  any additional arguments of the method call, and must return a new
  state or a [[reacl-c.core/return]] value.
  
  See [[method]] if you need to return both a result and change the
  state of the component."
    [wc method f]
    (method wc method (f/partial g f))))

(defn shadow
  "Specifies that the given web component should be rendered in a
  'shadow DOM'. The `init` argument must specify the encapsulation
  `:mode` as either \"open\" or \"closed\" and the focus behavious as
  `delegatesFocus`. See `js/HTMLElement.attachShadow` for details. "
  [wc init]
  (assoc (lift wc) :shadow-init init))

(defrecord ^:private Event [type options])

(defn event
  "Return an object to be used with [[dispatch-event!]], contains the
  type and options for a DOM event. Options are `:bubbles`,
  `:cancelable` `:composed` according the the constructor of a
  `js/Event`. If the options contain the key `:detail`, then a
  `js/CustomEvent` is created."
  [type & [options]]
  (assert (string? type))
  ;; options :bubbles, :cancelable, :composed, :detail
  (Event. type options))

#?(:cljs
   (defn- really-dispatch-event! [target event]
     (assert (some? target) "No target set; dispatch-event! can only be used for a web component.")
     (let [type (:type event)
           options (:options event)
           init (clj->js (dissoc options :detail))
           js-event (if (contains? options :detail)
                      (new js/CustomEvent type (do (aset init "detail" (:detail options))
                                                   init))
                      (new js/Event type init))]
       (.dispatchEvent target js-event))))

(c/defn-effect ^:private dispatch-event!* [event target-atom]
  #?(:cljs (really-dispatch-event! @target-atom event)))

(c/defn-effect ^:private new-atom! []
  (atom nil))

(defn dispatch-event!
  "Returns an action effect that will dispatch the given event when
  executed. Must be used from the item that implements a web component
  only, which is set as the target of the event. See [[event]] to
  define what kind of event is emitted."
  [event]
  (assert (instance? Event event))
  (c/seq-effects (new-atom!)
                 (f/partial dispatch-event!* event)))

(defrecord ^:private HandleEvent [f args])
(defrecord ^:private HandleAccess [f args result])

(c/defn-effect ^:private set-atom! [a value]
  (reset! a value))

(let [dispatch-event!-f (base/effect-f (dispatch-event!* nil nil))]
  (defn- wrap [element item]
    (as-> item $
      (c/handle-effect $ (fn [state e]
                           ;; we want the user to just emit an effect
                           ;; (dispatch-event! event), so we have to
                           ;; sneak the element reference into the
                           ;; effect, so that they can still use
                           ;; handle-effect-result, which wraps it in
                           ;; a composed-effect. ...slightly hacky.
                           (let [repl (fn repl [e]
                                        (if (base/composed-effect? e)
                                          (base/map-composed-effect e repl)
                                          (if (= dispatch-event!-f (base/effect-f e))
                                            (let [[event target-atom] (base/effect-args e)]
                                              (c/seq-effects (set-atom! target-atom element) (f/constantly e)))
                                            e)))]
                             (c/return :action (repl e)))))
      (c/handle-message (fn [state msg]
                          (condp instance? msg
                            HandleEvent (apply (:f msg) state (:args msg))
                            HandleAccess (c/return :action (set-atom! (:result msg) (apply (:f msg) state (:args msg))))
                            ;; other messages should be impossible, as noone can refer to the result of this.
                            (do (assert false (str "Unexpected message received in web component: " (pr-str msg)))
                                (c/return))))
                        $))))

(let [set-atom-f (base/effect-f (set-atom! nil nil))]
  (defn- emulate-set-atom! [returned]
    (let [as (base/returned-actions returned)]
      (lens/overhaul returned base/returned-actions
                     (fn [as]
                       (reduce (fn [res a]
                                 (if (and (base/simple-effect? a)
                                          (= set-atom-f (base/effect-f a)))
                                   (do (base/run-effect! a)
                                       res)
                                   (conj res a)))
                               []
                               as))))))

#?(:cljs (defn- eval-event-unmounted [class handler args]
           (let [wc (.-definition class)
                 state (:initial-state wc)
                 ;; we 'emulate' the set-atom effect as an exception;
                 ;; because that is used for methods which should work
                 ;; in the unmounted state too.
                 
                 r (-> (c/as-returned (apply handler state args))
                       (emulate-set-atom!))]
             (assert (empty? (c/returned-actions r)) "Cannot return actions from this web component handler in the unmounted state.")
             (assert (empty? (c/returned-messages r)) "Cannot return messages from this web component handler in the unmounted state.")
             (let [state (c/returned-state r)]
               (when (not= c/keep-state state)
                 (set! (.-definition class)
                       (assoc wc :initial-state state)))))))

#?(:cljs
   (defn- call-handler-wc [class ^js this f & args]
     (let [app (.-app this)]
       ;; an attribute change (not also other events), may occur
       ;; before the connected event (the element is mounted), in
       ;; which case we don't have a running app yet; in that
       ;; case, we change the initial-state for now, and complain
       ;; about actions and messages.
       (if (some? app)
         (main/send-message! app (HandleEvent. f args))
         (eval-event-unmounted class f args)))))

#?(:cljs
   (defn- access-wc [class ^js this f & args]
     (let [app (.-app this)]
       ;; if it's called before mount, we have no app yet, and access the initial-state instead.
       (if (some? app)
         ;; as of now, event handling is synchronous - if that
         ;; changes, we hopefully find a lower level access to the
         ;; current state.
         (let [result (atom ::fail)]
           (main/send-message! app (HandleAccess. f args result))
           (assert (not= ::fail @result) "Property access failed. Maybe message handling is not synchronous anymore?")
           @result)
         (let [wc (.-definition class)]
           (apply f (:initial-state wc) args))))))

#?(:cljs
   (defn ^:no-doc lifecycle-method-wc [class field]
     (fn [& args]
       (this-as ^js this
         (let [f (field (.-definition class))]
           (when f (apply call-handler-wc class this f args)))))))

#?(:cljs
   (defn- property-wc [class [property-name descriptor]]
     ;; Note: for more hot code reload, we might lookup descriptor in the definition, but that would be slower.
     (let [{get :get set :set} descriptor
           js-descriptor (clj->js (dissoc descriptor :get :set :value))]
       (when (some? (:get descriptor))
         (aset js-descriptor "get" (fn []
                                     (this-as this (access-wc class this get)))))
       (when (some? (:set descriptor))
         (aset js-descriptor "set" (fn [value]
                                     (this-as this (call-handler-wc class this set value)))))
       (when (contains? descriptor :value)
         (aset js-descriptor "value" (:value descriptor)))
       [property-name js-descriptor])))

#?(:cljs
   (defn- call-method-wc [class ^js this f args]
     ;; Note: for more hot code reload, we might lookup f in the definition, but that would be slower.
     (let [result (atom nil)
           ;; extra first arg: an action to return the result of the method.
           full-args (cons (f/partial set-atom! result) args)
           app (.-app this)]
       (if (some? app)
         ;; as of now, event handling is synchronous
         (do
           (main/send-message! app (HandleEvent. f full-args))
           @result)

         (do (eval-event-unmounted class f full-args)
             @result)))))

#?(:cljs
   (defn- method-wc [class f]
     (fn [& args]
       (this-as this (call-method-wc class this f args)))))

#?(:cljs
   (defn- prototype-of [tag-or-class]
     (if (string? tag-or-class)
       (js/Object.getPrototypeOf (js/document.createElement tag-or-class))
       (.-prototype tag-or-class))))

#?(:cljs
   (defn- attach-shadow-root [element init]
     (.attachShadow element (clj->js init))))

#?(:cljs
   (defn- new-wc []
     ;; Note: absolutely not sure if all this OO/prototype stuff is correct; but it seems to work.
     (let [super (prototype-of js/HTMLElement) 
           ctor (fn ctor []
                  ;; = super()
                  (let [this (js/Reflect.construct js/HTMLElement (to-array nil) ctor)]
                    this))
           
           prototype
           #js {:connectedCallback (let [user (lifecycle-method-wc ctor :connected)]
                                     (fn []
                                       (this-as ^js this
                                         ;; Note: we cannot render before 'connected event'.
                                         ;; Note: a shadow root can apparently be created and filled in the constructor already; but for consistency we do it here.
                                         (let [wc (.-definition ctor)]
                                           (set! (.-app this)
                                                 (main/run (if-let [init (:shadow-init wc)]
                                                             (attach-shadow-root this init)
                                                             this)
                                                   (wrap this (:item wc)) {:initial-state (:initial-state wc)})))
                                         (.call user this))))
                :disconnectedCallback (lifecycle-method-wc ctor :disconnected)
                :adoptedCallback (lifecycle-method-wc ctor :adopted)
                :attributeChangedCallback (lifecycle-method-wc ctor :attribute-changed)}]
       
       (js/Object.setPrototypeOf prototype super)
       (set! (.-prototype ctor) prototype)

       ;; static methods
       (js/Object.defineProperty ctor "observedAttributes"
                                 ;; Note: might be called by the browser only once; so no hot code reload for this.
                                 #js {:get (fn []
                                             (let [wc (.-definition ctor)]
                                               (let [as (:observed-attributes wc)]
                                                 (to-array as))))})
       ctor)))

#?(:cljs
   (defn- get-native-wc
     [n]
     (js/customElements.get n)))

#?(:cljs
   (defn define-wc!
     "Registers the given web component under the given name in the browser.

  If a web component has beed registered with this name and this
  library before, and unless the option `:no-hot-update?` is set, an attempt
  is made to change the behaviour of the web component at
  runtime. Note that not all aspects of the web component can be
  changed at runtime, and some may require a recreation of such DOM
  elements.
  "
     [name wc & [options]]
     (let [f (if-let [f (and (not (:no-hot-update? options))
                             (get-native-wc name))]
               ;; trying at least some hot updates:
               (do (assert (some? (.-definition f)) "Can only redefine components defined by this library.")
                   ;; Note: this will not update existing elements immediately (only after they update somehow; but we cannot force this here, I think)
                   (set! (.-definition f) (lift wc))
                   f)
               (let [f (new-wc)]
                 (set! (.-definition f) (lift wc))
                 ;; properties cannot be added or remove on a hot update
                 (js/Object.defineProperties (.-prototype f) (apply js-obj (mapcat (partial property-wc f) (:properties wc))))
                 (js/customElements.define name f)
                 f))]
       ;; Note: this will not remove methods on a hot code reload:
       (let [p (.-prototype f)]
         (doseq [[m-name impl] (:methods wc)]
           (aset p m-name (method-wc f impl))))
       f)))

