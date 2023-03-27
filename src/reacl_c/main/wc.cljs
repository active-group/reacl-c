(ns reacl-c.main.wc
  "Functions to define items as a web component.

  Any function that takes an attribute map as the first argument and
  returns an item, is a simple web component. Methods, properties and
  several other settings can be added to a web component with the
  functions in this namespace. Then, it can be registered in the
  browser under a unique tag name with [[define-wc!]]."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [clojure.set :as set]
            [active.clojure.functions :as f]
            goog
            [active.clojure.lens :as lens])
  (:refer-clojure :exclude [use]))

;; Note: extending existing elements, as well as having custom
;; elements with child markup, is probably not what people want to do
;; here - it's the reacl-c item that does the rendering; One might
;; want to use a different web-component library for that.

(defrecord ^:private WebComponent [item-f initial-state connected disconnected adopted attributes properties methods shadow-init])

(defn- lift [v]
  (assert (or (ifn? v) (instance? WebComponent v)))
  (if (instance? WebComponent v)
    v
    (WebComponent. v nil nil nil nil {} {} {} nil)))

(defn initial-state
  "Sets an initial state for the given web component."
  [wc initial-state]
  (assoc (lift wc) :initial-state initial-state))

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

(let [f (fn [lens state old new]
          (lens/shove state lens new))]
  (defn attribute
    "Adds an attribute declaration to the given web component. Declared
  attributes, and only those, will be included with their current
  value in the first argument of the web component rendering
  function. The given `attr` can be a keyword or a string. The current
  value of the attribute will be in the attribute map of the component
  under the key `key`, which defaults to `attr` if not specified."
    [wc attr & [key]]
    (assert (or (string? attr) (keyword? attr)))
    (update (lift wc) :attributes
            assoc
            (if (keyword? attr) (name attr) attr)
            (if (nil? key) attr key))))

(defn attributes
  "Adds several simple attributes to the given web component, as
  multiple calls to [[attribute]] would."
  [wc & attrs]
  (reduce attribute wc attrs))

(defn ^:no-doc raw-property [wc property descriptor]
  (assert (string? property))
  (update (lift wc) :properties assoc property descriptor))

(defn data-property
  "Adds a data property the the given web component, with the given
  default value. Options can be `:writable`, `:configurable` and
  `:enumerable` according to `js/Object.defineProperty`."
  [wc property value & [options]]
  (raw-property wc property (assoc options :value value)))

(defn accessor-property
  "Adds an accessor property the the given web component, with the given
  getter and optional setter functions. The `get` function is called
  on the current state of the component and must return the current
  property value, and the `set` function is called on the current
  state and the new value, and must return a new state or
  a [[reacl-c.core/return]] value. Options can be `:configurable` and
  `:enumerable` according to `js/Object.defineProperty`."
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
  `:enumerable` according to `js/Object.defineProperty`."
    [wc property & [lens options]]
    (assert (or (string? property) (keyword? property)))
    (assert (or (keyword? property) (some? lens)))
    (let [lens (or lens
                   (if (keyword? property)
                     property
                     (lens/member property)))]
      (accessor-property wc
                         (if (keyword? property) (name property) property)
                         (f/partial get lens)
                         (when-not (:read-only? options)
                           (f/partial set lens))
                         (dissoc options :read-only?)))))

(defn properties
  "Adds several simple properties to the given web component, as
  multiple calls to [[property]] would."
  [wc & properties]
  (reduce property wc properties))

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
  "Return an object to be used with [[dispatch]], contains the
  type and options for a DOM event. Options are `:bubbles`,
  `:cancelable` `:composed` according the the constructor of a
  `js/Event`. If the options contain the key `:detail`, then a
  `js/CustomEvent` is created."
  [type & [options]]
  (assert (string? type) (str "Event type must be a string, got: " (pr-str type)))
  ;; options :bubbles, :cancelable, :composed, :detail
  (Event. type options))

(defn- really-dispatch-event! [target event]
  (assert (some? target) "No target set; dispatch can only be used within a web component.")
  (let [type (:type event)
        options (:options event)
        init (clj->js (dissoc options :detail))
        js-event (if (contains? options :detail)
                   (new js/CustomEvent type (do (aset init "detail" (:detail options))
                                                init))
                   (new js/Event type init))]
    (.dispatchEvent target js-event)))

(c/defn-effect ^:private dispatch* [event target-atom]
  (really-dispatch-event! @target-atom event))

(c/defn-effect ^:private new-atom! []
  (atom nil))

(defn dispatch
  "Returns an action effect that will dispatch the given event when
  executed. Must be used from the item that implements a web component
  only, which is set as the target of the event. See [[event]] to
  define what kind of event is emitted."
  [event]
  (assert (instance? Event event))
  (c/seq-effects (new-atom!)
                 (f/partial dispatch* event)))

(defrecord ^:private HandleEvent [f args])
(defrecord ^:private HandleAccess [f args result])

(c/defn-effect ^:private set-atom! [a value]
  (reset! a value))

(let [dispatch-f (base/effect-f (dispatch* nil nil))]
  (defn- wrap [element attributes item-f]
    (let [attrs (->> attributes
                     (map (fn [[name key]]
                            (when (.hasAttribute element name)
                              [key (.getAttribute element name)])))
                     (remove nil?)
                     (into {}))]
      (as-> (item-f attrs) $
        (c/handle-effect $ (fn [state e]
                             ;; we want the user to just emit an effect
                             ;; (dispatch event), so we have to
                             ;; sneak the element reference into the
                             ;; effect, so that they can still use
                             ;; execute-effect, which wraps it in
                             ;; a composed-effect. ...slightly hacky.
                             (let [repl (fn repl [e]
                                          (if (base/composed-effect? e)
                                            (base/map-composed-effect e repl)
                                            (if (= dispatch-f (base/effect-f e))
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
                          $)))))

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

(defonce ^:private internal-name-suffix (str (random-uuid)))

(defn- internal-name [prefix]
  (str prefix "$" internal-name-suffix))

(let [n (internal-name "reacl_c_app")]
  (def ^:private app-property-name n)
  
  (defn- set-app! [element app]
    (aset element n app))

  (defn- get-app [element]
    (aget element n)))

(let [n (internal-name "reacl_c_definition")]
  (def def-property-name n)
  
  (defn- set-def! [class wc]
    (aset class n wc))

  (defn- get-def [class]
    (aget class n))

  (defn- has-def? [class]
    ;; not if get-def is nil or not, but if that property is defined
    (.hasOwnProperty class n)))

(let [n (internal-name "reacl_c_instances")]
  (def ^:private instances-property-name n)

  (defn- get-instances [class]
    (or (aget class instances-property-name) #{}))

  (defn- update-instances! [class f & args]
    (aset class instances-property-name (apply f (get-instances class) args))))

(defn- eval-event-unmounted [class handler args]
  (let [wc (get-def class)
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
        (set-def! class
                  (assoc wc :initial-state state))))))

(defn- call-handler-wc [class ^js this f & args]
  (let [app (get-app this)]
    ;; an attribute change (not also other events), may occur
    ;; before the connected event (the element is mounted), in
    ;; which case we don't have a running app yet; in that
    ;; case, we change the initial-state for now, and complain
    ;; about actions and messages.
    (if (some? app)
      (main/send-message! app (HandleEvent. f args))
      (eval-event-unmounted class f args))))

(defn- access-wc [class ^js this f & args]
  (let [app (get-app this)]
    ;; if it's called before mount, we have no app yet, and access the initial-state instead.
    (if (some? app)
      ;; as of now, event handling is synchronous - if that
      ;; changes, we hopefully find a lower level access to the
      ;; current state.
      (let [result (atom ::fail)]
        (main/send-message! app (HandleAccess. f args result))
        (assert (not= ::fail @result) "Property access failed. Maybe message handling is not synchronous anymore?")
        @result)
      (let [wc (get-def class)]
        (apply f (:initial-state wc) args)))))

(defn- property-wc [class descriptor]
  ;; Note: for more hot code reload, we might lookup descriptor in the definition, but that would be slower.
  (let [{get :get set :set} descriptor
        js-descriptor (clj->js (dissoc descriptor :get :set :value))]
    (when (some? (:get descriptor))
      (aset js-descriptor "get" (fn []
                                  (this-as this (access-wc class this get)))))
    (when (some? (:set descriptor))
      (aset js-descriptor "set" (fn [value]
                                  (this-as this
                                    (call-handler-wc class this set value)))))
    (when (contains? descriptor :value)
      (aset js-descriptor "value" (:value descriptor)))
    js-descriptor))

(defn- call-method-wc [class ^js this f args]
  ;; Note: for more hot code reload, we might lookup f in the definition, but that would be slower.
  (let [result (atom nil)
        ;; extra first arg: an action to return the result of the method.
        full-args (cons (f/partial set-atom! result) args)
        app (get-app this)]
    (if (some? app)
      ;; as of now, event handling is synchronous
      (do
        (main/send-message! app (HandleEvent. f full-args))
        @result)

      (do (eval-event-unmounted class f full-args)
          @result))))

(defn- method-wc [class f]
  (fn [& args]
    (this-as this (call-method-wc class this f args))))

(defn- prototype-of [tag-or-class]
  (if (string? tag-or-class)
    (js/Object.getPrototypeOf (js/document.createElement tag-or-class))
    (.-prototype tag-or-class)))

(defn- attach-shadow-root [element init]
  (.attachShadow element (clj->js init)))

(defn- render! [this ctor]
  (let [wc (get-def ctor)]
    (assert (some? (:item-f wc)) wc)
    (set-app! this
              (main/run (if-let [init (:shadow-init wc)]
                          (attach-shadow-root this init)
                          this)
                (wrap this (:attributes wc) (:item-f wc))
                {:initial-state (:initial-state wc)}))))

(defn- stop! [this]
  ;; stop react actively; otherwise nodes can be laying around, with react kind of 'running in background' continuously.
  (main/stop! (get-app this)))

(def ^:private hot-update-enabled? goog/DEBUG)

(defn- new-empty-wc []
  ;; Note: absolutely not sure if all this OO/prototype stuff is correct; but it seems to work.
  (let [htmlelement js/HTMLElement 
        super (prototype-of htmlelement)
        ctor (fn ctor []
               ;; = super()
               (let [this (js/Reflect.construct htmlelement (to-array nil) ctor)]
                 (js/Object.defineProperty this app-property-name
                                           #js {:value nil
                                                :writable true
                                                :enumerable false})
                 this))
        
        prototype
        #js {:attributeChangedCallback (fn [attr old new]
                                         ;; we always rerender, where all current attribute values are read.
                                         (this-as ^js this
                                           (render! this ctor)))}]

    (js/Object.setPrototypeOf prototype super)
    (set! (.-prototype ctor) prototype)
    (set! (.-constructor (.-prototype ctor)) ctor)

    ;; statics
    (js/Object.defineProperty ctor def-property-name
                              #js {:value nil
                                   :writable true
                                   :enumerable false})

    ctor))

(defn- set-lifecycle-methods! [class wc]
  (doto (.-prototype class)
    (aset "connectedCallback"
          (let [user (:connected wc)]
            (fn []
              (this-as ^js this
                (when hot-update-enabled? (update-instances! class conj this))
                ;; Note: we cannot render before the 'connected event'.
                ;; Note: a shadow root can apparently be created and filled in the constructor already; but for consistency we do it here.
                (render! this class)
                (when user (call-handler-wc class this user))))))
    (aset "disconnectedCallback"
          (let [user (:disconnected wc)]
            (fn []
              (this-as ^js this
                (stop! this)
                (when hot-update-enabled? (update-instances! class disj this))
                (when user (call-handler-wc class this user))))))
    (aset "adoptedCallback" (if-let [user (:adopted wc)]
                              (fn []
                                (this-as this
                                  (call-handler-wc class this user)))
                              js/undefined))))

(defn- list-diff [l1 l2]
  (let [s1 (set l1)
        s2 (set l2)]
    [(set/difference s1 s2)
     (set/intersection s1 s2)
     (set/difference s2 s1)]))

(defn- map-diff [m1 m2]
  (list-diff (keys m1) (keys m2)))

(defn- methods-update [prev new]
  (let [[removed changed added] (map-diff prev new)]
    (fn [class]
      (let [p (.-prototype class)]
        (doseq [m-name removed]
          (js-delete p m-name))
        (doseq [m-name (concat changed added)]
          (aset p m-name (method-wc class (get new m-name))))))))

(defn- properties-update [prev new]
  (let [[removed changed added] (map-diff prev new)]
    ;; cannot remove properties, nor change whem in general (we could change some aspects...?)
    (when (and (empty? removed)
               (empty? changed))
      (fn [class]
        (js/Object.defineProperties (.-prototype class)
                                    (apply js-obj (mapcat (fn [n]
                                                            [n (property-wc class (get new n))])
                                                          added)))))))

(defn- define-attributes! [class names]
  (js/Object.defineProperty class "observedAttributes"
                            #js {:value (to-array names)}))

(defn- broadcast-rerender! [class]
  (doseq [i (get-instances class)]
    (render! i class)))

(defn- try-update! [class wc]
  ;; 1. connected, disconnected, adopted can always be updated; though
  ;; connected will of course not be called again if an element is
  ;; already used.
  
  ;; 2. item-f, initial-state and changed attributes require a forced
  ;; rerendering.
  
  ;; 3. attributes, properties, and methods have to be diffed

  ;; 4. attribute names cannot change; adding and removing attributes
  ;; would require the browser to reevaluate 'observedAttributes' -
  ;; does it do that? probably not.

  ;; 5. shadow-init cannot change.

  (if (not (has-def? class))
    false ;; a different web component?
    (let [prev (get-def class)]
      ;; prev will be nil initially!
      (if (and (some? prev)
               ;; Not done in production:
               (not hot-update-enabled?))
        false
        (let [mu (methods-update (:method prev) (:methods wc))
              pu (properties-update (:properties prev) (:properties wc))
              same-attrs? (= (keys (:attributes prev)) (keys (:attributes wc)))
              same-shadow? (= (:shadow-init prev) (:shadow-init wc))]
          (if (and mu pu (or (nil? prev) (and same-attrs? same-shadow?)))
            (do (set-def! class wc)
                (set-lifecycle-methods! class wc)
            
                (mu class)
                (pu class)

                (when (nil? prev)
                  (define-attributes! class (keys (:attributes wc))))

                ;; tell all connected instances of this class to rerender if needed;
                ;; Note: not needed initially;
                (when (and (some? prev)
                           (or (not= (:attributes prev) (:attributes wc))
                               (not= (:item-f prev) (:item-f wc))
                               (not= (:initial-state prev) (:initial-state wc))))
                  (broadcast-rerender! class))
                true)
            false))))))

(defn- wc-class [wc]
  (let [class (new-empty-wc)
        r (try-update! class wc)]
    (assert r "Updating a new class must succeed.")
    class))

(defn- get-native-wc
  [n]
  (js/customElements.get n))

(defn define!
  "Tries to register or update the given web component under the
  given name in the browser, returning whether that succeeded."
  [name wc & [options]]
  (let [wc (lift wc)]
    (if-let [class (get-native-wc name)]
      (try-update! class wc)
      (do (js/customElements.define name (wc-class wc))
          true))))

(c/defn-effect ^:private gen-name []
  (name (gensym "reacl-c-web-component")))

(defn- snd [a b] b)

(c/defn-item ^:private define-it [wc]
  (c/with-state-as name
    (if (nil? name)
      (c/execute-effect (gen-name) snd)
      (c/execute-effect (c/effect define! name wc)
                        (fn [st ok?]
                          (if ok?
                            (c/return)
                            ;; set name to nil, will generate a new one and register that.
                            (c/return :state nil)))))))

(let [f (fn [[_ name] args]
          (when name
            (c/focus lens/first (apply dom/h name args))))]
  (c/defn-item use
    "Registers the given web component under a unique name, and
  returns an item using that component. This can be especially useful
  during development of a web component."
    [wc & args]
    (c/local-state nil
                   (c/fragment (c/focus lens/second (define-it wc))
                               (c/dynamic f args)))))
