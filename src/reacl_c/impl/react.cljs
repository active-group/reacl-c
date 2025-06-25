(ns ^:no-doc reacl-c.impl.react
  (:require [reacl-c.impl.react0 :as r0 :include-macros true :refer [let-obj]]
            ["react" :as react]
            [reacl-c.interop.react :as interop]
            [reacl-c.impl.utils :as utils]
            [reacl-c.impl.stores :as stores]
            [reacl-c.base :as base]
            [reacl-c.dom-base :as dom-base]
            [reacl-c.impl.dom0 :as dom0]
            [reacl-c.impl.events :as events]
            [clojure.string :as str]
            [clojure.data :as data]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            goog.object)
  (:refer-clojure :exclude [refer deref]))

;; bindings

(defrecord Binding [state ;; to be used for rendering only.
                    store ;; to be used for state transitions.
                    action-target ;; for action handling, an atom.
                    ])

(def ^:private $handle-message "handleMessage")

;; Note: assume 'elide asserts' => release mode.
(def dev-mode? (try (do (assert (= 1 0)) ((constantly false))) ;; Note: some tricks to prevent 'unreachable code' warning.
                    (catch :default _
                      true)))

(defn- invalid-message-target [target msg]
  (ex-info (str "Invalid target " (pr-str target) " to send message: " (pr-str msg) ".") {:target target :message msg}))

(defn- send-message!
  "Send a message to a React component."
  [comp msg]
  (if (and dev-mode? (or (nil? comp) (nil? (aget comp $handle-message))))
    (throw (invalid-message-target comp msg))
    (.call (aget comp $handle-message) comp msg)))

(defn send-message-to-ref!
  "Send a message to a ref or referred item."
  [target msg]
  (let [comp (base/deref-ref target)]
    (when (and dev-mode? (nil? comp))
      (throw (invalid-message-target target msg)))
    (send-message! comp msg)))

(defn- process-messages [messages]
  (doseq [[target msg] messages]
    (send-message-to-ref! target msg)))

(defn- call-event-handler*!
  "Calls (handler state & args) and returns tuple of [new-state
  callback], where callback must be called after storing the new
  state."
  [action-target handler state & args]
  (let [r (apply handler state args)
        new-state (if (base/returned? r)
                    (let [s (base/returned-state r)]
                      (if (= base/keep-state s) state s))
                    r)
        actions (if (base/returned? r)
                  (not-empty (base/returned-actions r))
                  nil)
        messages (when (base/returned? r)
                   (not-empty (base/returned-messages r)))]
    [new-state (when (or actions messages)
                 (fn after-state-callback []
                   ;; Note: defines the order or processing actions and messages
                   (when actions (@action-target actions))
                   (when messages (process-messages messages))))]))

(defn- event-handler-binding [binding]
  ;; Note: we can (and should) remove state from a binding, if it is
  ;; used for event handling only. Because otherwise the action-target
  ;; will be different even across static items; causing unneccessary
  ;; rerenders. (ideally we should have separated rendering state and
  ;; the other binding info)
  (assoc binding :state nil))

(defn- call-event-handler!
  "Calls (handler state & args) and fully processes the (return) value
  returned from it as a side effect."
  [binding handler & args]
  ;; Note: must not use (:state binding) - see event-handler-binding
  (let [store (:store binding)
        [new-state callback] (apply call-event-handler*! (:action-target binding)
                                    handler (stores/store-get store) args)]
    ;; store state first, then process actions and messages.
    (stores/store-set! store new-state)
    (when callback (callback))))

;; base

(defprotocol ^:private IReact
  (-instantiate-react [this binding ref key] "Returns a React element"))

(defn- void-binding [binding]
  (assoc binding
         :state nil
         :store stores/void-store))

(defn- void-binding? [binding]
  (= (:store binding) stores/void-store))

(defn- resolve-keys [outer-key inner-key]
  ;; outer wins if not nil
  (if (some? outer-key)
    outer-key
    inner-key))

(defn- set-key! [props key]
  (if (some? key)
    (do (aset props "key" key)
        props)
    props))

;; Note: strings, nil and fragments can't have refs. We don't really
;; need them, but using a class can make better error
;; messages. That's done for fragments in dev-mode:
(let [empty (base/really-make-fragment nil)]
  (defn- render-nil [binding ref key]
    (if dev-mode?
      (-instantiate-react empty binding ref key)
      nil)))

(defn- render-string [v binding ref key]
  (if dev-mode?
    (-instantiate-react (base/really-make-fragment (list v))
                        binding ref key)
    v))

(defn- render
  "Render to something that can be the result of a React 'render' method."
  [item binding ref key]
  ;; - ref must be used on the first component that handles messages 'down the line'
  ;; - key must be used on the first component rendered.
  (cond
    (nil? item) (render-nil binding ref key)
    (string? item) (render-string item binding ref key)
    :else
    (do (if (or (not dev-mode?) (satisfies? IReact item))
          ;; Note: is-dynamic? can be a deep recursion, so try to avoid it.
          ;; Note: with this, the binding in Static is already a static binding.
          (let [binding (if (and (not (void-binding? binding))
                                 (not (base/is-dynamic? item)))
                          (void-binding binding)
                          binding)]
            (-instantiate-react item binding ref key))
          (if (base/item? item)
            (throw (ex-info (str "No React implementation of: " (pr-str item)) {:item item}))
            (throw (ex-info (str "Not a valid item: " (pr-str item)) {:value item})))))))

(defn- render-child
  "Render to something that can be in the child array of a React element (dom or fragment)."
  [item binding]
  (cond
    (nil? item) item
    (string? item) item
    :else
    (render item binding nil nil)))

(defn- toplevel-actions [onaction actions]
  (loop [actions actions]
    (when-not (empty? actions)
      (let [action (first actions)]
        (onaction action))
      (recur (rest actions)))))

(defn- message-deadend [elem]
  (fn [this msg]
    (when dev-mode?
      (throw (ex-info (str "Can't send a message to a " elem " item: " (pr-str msg) ".") {:item-type elem :message msg})))))

(defn- merge-refs [r1 r2]
  (if (some? r1)
    (if (some? r2)
      (r0/merge-refs r1 r2)
      r1)
    r2))

(r0/defclass toplevel
  "getInitialState"
  (fn [^js this]
    (let-obj [{state "state" onchange "onchange"} (.-props this)]
      #js {"s_ref" (r0/create-ref)
           "this_store" (stores/make-delegate-store! state onchange)
           "action_target" (atom nil)}))

  "shouldComponentUpdate" (r0/update-on ["state" "item"])

  ;; see [[handle-action]] for the reasons of the action-target atom.
  [:static "getDerivedStateFromProps"]
  (fn [props local-state]
    (let-obj [{state "state" onchange "onchange" onaction "onaction"} props
              {store "this_store" target "action_target"} local-state]
      (stores/reset-delegate-store! store state onchange)
      (reset! target (f/partial toplevel-actions onaction))
      nil))

  "render"
  (fn [^js this]
    (let-obj [{state "state" item "item"} (.-props this)
              {store "this_store" target "action_target" ref "s_ref"} (.-state this)]
      (render item
              (Binding. state store target)
              ref
              nil))))

(defn react-run [item state onchange onaction]
  (cond
    ;; for LiftReact, we don't need a toplevel nor a binding (it cannot have state, or emit actions); same for LiftReactElement
    (or (interop/lift-react? item)
        (interop/lift-react-element? item))
    (render item nil nil nil)

    ;; this would make the action target unstable; not sure if that would be a problem.
    ;; (not (base/is-dynamic? item))
    ;; (render item (Binding. state stores/void-store (atom (f/partial toplevel-actions onaction))) nil nil)
    
    :else
    ;; Note: because we add an extra class, a keyed item can only be effective if the key is moved to the toplevel element
    (let [[key item] (if (base/keyed? item)
                       [(base/keyed-key item) (base/keyed-e item)]
                       [nil item])]
      (r0/elem toplevel (-> #js {"state" state
                                 "item" item
                                 "onchange" onchange
                                 "onaction" onaction}
                            (set-key! key))))))

(defrecord ^:private ReactApplication [handle current-comp message-queue]
  base/Application
  (-stop! [this]
    (r0/unmount-component! handle))
  (-send-message! [this msg]
    (if-let [comp @current-comp]
      (send-message! comp msg)
      (swap! message-queue conj msg))))

(defn run [dom item state onchange onaction]
  ;; Note: that render-component returns the component is legacy in
  ;; React. It actually returns nil when this is used within another
  ;; component (React in React, so to say).  Not sure if this is the
  ;; best solution, but for now we queue messages until a callback ref
  ;; is set (alternatively run would need a callback, which marks the
  ;; point in time when send-messages/setStates are possible)
  ;; Damn it, when running an item from within the rendering of another react element (like using a web component),
  ;; comp may actually return null - https://reactjs.org/docs/react-dom.html#render
  (let [message-queue (atom [])
        current-comp (atom nil)
        callback-ref (fn [current]
                       (reset! current-comp current)
                       (doseq [msg (when (some? current)
                                     (not-empty @message-queue))]
                         (send-message! current msg)))
        react-comp (react-run (base/make-refer item callback-ref) state onchange onaction)]
    (if (instance? ReactApplication dom)
      (let [app dom]
        (r0/rerender-component! react-comp (:handle app))
        app)
      (let [handle (r0/render-component react-comp dom)]
        (ReactApplication. handle current-comp message-queue)))))

(defn flush-sync! [thunk]
  (r0/flush-sync! thunk))

(defn transition! [thunk]
  (r0/transition! thunk))

;; items

(defn- gen-dynamic [name]
  (r0/class name
            "shouldComponentUpdate" (r0/update-on ["f" "c_ref" "binding" "args"])

            "render" (fn [^js this]
                       (let-obj [{binding "binding" f "f" args "args" ref "c_ref"} (.-props this)]
                         (render (apply f (:state binding) args)
                                 binding ref nil)))))

(def dynamical (utils/named-generator gen-dynamic))

(extend-type base/Dynamic
  IReact
  (-instantiate-react [{f :f args :args name-id :name-id} binding ref key]
    (if name-id
      (r0/elem (dynamical name-id)
               (-> #js {"binding" binding
                        "c_ref" ref
                        "f" f
                        "args" args}
                   (set-key! key)))
      (render (apply f (:state binding) args) binding ref key))))

(extend-type base/Focus
  IReact
  (-instantiate-react [{item :e lens :lens} binding ref key]
    (if (void-binding? binding)
      (do (assert (not (base/is-dynamic? item)))
          ;; item isn't looking at the state, and it is already replaced by a void-store.
          ;; So don't yank on it (lens might not work on nil)
          (render item binding
                  ref key))
      
      (render item
              (assoc binding
                     :state (lens/yank (:state binding) lens)
                     :store (stores/focus-store (:store binding) lens))
              ref
              key))))

(defn- make-new-state! [^js this init]
  (let [store (stores/make-resettable-store!
               init
               base/eval-local-state-init
               (fn [new-state]
                 (.setState this #js {"this_state" new-state})))]
    #js {"this_store" store
         "this_state" (stores/store-get store)}))

(defn- new-state-reinit! [get-initial-state]
  (fn [props state]
    (let [init (get-initial-state props)
          store (aget state "this_store")]
      (when (stores/maybe-reset-resettable-store! store init base/eval-local-state-init)
        #js {"this_state" (stores/store-get store)}))))

(r0/defclass local-state
  "getInitialState" (fn [^js this]
                      (make-new-state! this (aget (.-props this) "initial")))

  "shouldComponentUpdate" (r0/update-on ["c_ref" "item" "initial" "binding"] ["this_state"])

  "render" (fn [^js this]
             (let-obj [{binding "binding" ref "c_ref" item "item"} (.-props this)
                       {this-state "this_state" this-store "this_store"} (.-state this)]
               (render item
                       (assoc binding
                              :state [(:state binding) this-state]
                              :store (stores/conc-store (:store binding) this-store))
                       ref
                       nil)))
  
  [:static "getDerivedStateFromProps"] (new-state-reinit! (fn [props] (aget props "initial"))))

(extend-type base/LocalState
  IReact
  (-instantiate-react [{e :e initial :initial} binding ref key]
    (r0/elem local-state (-> #js {"binding" binding
                                  "c_ref" ref
                                  "item" e
                                  "initial" initial}
                             (set-key! key)))))

(let [h (fn [binding f pred actions]
          (let [{handle true pass false} (group-by pred actions)]
            (doseq [action handle]
              (call-event-handler! binding f action))
            (@(:action-target binding) pass)))]
  (r0/defclass handle-action
    ;; Note: children do not have to (and shall not) rerender when the
    ;; action target changes, which is why we put it in a stable atom
    ;; here (and remove the rendering state in the first argument to
    ;; 'h')
    "getInitialState" (fn [this]
                        #js {"this_target" (atom nil)})

    ;; Note: I assume getDerivedStateFromProps gets called event if shouldComponentUpdate returns false,
    ;; which is why we can ignore 'f' and 'pred' here:
    "shouldComponentUpdate" (r0/update-on ["c_ref" "item" "binding"])

    [:static "getDerivedStateFromProps"]
    (fn [props state]
      (let-obj [{binding "binding" f "f" pred "pred"} props
                {atom "this_target"} state]
        (reset! atom (f/partial h (event-handler-binding binding) f pred))
        nil))
    
    "render" (fn [^js this]
               (let-obj [{binding "binding" item "item" ref "c_ref"} (.-props this)
                         {target "this_target"} (.-state this)]
                 (render item
                         (assoc binding :action-target target)
                         ref nil)))))

(extend-type base/HandleAction
  IReact
  (-instantiate-react [{e :e f :f pred :pred} binding ref key]
    ;; Note: by using a class, the item does not have to rerender when 'f' changes (is a lambda)
    (r0/elem handle-action (-> #js {"binding" binding
                                    "c_ref" ref
                                    "item" e
                                    "f" f
                                    "pred" pred}
                               (set-key! key)))))

(let [invoke! (fn [binding handler]
                (call-event-handler! binding handler))]
  (extend-type base/WithAsync
    IReact
    (-instantiate-react [{f :f args :args} binding ref key]
      (render (apply f (f/partial invoke! (event-handler-binding binding)) args)
              binding
              ref
              key))))

(defn- lifecycle-f [name]
  (fn [^js this]
    (let-obj [{f name binding "binding"} (.-props this)]
      (when (some? f)
        (call-event-handler! binding f)))))

(r0/defclass lifecycle
  "render" (fn [this] nil)

  $handle-message (message-deadend "lifecycle")
  
  ;; Note: no update needed when only the finish fn changes
  "shouldComponentUpdate" (r0/update-on ["binding" "init"])

  "componentDidMount" (lifecycle-f "init")
  "componentDidUpdate" (lifecycle-f "init")

  ;; Note: a cleaup can run on 'non-existing' state; i.e. the lens may
  ;; fail; run it on the rendered state instead? Changing state in
  ;; cleanup is dangerous, too; maybe it cannot be disallowed in
  ;; general; although it seems the only meaningful cleanups have to
  ;; be side effects only. So maybe adding a special cleanup for that,
  ;; without access to the state would be better?
  "componentWillUnmount" (lifecycle-f "finish"))

(r0/defclass lifecycle-h
  ;; 'optimized' version of lifecycle without finish.
  "render" (fn [this] nil)

  $handle-message (message-deadend "lifecycle")
  
  "shouldComponentUpdate" (r0/update-on ["binding" "init"])

  "componentDidMount" (lifecycle-f "init")
  "componentDidUpdate" (lifecycle-f "init"))

(extend-type base/Lifecycle
  IReact
  (-instantiate-react [{init :init finish :finish} binding ref key]
    (if (some? finish)
      (r0/elem lifecycle (-> #js {"ref" ref
                                  "binding" binding
                                  "init" init
                                  "finish" finish}
                             (set-key! key)))
      (r0/elem lifecycle-h (-> #js {"ref" ref
                                    "binding" binding
                                    "init" init}
                               (set-key! key))))))

(extend-type base/HandleStateChange
  IReact
  (-instantiate-react [{item :e f :f} binding ref key]
    (render item
            (assoc binding
                   :store (stores/intercept-store (:store binding)
                                                  ;; Note: passes a callback to intercept-store.
                                                  (f/partial call-event-handler*! (:action-target binding) f)))
            ref
            key)))

(r0/defclass handle-message
  "shouldComponentUpdate" (r0/update-on ["item" "binding"])

  "render" (fn [^js this]
             (let-obj [{item "item" binding "binding"} (.-props this)]
               (render item binding nil nil)))
  
  $handle-message (fn [^js this msg]
                    (let-obj [{binding "binding" f "f"} (.-props this)]
                      (call-event-handler! binding f msg))))

(extend-type base/HandleMessage
  IReact
  (-instantiate-react [{e :e f :f} binding ref key]
    (r0/elem handle-message (-> #js {"ref" ref
                                     "binding" binding
                                     "item" e
                                     "f" f}
                                (set-key! key)))))

(defn- gen-named [name]
  (r0/class name
            [:static "getDerivedStateFromProps"]
            (fn [props state]
              (let-obj [{validate-state! "validate" binding "binding"} props]
                (when validate-state!
                  (validate-state! (:state binding))))
              nil)
            
            ;; Dummy state; otherwise React complains that this uses getDerivedStateFromProps
            "getInitialState" (fn [this] #js {})
            
            "shouldComponentUpdate" (r0/update-on ["item" "c_ref" "binding"])

            "render" (fn [^js this]
                       (let-obj [{item "item" binding "binding" ref "c_ref"} (.-props this)]
                         (render item binding ref nil)))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReact
  (-instantiate-react [{e :e name-id :name-id validate-state! :validate-state!} binding ref key]
    (r0/elem (named name-id) (-> #js {"binding" binding
                                      "c_ref" ref
                                      "item" e
                                      "validate" validate-state!}
                                 (set-key! key)))))

(defn- gen-static [name]
  (r0/class name
            "shouldComponentUpdate" (r0/update-on ["c_ref" "f" "args" "binding"])

            "render" (fn [^js this]
                       (let-obj [{f "f" args "args" binding "binding" ref "c_ref"} (.-props this)]
                         (render (apply f args)
                                 binding ref nil)))))

(def statical (utils/named-generator gen-static))

(def static (statical (base/make-name-id "reacl-c/static")))

(extend-type base/Static
  IReact
  (-instantiate-react [{f :f args :args name-id :name-id} binding ref key]
    (r0/elem (if name-id (statical name-id) static)
             (-> #js {"binding" (if (void-binding? binding) binding (void-binding binding))
                      "c_ref" ref
                      "f" f
                      "args" args}
                 (set-key! key)))))

(defn- native-dom [type binding attrs ref children]
  (apply r0/dom-elem type (assoc attrs
                                 :ref ref)
         (map #(render-child % binding) children)))

(defn- rename-key! [m key replacement]
  (assert (not= key replacement))
  (-> m
      (assoc! replacement (get m key))
      (dissoc! key)))

(defn- react-dom-attrs [attrs]
  ;; Note: only needed for non-custom dom elements.
  (when (some? attrs)
    (if (empty? attrs)
      attrs
      (persistent!
       (cond-> (transient attrs)
         ;; TODO: maybe warn if :className or :htmlFor are used? Or even disallow?
         (contains? attrs :class) (rename-key! :class :className)
         (contains? attrs :for) (rename-key! :for :htmlFor))))))


(let [get-derived-state-from-props
      (fn [props state]
        (events/update-bound-event-handlers (aget props "events")
                                            (f/partial call-event-handler! (aget props "binding"))
                                            state))
      should-component-update
      (r0/update-on ["d_ref" "attrs" "events" "contents" "binding"])]

  ;; 'dom-class' uses React functinality; 'custom-dom-class' is even more native, and
  ;; required to use web component custom events in a 'standard' way.
  ;; Note: attributes are not properties; settings properties is not meaningful generally - /maybe/ for data properties (see Object.getOwnPropertyDescriptor)
  
  (def custom-dom-class
    (memoize (fn [type]
               (r0/class (str "reacl-c.custom-dom/" type)
                         $handle-message (message-deadend type)

                         "getInitialState"
                         (fn [^js this]
                           ;; Note: a_ref is a stable fallback ref, in case the user doesn't set
                           ;; one (because we always need one for the events here)
                           (doto #js {"a_ref" (r0/create-ref)}
                             (events/init-state!)))

                         "componentDidMount"
                         (fn [^js this]
                           (let-obj [{a-ref "a_ref"} (.-state this)]
                             (events/update-custom-event-listeners! (r0/current-ref a-ref)
                                                                    nil ;; no previous handlers
                                                                    (events/get-bound-event-handlers (.-state this)))))

                         "componentDidUpdate"
                         (fn [^js this prev-props prev-state]
                           (let-obj [{a-ref "a_ref"} (.-state this)]
                             (let [prev-handlers (events/get-bound-event-handlers prev-state)
                                   new-handlers (events/get-bound-event-handlers (.-state this))
                                   elem (r0/current-ref a-ref)]
                               ;; Note: elem might be the same, or a new one since last render. Can we do better than remove/add all each time?
                               (events/update-custom-event-listeners! elem
                                                                      prev-handlers
                                                                      nil ;; no new; remove all
                                                                      )
                               (events/update-custom-event-listeners! elem
                                                                      nil
                                                                      new-handlers))))

                         [:static "getDerivedStateFromProps"] get-derived-state-from-props
                         
                         "shouldComponentUpdate" should-component-update

                         "render" (fn [^js this]
                                    (let-obj [{binding "binding" attrs "attrs" children "contents" d-ref "d_ref"} (.-props this)
                                              {a-ref "a_ref"} (.-state this)]
                                      (native-dom type
                                                  binding
                                                  attrs ;; Note: no events added here
                                                  (merge-refs d-ref a-ref)
                                                  children)))))))
  
  (def dom-class
    (memoize (fn [type]
               (r0/class (str "reacl-c.dom/" type)
                         $handle-message (message-deadend type)
                         
                         "getInitialState"
                         (fn [^js this]
                           (doto #js {}
                             (events/init-state!)))

                         "shouldComponentUpdate" should-component-update

                         "render" (fn [^js this]
                                    (let-obj [{binding "binding" attrs "attrs" children "contents" d-ref "d_ref"} (.-props this)]
                                      (native-dom type
                                                  binding
                                                  (merge (react-dom-attrs attrs)
                                                         (events/get-bound-event-handlers (.-state this)))
                                                  d-ref
                                                  children)))

                         [:static "getDerivedStateFromProps"]
                         (fn [props state]
                           (events/update-bound-event-handlers (aget props "events")
                                                               (f/partial call-event-handler! (aget props "binding"))
                                                               state)))))))

(extend-type dom-base/Element
  IReact
  (-instantiate-react [{type :type attrs :attrs events :events attr-ref :ref children :children} binding c-ref key]
    ;; Note: attrs-ref, ie `(:ref attrs)` should always deref to the
    ;; dom element (passed as d_ref prop to the wrapper classes) We
    ;; merge that with c-ref (passed down from `refer` items) (sending
    ;; messages to dom elements is futile anyway; so referencing the
    ;; wrapper class is never needed)

    ;; Note: both :key in attrs, and (keyed) should always key the class (outermost element)
    (cond
      ;; Note: for custom elements (web components), React does not do message handling via 'onX' props,
      ;; so events need special code for them:
      (dom0/custom-type? type)
      (r0/elem (custom-dom-class type) (-> #js {"binding" binding
                                                "attrs" (dissoc attrs :key)
                                                "contents" children
                                                "d_ref" (merge-refs attr-ref c-ref)
                                                "events" events}
                                           (set-key! (resolve-keys key (:key attrs)))))

      (or (not (empty? events))
          (and dev-mode? (some? c-ref)))
      (r0/elem (dom-class type) (-> #js {"binding" binding
                                         "attrs" (dissoc attrs :key)
                                         "contents" children
                                         "d_ref" (merge-refs attr-ref c-ref)
                                         "events" events}
                                    (set-key! (resolve-keys key (:key attrs)))))

      ;; no events: no extra wrapper class 
      :else (native-dom type binding
                        ;; (keyed) overrides :key (see resolve-keys)
                        (cond-> (react-dom-attrs attrs)
                          (some? key) (assoc :key key))
                        (merge-refs attr-ref c-ref)
                        children))))

(extend-type base/Fragment
  IReact
  (-instantiate-react [{children :children} binding ref key]
    ;; Note: React fragments can't have a ref. We don't really need them; if ref!=nil here, we could issue a warning, but not more.
    
    ;; render directly into a react fragment - no class needed:
    (apply r0/fragment key (map #(render-child % binding)
                                children))))

(r0/defclass id
  "shouldComponentUpdate" (r0/update-on ["c_ref" "item" "binding"])

  "render" (fn [^js this]
             (let-obj [{item "item" binding "binding" c-ref "c_ref"} (.-props this)]
               (render item binding c-ref nil))))

(extend-type base/Keyed
  IReact
  (-instantiate-react [{item :e inner-key :key} binding ref outer-key]
    (render item binding ref (resolve-keys outer-key inner-key))))


(defn- create-refs [n]
  (doall (repeatedly n base/make-ref)))

(let [g (fn [state f & args]
          (base/make-focus (apply f (second state) args)
                           lens/first))
      c (fn [n]
          (base/make-initializer create-refs (list n)))]
  (extend-type base/WithRefs
    IReact
    (-instantiate-react [{n :n f :f args :args} binding ref key]
      ;; Note: this translation is not possible in core, as long as we
      ;; want to treat the initializer (create-ref) as implementation-specific.
      (render (base/make-local-state (base/make-dynamic nil g (cons f args))
                                     (c n))
              binding ref key))))

(extend-type base/Refer
  IReact
  (-instantiate-react [{e :e ref :ref} binding ref2 key]
    ;; Note: ref is this one; ref2 is one from further outside; i.e. (refer (refer ...))
    (render e binding (merge-refs ref2 ref) key)))

(r0/defclass ^:private on-mount ;; handle-error helper class.
  "render" (fn [this] (r0/fragment nil))
  
  "shouldComponentUpdate" (constantly false)

  "componentDidMount"
  (fn [^js this]
    (let [f (aget (.-props this) "f")]
      (f))))

(let [no-error #js {"error" nil}
      raise (fn [^js this error]
              (let-obj [{binding "binding" f "f"} (.-props this)]
                (.setState this no-error)
                (call-event-handler! binding f error)))]
  (r0/defclass handle-error
    ;; store error in state, then immediately clear it again and call
    ;; handler; handler is reponsible to the reset the problem. Note:
    ;; handle-error matched better with 'componentDidCatch' -
    ;; core/try-catch would be the better primitive when using
    ;; getDerivedStateFromError.
  
    "getInitialState" (fn [this] no-error)
  
    "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item" "f"] ["error"])

    "render" (fn [^js this]
               (let-obj [{item "item" binding "binding" c-ref "c_ref"} (.-props this)
                         {error "error"} (.-state this)]
                 (if (some? error)
                   (r0/elem on-mount
                            #js {"f" (f/partial raise this error)})
                   ;; Note: delaying the rendering of item here, to
                   ;; make sure the error boundary is installed while
                   ;; rendering it (some things may be inlined):
                   (r0/elem id #js {"binding" binding
                                    "c_ref" c-ref
                                    "item" item}))))

    [:static "getDerivedStateFromError"] (fn [error] #js {"error" error})))

(extend-type base/HandleError
  IReact
  (-instantiate-react [{e :e f :f} binding ref key]
    (r0/elem handle-error (-> #js {"binding" binding
                                   "c_ref" ref
                                   "item" e
                                   "f" f}
                              (set-key! key)))))

;; React compat

(extend-type interop/LiftReact
  IReact
  (-instantiate-react [{class :class-or-fn props :props children :children} binding ref key]
    ;; Note: (keyed) overrides key in props - see resolve-keys
    (let [r (r0/merge-refs ref (when props (aget props "ref")))]
      (apply r0/elem class
             (if (some? key)
               (js/Object.assign #js {} props #js {"key" key "ref" r})
               (if (some? r)
                 (js/Object.assign #js {} props #js {"ref" r})
                 props))
             children))))

(extend-type interop/LiftReactElement
  IReact
  (-instantiate-react [it binding ref key]
    (assert (nil? ref) "Cannot refer to a lifted react element. Set they ref prop when constructing the element.")
    (if (some? key) ;; one shouldn't do this either, but if you really have to we can add a wrapper class:
      (r0/elem id #js {"key" key
                       "item" it
                       "c_ref" nil
                       "binding" binding})
      (interop/lift-react-element-element it))))
