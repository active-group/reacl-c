(ns ^:no-doc reacl-c.impl.react
  (:require [reacl-c.impl.react0 :as r0 :include-macros true]
            ["react" :as react]
            [reacl-c.interop.react :as interop]
            [reacl-c.impl.utils :as utils]
            [reacl-c.impl.stores :as stores]
            [reacl-c.base :as base]
            [reacl-c.core :as core]
            [reacl-c.dom-base :as dom-base]
            [reacl-c.impl.dom0 :as dom0]
            [clojure.string :as str]
            [clojure.data :as data]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            goog.object))

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

(defn send-message! [comp msg & [callback]]
  ;; TODO: callback non-optional -> handle-message
  (assert (some? comp) (pr-str comp));; TODO: exn if not defined.
  (assert (some? (aget comp $handle-message)) (pr-str comp))
  (.call (aget comp $handle-message) comp msg))

(defn- send-message-react-ref! [target msg]
  ;; TODO: exn if ref not set.
  (assert (some? (r0/current-ref target)) (str (pr-str target) ", " (pr-str msg)))
  (send-message! (r0/current-ref target) msg))

(defn- send-message-base-ref! [target msg]
  ;; TODO: exn if ref not set.
  (send-message! (base/deref-message-target target) msg))

(defn- process-messages [messages]
  (doseq [[target msg] messages]
    (send-message-base-ref! target msg)))

(defn- call-event-handler*! [state action-target handler & args]
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
                 (fn []
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

(defn- call-event-handler! [binding handler & args]
  ;; Note: must not use :state - see event-handler-binding
  (let [store (:store binding)
        [new-state callback] (apply call-event-handler*! (stores/store-get store)
                                    (:action-target binding) handler args)]
    (stores/store-set! store new-state)
    (when callback (callback))))

;; base

(defprotocol ^:private IReact
  (-instantiate-react [this binding ref] "Returns a React element"))

;; Note: strings, nil and fragments can't have refs. We don't really
;; need them, but using a class can make better error
;; messages. That's done for fragments in dev-mode:
(let [empty (base/really-make-fragment nil)]
  (defn- render-nil [binding ref]
    (if dev-mode?
      (-instantiate-react empty binding ref)
      nil)))

(defn- render-string [v binding ref]
  (if dev-mode?
    (-instantiate-react (base/really-make-fragment (list v))
                        binding ref)
    v))

(defn- render
  "Render to something that can be the result of a React 'render' method."
  [item binding ref]
  (cond
    (nil? item) (render-nil binding ref)
    (string? item) (render-string item binding ref)
    :else
    (do (if-not (satisfies? IReact item)
          (throw (ex-info (str "No implementation of: " item) {:item item}))
          (-instantiate-react item binding ref)))))

(defn- render-child
  "Render to something that can be in the child array of a React element (dom or fragment)."
  [item binding]
  (cond
    (nil? item) item
    (string? item) item
    :else
    (render item binding nil)))

(defn- toplevel-actions [onaction actions]
  (loop [actions actions]
    (when-not (empty? actions)
      (let [action (first actions)]
        (onaction action))
      (recur (rest actions)))))

(defn- message-deadend [elem]
  (fn [this msg]
    ;; TODO: exn?!
    (assert false (str "Can't send a message to a " elem " element: " (pr-str msg) "."))))

(r0/defclass toplevel
  "getInitialState"
  (fn [this]
    (let [props (.-props this)]
      #js {"ref" (r0/create-ref)
           "this_store" (stores/delegate-store (aget props "state") (aget props "onchange"))
           "action_target" (atom nil)}))

  "shouldComponentUpdate" (r0/update-on ["state" "item"])

  ;; see [[handle-action]] for the reasons of the action-target atom.
  [:static "getDerivedStateFromProps"]
  (fn [props local-state]
    (stores/reset-delegate-store! (aget local-state "this_store")
                                  (aget props "state") (aget props "onchange"))
    (reset! (aget local-state "action_target") (f/partial toplevel-actions (aget props "onaction")))
    nil)

  "render"
  (fn [this]
    (let [props (.-props this)
          state (aget props "state")
          local (.-state this)]
      (render (aget props "item")
              (Binding. state
                        (aget local "this_store")
                        (aget local "action_target"))
              (aget local "ref"))))
  
  $handle-message
  (fn [this msg]
    (send-message-react-ref! (aget (.-state this) "ref") msg)))

(defn react-run [item state onchange onaction ref key]
  ;; Note: must have a ref for react-send-message to work.
  (r0/elem toplevel #js {"state" state
                         "item" item
                         "onchange" onchange
                         "onaction" onaction
                         "key" key
                         "ref" (or ref (r0/create-ref))}))

(defrecord ^:private ReactApplication [current-comp message-queue]
  base/Application
  (-send-message! [this msg callback]
    (if-let [comp @current-comp]
      (send-message! comp msg callback)
      (swap! message-queue conj [msg callback]))))

(defn react-send-message!
  "Send a message to the component returned by [[react-run]]."
  [comp msg & [callback]]
  (if (goog.object/containsKey comp "ref")
    ;; if comp is the thing returned from createElement:
    (send-message! (r0/current-ref (.-ref comp)) msg callback)
    ;; if comp is 'this' inside the code of a class:
    (send-message! comp msg callback)))

(defn run [dom item state onchange onaction]
  ;; Note: that render-component returns the component is legacy in
  ;; React. It actually returns nil when this is used within another
  ;; component (React in React, so to say).  Not sure if this is the
  ;; best solution, but for now we queue messages until a callback ref
  ;; is set (alternatively run would need a callback, which marks the
  ;; point in time when send-messages/setStates are possible)
  (let [message-queue (atom [])
        current-comp (atom nil)
        callback-ref (fn [current]
                       (reset! current-comp current)
                       (when-let [msgs (and (some? current)
                                            (not-empty @message-queue))]
                         (doseq [[msg callback] msgs]
                           (send-message! current msg callback))))]
    ;; 
    (when-let [comp (r0/render-component (react-run item state onchange onaction callback-ref nil)
                                         dom)]
      (reset! current-comp comp))
    ;; Damn it, when running an item from within the rendering of another react element (like using a web component),
    ;; comp may actually return null - https://reactjs.org/docs/react-dom.html#render
    (ReactApplication. current-comp message-queue)))

(defrecord RRef [ref]
  base/Ref
  (-deref-ref [_] (.-current ref)))

(defn native-ref [v]
  (if (instance? RRef v)
    (:ref v)
    v))

(defn native-deref [v]
  (if (satisfies? base/Ref v)
    (base/-deref-ref v)
    (.-current v)))

;; items

(defn- gen-dynamic [name]
  (r0/class name
            "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "f" "args"])

            "render" (fn [this]
                       (let [props (.-props this)
                             binding (aget props "binding")]
                         (render (apply (aget props "f") (:state binding) (aget props "args"))
                                 binding (aget props "c_ref"))))))

(def dynamical (utils/named-generator gen-dynamic))

(def dynamic (dynamical (base/make-name-id "reacl-c/dynamic")))

(extend-type base/Dynamic
  IReact
  (-instantiate-react [{f :f args :args name-id :name-id} binding ref]
    (r0/elem (if name-id (dynamical name-id) dynamic)
             #js {"binding" binding
                  "c_ref" ref
                  "f" f
                  "args" args})))

(r0/defclass focus
  "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item" "lens"])

  "render" (fn [this]
             (let [props (.-props this)
                   binding (aget props "binding")
                   lens (aget props "lens")]
               (render (aget props "item")
                       (assoc binding
                              :state (lens/yank (:state binding) lens)
                              :store (stores/focus-store (:store binding) lens))
                       (aget props "c_ref")))))

(extend-type base/Focus
  IReact
  (-instantiate-react [{e :e lens :lens} binding ref]
    (r0/elem focus #js {"binding" binding
                        "c_ref" ref
                        "item" e
                        "lens" lens})))

(defn- make-new-state! [this init]
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
      (when (stores/maybe-reset-store! store init base/eval-local-state-init)
        #js {"this_state" (stores/store-get store)}))))

(r0/defclass local-state
  "getInitialState" (fn [this]
                      (make-new-state! this (aget (.-props this) "initial")))

  "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item" "initial"] ["this_state"])

  "render" (fn [this]
             (let [props (.-props this)
                   binding (aget props "binding")
                   ref (aget props "c_ref")
                   e (aget props "item")]
               (render e 
                       (assoc binding
                              :state [(:state binding) (aget (.-state this) "this_state")]
                              :store (stores/conc-store (:store binding)
                                                        (aget (.-state this) "this_store")))
                       ref)))
  
  [:static "getDerivedStateFromProps"] (new-state-reinit! (fn [props] (aget props "initial"))))

(extend-type base/LocalState
  IReact
  (-instantiate-react [{e :e initial :initial} binding ref]
    (r0/elem local-state #js {"binding" binding
                              "c_ref" ref
                              "item" e
                              "initial" initial})))

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
    "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item"])

    [:static "getDerivedStateFromProps"]
    (fn [props local-state]
      (let [atom (aget local-state "this_target")]
        (reset! atom (f/partial h (event-handler-binding (aget props "binding")) (aget props "f") (aget props "pred")))
        nil))
    
    "render" (fn [this]
               (let [props (.-props this)
                     atom (aget (.-state this) "this_target")]
                 (render (aget props "item")
                         (assoc (aget props "binding") :action-target atom)
                         (aget props "c_ref"))))))

(extend-type base/HandleAction
  IReact
  (-instantiate-react [{e :e f :f pred :pred} binding ref]
    (r0/elem handle-action #js {"binding" binding
                                "c_ref" ref
                                "item" e
                                "f" f
                                "pred" pred})))

(let [send! (fn [binding r]
              (call-event-handler! binding (constantly r)))]
  (r0/defclass with-async-return
    "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "f" "args"])

    "render" (fn [this]
               (let [props (.-props this)
                     binding (aget props "binding")]
                 (render (apply (aget props "f") (f/partial send! (event-handler-binding binding)) (aget props "args"))
                         binding
                         (aget props "c_ref"))))))

(extend-type base/WithAsyncReturn
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-async-return #js {"binding" binding
                                    "c_ref" ref
                                    "f" f
                                    "args" args})))

(defn- lifecycle-f [name]
  (fn [this]
    (let [props (.-props this)
          f (aget props name)]
      (when (some? f)
        (let [binding (aget props "binding")]
          (call-event-handler! binding f))))))

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
  "render" (fn [this] nil)

  $handle-message (message-deadend "lifecycle")
  
  "shouldComponentUpdate" (r0/update-on ["binding" "init"])

  "componentDidMount" (lifecycle-f "init")
  "componentDidUpdate" (lifecycle-f "init"))

(extend-type base/Lifecycle
  IReact
  (-instantiate-react [{init :init finish :finish} binding ref]
    (if (some? finish)
      (r0/elem lifecycle #js {"ref" ref
                              "binding" binding
                              "init" init
                              "finish" finish})
      (r0/elem lifecycle-h #js {"ref" ref
                                "binding" binding
                                "init" init}))))

(let [upd (fn [binding f old-state new-state]
            (call-event-handler*! old-state (:action-target binding) f new-state))]
  (r0/defclass handle-state-change
    "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item" "f"])

    "render" (fn [this]
               (let [props (.-props this)
                     binding (aget props "binding")]
                 (render (aget props "item")
                         (assoc binding
                                :store (stores/intercept-store (:store binding)
                                                               (f/partial upd (event-handler-binding binding) (aget props "f"))))
                         (aget props "c_ref"))))))

(extend-type base/HandleStateChange
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-state-change #js {"binding" binding
                                      "c_ref" ref
                                      "item" e
                                      "f" f})))

(r0/defclass handle-message
  "shouldComponentUpdate" (r0/update-on ["binding" "item"])

  "render" (fn [this]
             (let [props (.-props this)]
               (render (aget props "item") (aget props "binding") nil)))
  
  $handle-message (fn [this msg]
                    (let [props (.-props this)]
                      (call-event-handler! (aget props "binding") (aget props "f") msg))))

(extend-type base/HandleMessage
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-message #js {"ref" ref
                                 "binding" binding
                                 "item" e
                                 "f" f})))

(defn- gen-named [name]
  (r0/class name
            [:static "getDerivedStateFromProps"]
            (fn [props state]
              (let [validate-state! (aget props "validate")]
                (when validate-state!
                  (validate-state! (:state (aget props "binding")))))
              nil)
            
            ;; Dummy state; otherwise React complains that this uses getDerivedStateFromProps
            "getInitialState" (fn [this] #js {})
            
            "shouldComponentUpdate" (r0/update-on ["item" "binding" "c_ref"])

            "render" (fn [this]
                       (let [props (.-props this)]
                         (render (aget props "item") (aget props "binding") (aget props "c_ref"))))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReact
  (-instantiate-react [{e :e name-id :name-id validate-state! :validate-state!} binding ref]
    (r0/elem (named name-id) #js {"binding" binding
                                  "c_ref" ref
                                  "item" e
                                  "validate" validate-state!})))

(defn- gen-static [name]
  (r0/class name
            "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "f" "args"])

            "render" (fn [this]
                       (let [props (.-props this)]
                         (render (apply (aget props "f") (aget props "args"))
                                 (aget props "binding")
                                 (aget props "c_ref"))))))

(def statical (utils/named-generator gen-static))

(def static (statical (base/make-name-id "reacl-c/static")))

(extend-type base/Static
  IReact
  (-instantiate-react [{f :f args :args name-id :name-id} binding ref]
    (r0/elem (if name-id (statical name-id) static)
             #js {"binding" (assoc binding
                                   :state nil
                                   :store stores/void-store)
                  "c_ref" ref
                  "f" f
                  "args" args})))

(defn- native-dom [type binding attrs ref children]
  ;; Note: because we sometimes set a ref directly in the dom attributes (see c/refer), the ref may still be a RRef object here - not very clean :-/
  (apply r0/dom-elem type (assoc attrs :ref (native-ref ref)) (map #(render-child % binding) children)))

(defn- event-handler [current-events call-event-handler! capture?]
  (fn [ev]
    (let [f (dom0/find-event-handler (current-events) capture? ev)]
      (call-event-handler! f ev))))

(defn- event-handlers [current-events call-event-handler!]
  [(event-handler current-events call-event-handler! false)
   (event-handler current-events call-event-handler! true)])

(let [dom-events (fn [this]
                   (aget (.-props this) "events"))
      call! (fn [this f ev]
              (call-event-handler! (aget (.-props this) "binding") f ev))
      event-fns (fn [events handlers]
                  (let [[handler capture-handler] handlers]
                    (into {}
                          (map (fn [[k h]]
                                 [k (when (some? h)
                                      (if (dom0/capture-event? k) capture-handler handler))])
                               events))))
      event-name (memoize (fn [k]
                            (let [s (name k)]
                              (assert (str/starts-with? s "on"))
                              (str/lower-case (subs s 2)))))

      add-event-listeners (fn [elem events]
                            (doseq [[k f] events]
                              (.addEventListener elem (event-name k) f)))
      remove-event-listeners (fn [elem events]
                               (doseq [[k f] events]
                                 (.removeEventListener elem (event-name k) f)))]

  ;; 'dom-class' uses React functinality; 'custom-dom-class' is even more native, and
  ;; required to use web component custom events in a 'standard' way.
  ;; settings properties is not meaningful generally - /maybe/ for data properties (see Object.getOwnPropertyDescriptor)
  
  (def custom-dom-class
    (memoize (fn [type]
               (r0/class (str "reacl-c.custom-dom/" type)
                         $handle-message (message-deadend type)

                         "getInitialState"
                         (fn [this]
                           #js {"a_ref" (RRef. (r0/create-ref))
                                "event_handlers" (event-handlers (f/partial dom-events this)
                                                                 (f/partial call! this))})

                         "componentDidMount"
                         (fn [this]
                           (let [state (.-state this)
                                 props (.-props this)
                                 elem (native-deref (or (aget props "d_ref") (aget state "a_ref")))]
                             (add-event-listeners elem (event-fns (aget props "events") (aget state "event_handlers")))))

                         "componentDidUpdate"
                         (fn [this prev-props prev-state]
                           (let [prev-ref (aget prev-props "d_ref")
                                 prev-events (aget prev-props "events")
                                 
                                 new-state (.-state this)
                                 new-props (.-props this)
                                 new-ref (aget new-props "d_ref")
                                 new-events (aget new-props "events")

                                 prev-elem (native-deref (or prev-ref (aget prev-state "a_ref")))
                                 new-elem (native-deref (or new-ref (aget new-state "a_ref")))]
                             (let [[to-remove to-add]
                                   (let [prev-listeners (event-fns prev-events (aget prev-state "event_handlers"))
                                         new-listeners (event-fns new-events (aget new-state "event_handlers"))]
                                     (if (= prev-elem new-elem)
                                       (let [[to-remove to-add _]
                                             (data/diff prev-listeners new-listeners)]
                                         [to-remove to-add])
                                       [prev-listeners new-listeners]))]
                               (remove-event-listeners prev-elem to-remove)
                               (add-event-listeners new-elem to-add))))

                         ;; Note: I assume removing event listeners is not needed.
                         ;; "componentWillUnmount"
                         ;; (fn [this]
                         ;;   (let [state (.-state this)
                         ;;         ref (aget (.-props this) "d_ref")
                         ;;         events (aget (.-props this) "events")
                         ;;         elem (native-deref (or ref (aget state "a_ref")))]
                         ;;     (remove-event-listeners elem (event-fns events (aget state "event_handlers")))))
                         
                         "shouldComponentUpdate" (r0/update-on ["binding" "d_ref" "attrs" "contents" "events"])

                         "render" (fn [this]
                                    (let [props (.-props this)
                                          binding (aget props "binding")
                                          attrs (aget props "attrs")
                                          children (aget props "contents")]
                                      ;; we render
                                      (native-dom type
                                                  binding
                                                  attrs ;; Note: no events added here
                                                  (or (aget props "d_ref") (aget (.-state this) "a_ref"))
                                                  children)))))))
  
  (def dom-class
    (memoize (fn [type]
               (r0/class (str "reacl-c.dom/" type)
                         $handle-message (message-deadend type)
                         
                         "getInitialState"
                         (fn [this]
                           #js {"event_handlers" (event-handlers (f/partial dom-events this)
                                                                 (f/partial call! this))})
                         
                         "shouldComponentUpdate" (r0/update-on ["binding" "d_ref" "attrs" "contents" "events"])

                         "render" (fn [this]
                                    (let [props (.-props this)
                                          binding (aget props "binding")
                                          attrs (aget props "attrs")
                                          children (aget props "contents")
                                          events (aget props "events")]
                                      (native-dom type
                                                  binding
                                                  (-> attrs
                                                      (merge (event-fns events (aget (.-state this) "event_handlers"))))
                                                  (aget props "d_ref")
                                                  children))))))))

(extend-type dom-base/Element
  IReact
  (-instantiate-react [{type :type custom? :custom? attrs :attrs events :events ref :ref children :children} binding c-ref]
    ;; Note: ref is for refering to the dom element itself (to access the native node); c-ref is for message passing.
    ;; As one cannot send messages to dom elements, the only purpose is to make a better error messages; do that only in dev-mode:
    (cond
      ;; Note: for custom elements (web components), React does not do message handling via 'onX' props,
      ;; so events need special code for them:
      custom? (r0/elem (custom-dom-class type) #js {"ref" c-ref
                                                    "binding" binding
                                                    "attrs" attrs
                                                    "contents" children
                                                    "d_ref" ref
                                                    "events" events})

      (or (not (empty? events))
          (and dev-mode? (some? c-ref)))
      (r0/elem (dom-class type) #js {"ref" c-ref
                                     "binding" binding
                                     "attrs" attrs
                                     "contents" children
                                     "d_ref" ref
                                     "events" events})

      :else (native-dom type binding attrs ref children))))

(r0/defclass fragment
  $handle-message (message-deadend "fragment")

  "shouldComponentUpdate" (r0/update-on ["binding" "contents"])

  "render" (fn [this]
             (let [props (.-props this)
                   binding (aget props "binding")
                   children (aget props "contents")]
               (apply r0/fragment (map #(render-child % binding)
                                       children)))))

(extend-type base/Fragment
  IReact
  (-instantiate-react [{children :children} binding ref]
    ;; Note: React fragments can't have a ref. We don't really need them, except to make better error messages - do that only in dev-mode:
    (if (and dev-mode? (some? ref))
      (r0/elem fragment #js {"ref" ref
                             "binding" binding
                             "contents" children})
      ;; render directly into a react fragment - no class needed:
      (apply r0/fragment (map #(render-child % binding)
                              children)))))

(r0/defclass id
  "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item"])

  "render" (fn [this]
             (let [props (.-props this)]
               (render (aget props "item") (aget props "binding") (aget props "c_ref")))))

(extend-type base/Keyed
  IReact
  (-instantiate-react [{e :e key :key} binding ref]
    (r0/elem id #js {"key" key
                     "binding" binding
                     "c_ref" ref
                     "item" e})))

(r0/defclass with-ref
  "getInitialState" (fn [this]
                      #js {"ref" (RRef. (r0/create-ref))})
  
  "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "f" "args"])

  "render" (fn [this]
             (let [props (.-props this)]
               (render (apply (aget props "f") (aget (.-state this) "ref") (aget props "args"))
                       (aget props "binding") (aget props "c_ref")))))

(extend-type base/WithRef
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-ref #js {"binding" binding
                           "c_ref" ref
                           "f" f
                           "args" args})))

(r0/defclass set-ref
  $handle-message (fn [this msg]
                    (let [^RRef ref (aget (.-props this) "c_ref")]
                      (send-message-react-ref! (:ref ref) msg)))
  
  "shouldComponentUpdate" (r0/update-on ["binding" "item" "c_ref"])

  "render" (fn [this]
             (let [props (.-props this)
                   binding (aget props "binding")
                   e (aget props "item")
                   ^RRef ref (aget props "c_ref")]
               (render e binding (:ref ref)))))

(extend-type base/Refer
  IReact
  (-instantiate-react [{e :e ref :ref} binding ref2]
    (r0/elem set-ref #js {"ref" ref2
                          "binding" binding
                          "item" e
                          "c_ref" ref})))

(r0/defclass ^:private on-mount
  "render" (fn [this] (r0/fragment))
  
  "shouldComponentUpdate" (constantly false)

  "componentDidMount"
  (fn [this]
    (let [f (aget (.-props this) "f")]
      (f))))

(let [no-error #js {"error" nil}
      raise (fn [this error]
              (let [props (.-props this)]
                (.setState this no-error)
                (call-event-handler! (aget props "binding") (aget props "f") error)))]
  (r0/defclass handle-error
    ;; store error in state, then immediately clear it again and call
    ;; handler; handler is reponsible to the reset the problem. Note:
    ;; handle-error matched better with 'componentDidCatch' -
    ;; core/try-catch would be the better primitive when using
    ;; getDerivedStateFromError.
  
    "getInitialState" (fn [this] no-error)
  
    "shouldComponentUpdate" (r0/update-on ["binding" "c_ref" "item" "f"] ["error"])

    "render" (fn [this]
               (let [props (.-props this)]
                 (let [error (aget (.-state this) "error")]
                   (if (some? error)
                     (r0/elem on-mount
                              #js {"f" (f/partial raise this error)})
                     (render (aget props "item") (aget props "binding") (aget props "c_ref"))))))

    [:static "getDerivedStateFromError"] (fn [error] #js {"error" error})))

(extend-type base/HandleError
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-error #js {"binding" binding
                               "c_ref" ref
                               "item" e
                               "f" f})))


;; React compat

(extend-type interop/LiftReact
  IReact
  (-instantiate-react [{class :class props :props} binding ref]
    ;; FIXME: ref + a ref in props? How does that relate?
    (react/createElement class props)))
