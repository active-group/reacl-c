(ns reacl-c.impl.react
  (:require [reacl-c.impl.react0 :as r0 :include-macros true]
            [reacl-c.impl.utils :as utils]
            [reacl-c.impl.stores :as stores]
            [reacl-c.base :as base]
            [reacl-c.core :as core]
            [reacl-c.dom :as dom]
            [reacl-c.impl.dom0 :as dom0]
            [reacl2.core :as reacl :include-macros true]
            [clojure.string :as str]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

;; bindings

(defrecord Binding [state ;; to be used for rendering only.
                    store ;; to be used for state transitions.
                    action-target ;; for action handling
                    process-messages ;; for message delivery
                    ])

(def ^:private $handle-message "handleMessage")

(defn- send-message! [comp msg & [callback]]
  ;; TODO: callback non-optional -> handle-message
  (assert (some? comp) (pr-str comp));; TODO: exn if not defined.
  (assert (some? (aget comp $handle-message)) (pr-str comp))
  (.call (aget comp $handle-message) comp msg))

(defn- send-message-react-ref! [target msg]
  ;; TODO: exn if ref not set.
  (assert (some? (.-current target)) (str (pr-str target) ", " (pr-str msg)))
  (send-message! (.-current target) msg))

(defn- send-message-base-ref! [target msg]
  ;; TODO: exn if ref not set.
  (send-message! (base/deref-message-target target) msg))

(defn- call-event-handler*! [state action-target process-messages handler & args]
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
                   (when actions (action-target actions))
                   (when messages (process-messages messages))))]))

(defn- call-event-handler! [binding handler & args]
  (let [store (:store binding)
        [new-state callback] (apply call-event-handler*! (stores/store-get store)
                                    (:action-target binding) (:process-messages binding) handler args)]
    (stores/store-set! store new-state)
    (when callback (callback))))

;; base

(defprotocol ^:private IReact
  (-instantiate-react [this binding ref] "Returns a React element"))

(defn- render [item binding ref]
  (cond
    ;; TODO: fragments can't have refs... what does that mean for us? Maybe we should throw, because react ignores is silently...
    (string? item) (r0/fragment item)
    (nil? item) (r0/fragment)
    :else
    (do (if-not (satisfies? IReact item)
          (throw (ex-info (str "No implementation of: " item) {:item item}))
          (-instantiate-react item binding ref)))))

(defn- render-child [item binding]
  (cond
    (string? item) item
    :else
    (render item binding nil)))

(defn- toplevel-actions [onaction actions]
  (loop [actions actions]
    (when-not (empty? actions)
      (let [action (first actions)]
        (onaction action))
      (recur (rest actions)))))

(defn- make-new-state! [this init]
  {:store (stores/make-resettable-store!
           init (fn [new-state]
                  (r0/set-state this (fn [s] (assoc s :state new-state)))))
   :state init})

(defn- new-state-reinit! [args-initial-state]
  (fn [props state]
    (let [initial-state (args-initial-state (r0/extract-args props))
          state (r0/extract-state state)
          store (:store state)]
      (when (stores/maybe-reset-store! store initial-state)
        (r0/mk-state (assoc state :state initial-state))))))

(def ^:private message-forward
  (fn [this msg]
    (send-message-react-ref! (r0/child-ref this) msg)))

(defn- message-deadend [elem]
  (fn [this msg]
    ;; TODO: exn?
    (assert false (str "Can't send message to a " elem " element: " (pr-str msg) "."))))

(defn- toplevel-process-messages [messages]
  (doseq [[target msg] messages]
    (send-message-base-ref! target msg)))

(r0/defclass toplevel
  "render" (fn [this]
             (let [[item state onchange onaction] (r0/get-args this)]
               (render item
                       (Binding. state
                                 (stores/delegate-store state onchange)
                                 (f/partial toplevel-actions onaction)
                                 toplevel-process-messages)
                       (r0/child-ref this))))
  
  $handle-message message-forward)

(defrecord ^:private ReactApplication [comp]
  base/Application  
  (-send-message! [this msg callback]
    (send-message! comp msg callback)))

(defn react-send-message!
  [comp msg & [callback]]
  (send-message! comp msg callback))

(defn react-run [item state onchange onaction]
  (r0/elem toplevel nil [item state onchange onaction]))

(defn run [dom item state onchange onaction]
  (ReactApplication. (r0/render-component (react-run item state onchange onaction)
                                          dom)))

;; items

(r0/defclass dynamic
  $handle-message message-forward
  "render" (fn [this]
             (let [[binding f & args] (r0/get-args this)]
               (render (apply f (:state binding) args)
                       binding
                       (r0/child-ref this)))))

(extend-type base/Dynamic
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem dynamic ref (list* binding f args))))

(r0/defclass focus
  $handle-message message-forward
  
  "render" (fn [this]
             (let [[binding e lens] (r0/get-args this)]
               (render e
                       (assoc binding
                              :state (lens/yank (:state binding) lens)
                              :store (stores/focus-store (:store binding) lens))
                       (r0/child-ref this)))))

(extend-type base/Focus
  IReact
  (-instantiate-react [{e :e lens :lens} binding ref]
    ;; TODO use id class?
    (r0/elem focus ref [binding e lens])))

(r0/defclass local-state
  $handle-message message-forward

  "getInitialState" (fn [this]
                      (let [[_ _ initial] (r0/get-args this)]
                        (make-new-state! this initial)))

  "render" (fn [this]
             (let [[binding e _] (r0/get-args this)]
               (render e 
                       (assoc binding
                              :state [(:state binding) (:state (r0/get-state this))]
                              :store (stores/conc-store (:store binding)
                                                        (:store (r0/get-state this))))
                       (r0/child-ref this))))
  
  [:static "getDerivedStateFromProps"] (new-state-reinit! #(nth % 2)))

(extend-type base/LocalState
  IReact
  (-instantiate-react [{e :e initial :initial} binding ref]
    (r0/elem local-state ref [binding e initial])))

(let [h (fn [binding f pred actions]
          (let [{handle true pass false} (group-by pred actions)]
            (doseq [action handle]
              (call-event-handler! binding f action))
            ((:action-target binding) pass)))]
  (r0/defclass handle-action
    $handle-message message-forward
    
    "render" (fn [this]
               (let [[binding e f pred] (r0/get-args this)]
                 (render e
                         (assoc binding :action-target (f/partial h binding f pred))
                         (r0/child-ref this))))))

(extend-type base/HandleAction
  IReact
  (-instantiate-react [{e :e f :f pred :pred} binding ref]
    (r0/elem handle-action ref [binding e f pred])))

(let [send! (fn [binding r]
              (call-event-handler! binding (constantly r)))]
  (r0/defclass with-async-return
    $handle-message message-forward
    
    "render" (fn [this]
               (let [[binding f & args] (r0/get-args this)]
                 (render (apply f (f/partial send! binding) args)
                         binding
                         (r0/child-ref this))))))

(extend-type base/WithAsyncReturn
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-async-return ref (list* binding f args))))

(r0/defclass lifecycle
  "render" (fn [this] (r0/fragment nil))

  $handle-message (message-deadend "lifecycle")
  
  ;; OPT: shouldUpdate could ignore the finish fn.
  "componentDidMount" (fn [this]
                        (let [[binding init finish] (r0/get-args this)]
                          (call-event-handler! binding init)))
  "componentDidUpdate" (fn [this]
                         (let [[binding init finish] (r0/get-args this)]
                           (call-event-handler! binding init)))
  "componentWillUnmount" (fn [this]
                           (let [[binding init finish] (r0/get-args this)]
                             (call-event-handler! binding finish))))

(extend-type base/Lifecycle
  IReact
  (-instantiate-react [{init :init finish :finish} binding ref]
    (r0/elem lifecycle ref [binding init finish])))

(let [upd (fn [binding f old-state new-state]
            (call-event-handler*! old-state (:action-target binding) (:process-messages binding) f new-state))]
  (r0/defclass handle-state-change
    $handle-message message-forward
    
    "render" (fn [this]
               (let [[binding e f] (r0/get-args this)]
                 (render e
                         (assoc binding
                                :store (stores/handle-store-updates (:store binding) (f/partial upd binding f)))
                         (r0/child-ref this))))))

(extend-type base/HandleStateChange
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-state-change ref [binding e f])))

(r0/defclass handle-message
  "render" (fn [this]
             (let [[binding e f] (r0/get-args this)]
               (render e binding nil)))
  
  $handle-message (fn [this msg]
                    (let [[binding e f] (r0/get-args this)]
                      (call-event-handler! binding f msg))))

(extend-type base/HandleMessage
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-message ref [binding e f])))

(defn- gen-named [name]
  (r0/class name
            [:state "getDerivedStateFromProps"] (fn [state props]
                                                  (let [[_ _ validate-state!] (r0/extract-args props)]
                                                    (when validate-state!
                                                      (validate-state! (r0/extract-state state))))
                                                  nil)
            $handle-message message-forward
            "render" (fn [this]
                       (let [[binding e _] (r0/get-args this)]
                         (render e binding (r0/child-ref this))))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReact
  (-instantiate-react [{e :e name-id :name-id validate-state! :validate-state!} binding ref]
    (r0/elem (named name-id) ref [binding e validate-state!])))

(r0/defclass static
  "render" (fn [this]
             (let [[binding f args] (r0/get-args this)]
               (render (apply f args)
                       binding
                       (r0/child-ref this))))

  $handle-message message-forward)

(extend-type base/Static
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem static ref [(assoc binding
                                :state nil
                                :store stores/void-store) f args])))

(defn- native-dom [type binding attrs ref children]
  (apply r0/dom-elem type (assoc attrs :ref ref) (map #(render-child % binding) children)))

(defn- event-handler [current-events call-event-handler! capture?]
  (fn [ev]
    (let [f (dom0/find-event-handler (current-events) capture? ev)]
      (call-event-handler! f ev))))

(defn- event-handlers [current-events call-event-handler!]
  [(event-handler current-events call-event-handler! false)
   (event-handler current-events call-event-handler! true)])

(def dom-class
  (let [dom-events (fn [this]
                     (let [[binding attrs ref events children] (r0/extract-args (.-props this))]
                       events))
        call! (fn [this f ev]
                (let [[binding attrs ref events children] (r0/extract-args (.-props this))]
                  (call-event-handler! binding f ev)))
        event-fns (fn [this events]
                    (let [[handler capture-handler] (:event-handlers (r0/get-state this))]
                      (into {}
                            (map (fn [[k _]]
                                   [k (if (dom0/capture-event? k) capture-handler handler)])
                                 events))))]
    (memoize (fn [type]
               (r0/class (str "reacl-c.dom/" type)
                         $handle-message (message-deadend type)
                         "getInitialState" (fn [this] {:event-handlers (event-handlers (f/partial dom-events this)
                                                                                       (f/partial call! this))})
                         "render" (fn [this]
                                    (let [[binding attrs ref events children] (r0/get-args this)]
                                      (native-dom type
                                                  binding
                                                  (-> attrs
                                                      (merge (event-fns this events)))
                                                  ref
                                                  children))))))))

(extend-type dom/Element
  IReact
  (-instantiate-react [{type :type attrs :attrs events :events ref :ref children :children} binding c-ref]
    ;; TODO: optimize away some classes? (add base/E -might-receive-messages?)
    (if (empty? events)
      (native-dom type binding attrs ref children) ;; FIXME: c-ref?
      (r0/elem (dom-class type) c-ref [binding attrs ref events children]))))

(r0/defclass fragment
  $handle-message (message-deadend "fragment")
  "render" (fn [this]
             (let [[binding children] (r0/get-args this)]
               (apply r0/fragment (map #(render-child % binding)
                                       children)))))

(extend-type base/Fragment
  IReact
  (-instantiate-react [{children :children} binding ref]
    ;; TODO: needs the class for messages (only if base/E -might-receive-messages?). TODO: ref
    (apply r0/fragment (map #(render-child % binding)
                              children))
    ;; TODO: get rid of this; no one whould event try to set a ref on a fragment.
    #_(r0/elem fragment ref [binding children])))

(r0/defclass id
  $handle-message message-forward
  
  "render" (fn [this]
             (let [[binding e] (r0/get-args this)]
               (render e binding (r0/child-ref this)))))

(extend-type base/Keyed
  IReact
  (-instantiate-react [{e :e key :key} binding ref]
    (r0/elem id key ref [binding e key])))

(defrecord RRef [ref]
  base/Ref
  (-deref-ref [_] (.-current ref)))

(r0/defclass with-ref
  $handle-message message-forward

  "getInitialState" (fn [this] {:ref (RRef. (r0/create-ref))})
  
  "render" (fn [this]
             (let [[binding f args] (r0/get-args this)]
               (render (apply f (:ref (r0/get-state this)) args)
                       binding (r0/child-ref this)))))

(extend-type base/WithRef
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-ref ref [binding f args])))

(r0/defclass set-ref
  $handle-message (fn [this msg]
                    (let [[_ _ ^RRef ref] (r0/get-args this)]
                      (send-message-react-ref! (:ref ref) msg)))
  
  "render" (fn [this]
             (let [[binding e ^RRef ref] (r0/get-args this)]
               (render e binding (:ref ref)))))

(extend-type base/Refer
  IReact
  (-instantiate-react [{e :e ref :ref} binding ref2]
    (r0/elem set-ref ref2 [binding e ref])))

(r0/defclass handle-error
  
  $handle-message message-forward
  
  "render" (fn [this]
             (let [[binding e f] (r0/get-args this)]
               (render e binding (r0/child-ref this))))

  ;; [:static "getDerivedStateFromError"] (fn [error])
  
  "componentDidCatch" (fn [this error info]
                        (let [[binding e f] (r0/get-args this)]
                          (call-event-handler! binding f error))))

(extend-type base/HandleError
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-error ref [binding e f])))


;; Reacl compat

(let [set-app-state!
      (fn [binding state callback]
        (when-not (reacl/keep-state? state)
          (call-event-handler! binding (fn [_]
                                         (core/return :state state))))
        (when callback (callback)))
      handle-action!
      (fn [binding action]
        (call-event-handler! binding (fn [state]
                                       (base/make-returned base/keep-state [action] nil))))]
  (r0/defclass lifted-reacl
    "render"
    (fn [this]
      (let [[binding class args] (r0/get-args this)]
        (reacl/react-element class {:args args :app-state (:state binding)
                                    :set-app-state! (f/partial set-app-state! binding)
                                    :handle-action! (f/partial handle-action! binding)
                                    :ref (r0/child-ref this)})))
  
    $handle-message
    (fn [this msg]
      ;; TODO: callback?!
      (let [comp (.-current (r0/child-ref this))]
        (assert comp this)
        (reacl/send-message! comp msg)))))

(extend-type base/LiftReacl
  IReact
  (-instantiate-react [{class :class args :args} binding ref]
    (r0/elem lifted-reacl ref [binding class args])))
