(ns reacl-c.impl.react
  (:require [reacl-c.impl.react0 :as r0 :include-macros true]
            [reacl-c.impl.utils :as utils]
            [reacl-c.impl.stores :as stores]
            [reacl-c.base :as base]
            [reacl-c.core :as core]
            [reacl-c.dom :as dom]
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

(def ^:private $handle-message "reacl_c__handleMessage")

(defn- send-message! [comp msg & [callback]]
  ;; TODO: callback non-optional -> handle-message
  (assert (some? comp) (pr-str comp));; TODO: exn if not defined.
  (assert (some? (aget comp $handle-message)) (pr-str comp))
  ((aget comp $handle-message) msg))

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
    (do (when-not (satisfies? IReact item) (js/console.log "no implementation of" item))
        (-instantiate-react item binding ref))))

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

(defn- forward-messages [this & [ref]]
  (fn [msg]
    (send-message-react-ref! (or ref (r0/child-ref this)) msg)))

(defn- toplevel-process-messages [messages]
  (doseq [[target msg] messages]
    (send-message-base-ref! target msg)))

(r0/defclass toplevel-controlled this [item state onchange onaction]
  "render" (fn []
             (render item
                     (Binding. state
                               (stores/delegate-store state onchange)
                               (f/partial toplevel-actions onaction)
                               toplevel-process-messages)
                     (r0/child-ref this)))
  
  $handle-message (forward-messages this))

(defrecord ^:private ReactApplication [comp]
  base/Application  
  (-send-message! [this msg callback]
    (send-message! comp msg callback)))

(defn react-send-message!
  [comp msg & [callback]]
  (send-message! comp msg callback))

(defn react-run [item state onchange onaction]
  (r0/elem toplevel-controlled nil [item state onchange onaction]))

(defn run [dom item state onchange onaction]
  (ReactApplication. (r0/render-component (react-run item state onchange onaction)
                                          dom)))

;; items

(r0/defclass dynamic this [binding f & args]
  $handle-message (forward-messages this)
  "render" (fn [] (render (apply f (:state binding) args)
                          binding
                          (r0/child-ref this))))

(extend-type base/Dynamic
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem dynamic ref (list* binding f args))))

(r0/defclass focus this [binding e lens]
  $handle-message (forward-messages this)
  
  "render" (fn [] (render e
                          (assoc binding
                                 :state (lens/yank (:state binding) lens)
                                 :store (stores/focus-store (:store binding) lens))
                          (r0/child-ref this))))

(extend-type base/Focus
  IReact
  (-instantiate-react [{e :e lens :lens} binding ref]
    ;; TODO use id class?
    (r0/elem focus ref [binding e lens])))

(r0/defclass local-state this [binding e initial]
  $handle-message (forward-messages this)

  "getInitialState" (fn [] (make-new-state! this initial))

  "render" (fn [] (render e 
                          (assoc binding
                                 :state [(:state binding) (:state (r0/get-state this))]
                                 :store (stores/conc-store (:store binding)
                                                           (:store (r0/get-state this))))
                          (r0/child-ref this)))
  
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
  (r0/defclass handle-action this [binding e f pred]
    $handle-message (forward-messages this)
    
    "render" (fn [] (render e
                            (assoc binding :action-target (f/partial h binding f pred))
                            (r0/child-ref this)))))

(extend-type base/HandleAction
  IReact
  (-instantiate-react [{e :e f :f pred :pred} binding ref]
    (r0/elem handle-action ref [binding e f pred])))

(let [send! (fn [binding r]
              (call-event-handler! binding (constantly r)))]
  (r0/defclass with-async-return this [binding f & args]
    $handle-message (forward-messages this)
    
    "render" (fn [] (render (apply f (f/partial send! binding) args)
                            binding
                            (r0/child-ref this)))))

(extend-type base/WithAsyncReturn
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-async-return ref (list* binding f args))))

(r0/defclass lifecycle this [binding init finish]
  "render" (fn [] (r0/fragment nil))

  $handle-message (fn [msg] (assert false)) ;; TODO: exn?
  
  ;; OPT: shouldUpdate could ignore the finish fn.
  "componentDidMount" (fn [] (call-event-handler! binding init))
  "componentDidUpdate" (fn [] (call-event-handler! binding init))
  "componentWillUnmount" (fn [] (call-event-handler! binding finish)))

(extend-type base/Lifecycle
  IReact
  (-instantiate-react [{init :init finish :finish} binding ref]
    (r0/elem lifecycle ref [binding init finish])))

(let [upd (fn [binding f old-state new-state]
            (call-event-handler*! old-state (:action-target binding) (:process-messages binding) f new-state))]
  (r0/defclass handle-state-change this [binding e f]
    $handle-message (forward-messages this)
    
    "render" (fn [] (render e
                            (assoc binding
                                   :store (stores/handle-store-updates (:store binding) (f/partial upd binding f)))
                            (r0/child-ref this)))))

(extend-type base/HandleStateChange
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-state-change ref [binding e f])))

(r0/defclass handle-message this [binding e f]
  "render" (fn [] (render e binding nil))
  
  $handle-message (fn [msg] (call-event-handler! binding f msg)))

(extend-type base/HandleMessage
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-message ref [binding e f])))

(defn- gen-named [name]
  (r0/class name this [binding e validate-state!]
            [:state "getDerivedStateFromProps"] (fn [state props]
                                                  (when validate-state!
                                                    (validate-state! (r0/extract-state state)))
                                                  nil)
            $handle-message (forward-messages this)
            "render" (fn [] (render e binding (r0/child-ref this)))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReact
  (-instantiate-react [{e :e name-id :name-id validate-state! :validate-state!} binding ref]
    (r0/elem (named name-id) ref [binding e validate-state!])))

(r0/defclass static this [binding f args]
  "render" (fn [] (render (apply f args)
                          binding
                          (r0/child-ref this)))

  $handle-message (forward-messages this))

(extend-type base/Static
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem static ref [(assoc binding
                                :state nil
                                :store stores/void-store) f args])))

(defn- find-event [events type]
  ;; OPT: not really efficient
  (let [on-e (str "on" type)]
    (second (first (filter (fn [[k v]]
                             (= (str/lower-case (name k))
                                on-e))
                           events)))))

(defn- event-handler [this]
  (fn [ev]
    (let [[binding attrs events children] (r0/extract-args (.-props this))
          f (find-event events (.-type ev))]
      (call-event-handler! binding f ev))))

(defn- event-fns [this events]
  (let [handler (:event-handler (r0/get-state this))]
    (into {}
          (map (fn [[k _]]
                 [k handler])
               events))))

(def dom-class
  (memoize (fn [type]
             (r0/class type this [binding attrs events children]
                       $handle-message (fn [msg] (assert false)) ;; TODO: exn
                       "getInitialState" (fn [] {:event-handler (event-handler this)})
                       "render" (fn []
                                  (apply r0/dom-elem type
                                         (-> attrs
                                             (assoc :ref (r0/child-ref this))
                                             (merge (event-fns this events)))
                                         (map #(render-child % binding) children)))))))

(extend-type dom/Element
  IReact
  (-instantiate-react [{type :type attrs :attrs events :events ref :ref children :children} binding ref]
    ;; TODO: optimize away some classes? (add base/E -might-receive-messages?)
    ;; TODO: what would a :ref in attrs refer to?
    (if (empty? events)
      (apply r0/dom-elem type attrs (map #(render-child % binding) children)) ;; TODO: ref?
      (r0/elem (dom-class type) ref [binding attrs events children]))))

(r0/defclass fragment this [binding children]
  $handle-message (fn [msg] (assert false)) ;; TODO: exn
  "render" (fn [] (apply r0/fragment (map #(render-child % binding)
                                          children))))

(extend-type base/Fragment
  IReact
  (-instantiate-react [{children :children} binding ref]
    ;; TODO: needs the class for messages (only if base/E -might-receive-messages?). TODO: ref
    (apply r0/fragment (map #(render-child % binding)
                              children))
    ;; TODO: get rid of this; no one whould event try to set a ref on a fragment.
    #_(r0/elem fragment ref [binding children])))

(r0/defclass id this [binding e]
  $handle-message (forward-messages this)
  
  "render" (fn [] (render e binding (r0/child-ref this))))

(extend-type base/Keyed
  IReact
  (-instantiate-react [{e :e key :key} binding ref]
    (r0/elem id key ref [binding e key])))

(defrecord RRef [ref]
  base/Ref
  (-deref-ref [_] (.-current ref)))

(r0/defclass with-ref this [binding f args]
  $handle-message (forward-messages this)

  "getInitialState" (fn [] {:ref (RRef. (r0/create-ref))})
  
  "render" (fn [] (render (apply f (:ref (r0/get-state this)) args)
                          binding (r0/child-ref this))))

(extend-type base/WithRef
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-ref ref [binding f args])))

(r0/defclass set-ref this [binding e ^RRef ref]
  $handle-message (forward-messages this (:ref ref))
  
  "render" (fn [] (render e binding (:ref ref))))

(extend-type base/Refer
  IReact
  (-instantiate-react [{e :e ref :ref} binding ref2]
    (r0/elem set-ref ref2 [binding e ref])))

(r0/defclass handle-error this [binding e f]
  
  $handle-message (forward-messages this)
  
  "render" (fn [] (render e binding (r0/child-ref this)))

  ;; [:static "getDerivedStateFromError"] (fn [error])
  
  "componentDidCatch" (fn [error info]
                        (call-event-handler! binding f error)))

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
  (r0/defclass lifted-reacl this [binding class args]
    "render"
    (fn []
      (reacl/react-element class {:args args :app-state (:state binding)
                                  :set-app-state! (f/partial set-app-state! binding)
                                  :handle-action! (f/partial handle-action! binding)
                                  :ref (r0/child-ref this)}))
  
    $handle-message
    (fn [msg]
      ;; TODO: callback?!
      (let [comp (.-current (r0/child-ref this))]
        (assert comp this)
        (reacl/send-message! comp msg)))))

(extend-type base/LiftReacl
  IReact
  (-instantiate-react [{class :class args :args} binding ref]
    (r0/elem lifted-reacl ref [binding class args])))
