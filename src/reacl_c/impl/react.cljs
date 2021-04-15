(ns ^:no-doc reacl-c.impl.react
  (:require [reacl-c.impl.react0 :as r0 :include-macros true]
            ["react" :as react]
            [reacl-c.interop.react :as interop]
            [reacl-c.impl.utils :as utils]
            [reacl-c.impl.stores :as stores]
            [reacl-c.base :as base]
            [reacl-c.core :as core]
            [reacl-c.dom :as dom]
            [reacl-c.impl.dom0 :as dom0]
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

;; Note: assume 'elide asserts' => release mode.
(def dev-mode? (try (do (assert false) false)
                    (catch :default e
                      true)))

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

#_(defn- can-message-ref? [v]
  (not (or (nil? v)
           (string? v)
           (base/fragment? v)
           (dom/element? v))))


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

(defn- toplevel-process-messages [messages]
  (doseq [[target msg] messages]
    (send-message-base-ref! target msg)))

(r0/defclass toplevel
  "getInitialState"
  (fn [this]
    {:ref (r0/create-ref)})
  "render"
  (fn [this]
    (let [[item state onchange onaction] (r0/get-args this)]
      (render item
              (Binding. state
                        (stores/delegate-store state onchange)
                        (f/partial toplevel-actions onaction)
                        toplevel-process-messages)
              (:ref (r0/get-state this)))))
  
  $handle-message
  (fn [this msg]
    (send-message-react-ref! (:ref (r0/get-state this)) msg)))

(defrecord ^:private ReactApplication [comp]
  base/Application  
  (-send-message! [this msg callback]
    (send-message! comp msg callback)))

(defn react-send-message!
  [comp msg & [callback]]
  (send-message! comp msg callback))

(defn react-run [item state onchange onaction ref key]
  (r0/elem toplevel key ref [item state onchange onaction]))

(defn run [dom item state onchange onaction]
  (ReactApplication. (r0/render-component (react-run item state onchange onaction nil nil)
                                          dom)))

;; items

(r0/defclass dynamic
  "render" (fn [this]
             (let [[binding ref f & args] (r0/get-args this)]
               (render (apply f (:state binding) args)
                       binding ref))))

(extend-type base/Dynamic
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem dynamic nil (list* binding ref f args))))

(r0/defclass focus
  "render" (fn [this]
             (let [[binding ref e lens] (r0/get-args this)]
               (render e
                       (assoc binding
                              :state (lens/yank (:state binding) lens)
                              :store (stores/focus-store (:store binding) lens))
                       ref))))

(extend-type base/Focus
  IReact
  (-instantiate-react [{e :e lens :lens} binding ref]
    (r0/elem focus nil [binding ref e lens])))

(defn- eval-local-state-init [init]
  init)

(defn- make-new-state! [this init]
  (let [store (stores/make-resettable-store!
               init
               eval-local-state-init
               (fn [new-state]
                 (r0/set-state this (fn [s] (assoc s :state new-state)))))]
    {:store store
     :state (stores/store-get store)}))

(defn- new-state-reinit! [args-initial-state]
  (fn [props state]
    (let [init (args-initial-state (r0/extract-args props))
          state (r0/extract-state state)
          store (:store state)]
      (when (stores/maybe-reset-store! store init eval-local-state-init)
        (r0/mk-state (assoc state :state (stores/store-get store)))))))

(r0/defclass local-state
  "getInitialState" (fn [this]
                      (let [[_ _ _ initial] (r0/get-args this)]
                        (make-new-state! this initial)))

  "render" (fn [this]
             (let [[binding ref e _] (r0/get-args this)]
               (render e 
                       (assoc binding
                              :state [(:state binding) (:state (r0/get-state this))]
                              :store (stores/conc-store (:store binding)
                                                        (:store (r0/get-state this))))
                       ref)))
  
  [:static "getDerivedStateFromProps"] (new-state-reinit! (fn [[_ _ _ initial]] initial)))

(extend-type base/LocalState
  IReact
  (-instantiate-react [{e :e initial :initial} binding ref]
    (r0/elem local-state nil [binding ref e initial])))

(let [h (fn [binding f pred actions]
          (let [{handle true pass false} (group-by pred actions)]
            (doseq [action handle]
              (call-event-handler! binding f action))
            ((:action-target binding) pass)))]
  (r0/defclass handle-action
    "render" (fn [this]
               (let [[binding ref e f pred] (r0/get-args this)]
                 (render e
                         (assoc binding :action-target (f/partial h binding f pred))
                         ref)))))

(extend-type base/HandleAction
  IReact
  (-instantiate-react [{e :e f :f pred :pred} binding ref]
    (r0/elem handle-action nil [binding ref e f pred])))

(let [send! (fn [binding r]
              (call-event-handler! binding (constantly r)))]
  (r0/defclass with-async-return
    "render" (fn [this]
               (let [[binding ref f & args] (r0/get-args this)]
                 (render (apply f (f/partial send! binding) args)
                         binding
                         ref)))))

(extend-type base/WithAsyncReturn
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-async-return nil (list* binding ref f args))))

(r0/defclass lifecycle
  "render" (fn [this] nil)

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
    "render" (fn [this]
               (let [[binding ref e f] (r0/get-args this)]
                 (render e
                         (assoc binding
                                :store (stores/handle-store-updates (:store binding) (f/partial upd binding f)))
                         ref)))))

(extend-type base/HandleStateChange
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-state-change nil [binding ref e f])))

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
                                                  (let [[_ _ _ validate-state!] (r0/extract-args props)]
                                                    (when validate-state!
                                                      (validate-state! (r0/extract-state state))))
                                                  nil)

            "render" (fn [this]
                       (let [[binding ref e _] (r0/get-args this)]
                         (render e binding ref)))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReact
  (-instantiate-react [{e :e name-id :name-id validate-state! :validate-state!} binding ref]
    (r0/elem (named name-id) nil [binding ref e validate-state!])))

(r0/defclass static
  "render" (fn [this]
             (let [[binding ref f args] (r0/get-args this)]
               (render (apply f args)
                       binding
                       ref))))

(extend-type base/Static
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem static nil [(assoc binding
                                :state nil
                                :store stores/void-store)
                         ref
                         f args])))

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
    ;; Note: ref is for refering to the dom element itself (to access the native node); c-ref is for message passing.
    ;; As one cannot send messages to dom elements, the only purpose is to make a better error messages; do that only in dev-mode:
    (if (or (not (empty? events))
            (and dev-mode? (some? c-ref)))
      (r0/elem (dom-class type) c-ref [binding attrs ref events children])
      (native-dom type binding attrs ref children))))

(r0/defclass fragment
  $handle-message (message-deadend "fragment")
  "render" (fn [this]
             (let [[binding children] (r0/get-args this)]
               (apply r0/fragment (map #(render-child % binding)
                                       children)))))

(extend-type base/Fragment
  IReact
  (-instantiate-react [{children :children} binding ref]
    ;; Note: React fragments can't have a ref. We don't really need them, except to make better error messages - do that only in dev-mode:
    (if (and dev-mode? (some? ref))
      (r0/elem fragment ref [binding children])
      (apply r0/fragment (map #(render-child % binding)
                              children)))))

(r0/defclass id
  "render" (fn [this]
             (let [[binding ref e] (r0/get-args this)]
               (render e binding ref))))

(extend-type base/Keyed
  IReact
  (-instantiate-react [{e :e key :key} binding ref]
    (r0/elem id key nil [binding ref e])))

(defrecord RRef [ref]
  base/Ref
  (-deref-ref [_] (.-current ref)))

(r0/defclass with-ref
  "getInitialState" (fn [this] {:ref (RRef. (r0/create-ref))})
  
  "render" (fn [this]
             (let [[binding ref f args] (r0/get-args this)]
               (render (apply f (:ref (r0/get-state this)) args)
                       binding ref))))

(extend-type base/WithRef
  IReact
  (-instantiate-react [{f :f args :args} binding ref]
    (r0/elem with-ref nil [binding ref f args])))

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

(r0/defclass ^:private on-mount
  "render" (fn [this] (r0/fragment))
  
  "componentDidMount"
  (fn [this]
    (let [[f] (r0/get-args this)]
      (f))))

(r0/defclass handle-error
  ;; store error in state, then immediately clear it again and call
  ;; handler; handler is reponsible to the reset the problem. Note:
  ;; handle-error matched better with 'componentDidCatch' -
  ;; core/try-catch would be the better primitive when using
  ;; getDerivedStateFromError.
  
  "getInitialState" (fn [this] {:error nil})
  
  "render" (fn [this]
             (let [[binding ref e f] (r0/get-args this)]
               (let [error (:error (r0/get-state this))]
                 (if (some? error)
                   (r0/elem on-mount nil
                            [(fn []
                               (r0/set-state this (constantly {:error nil}))
                               (call-event-handler! binding f error))])
                   (render e binding ref)))))

  [:static "getDerivedStateFromError"] (fn [error] (r0/mk-state {:error error})))

(extend-type base/HandleError
  IReact
  (-instantiate-react [{e :e f :f} binding ref]
    (r0/elem handle-error nil [binding ref e f])))


;; React compat

(extend-type interop/LiftReact
  IReact
  (-instantiate-react [{class :class props :props} binding ref]
    ;; FIXME: ref + a ref in props? How does that relate?
    (react/createElement class props)))
