(ns reacl-c.impl.reacl
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.utils :as utils]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.xpath :as xp]
            [reacl2.dom :as rdom]
            [clojure.string :as str])
  (:refer-clojure :exclude [refer]))

(defprotocol ^:private IReacl
  (-instantiate-reacl [this binding] "Returns a list of Reacl components or dom elements.")
  (-xpath-pattern [this] "An xpath that selects something like this item.") ;; Note: multiple semantics of this possible; for now a very lose match.
  )

(defrecord WrapRef [reacl-ref]
  base/Ref
  (-deref-ref [this] (rcore/get-dom reacl-ref)))

(defn- reacl-ref [wr]
  (assert (instance? WrapRef wr) "Expected a ref from the reacl implementation.")
  (:reacl-ref wr))

(defn xpath-pattern [e]
  (cond
    (string? e) (xp/comp (xp/is= e))
    (nil? e) xp/self ;; nothing can be found in anything
    (satisfies? IReacl e) (-xpath-pattern e)
    :else (assert false (str "No reacl implementation for: " (pr-str e)))))

(defn- class-args-pattern [class args]
  (xp/comp (xp/type class)
           (xp/where (xp/comp xp/args (xp/is= args)))))

(defn- wrapper-pattern [class e & rest-args]
  ;; first arg must be the sub item.
  (xp/comp (xp/type class)
           (xp/where (if (nil? rest-args) ;; can't select no args   (FIXME: an 'is-empty' selector?)
                       xp/self
                       (xp/comp xp/args (xp/is? (fn [args_]
                                                  (= (rest args_) rest-args))))))
           ;; TODO: exactly one child or not?
           (xp/where (xp/comp xp/children (xpath-pattern e)))))

(extend-type base/LiftReacl
  IReacl
  (-xpath-pattern [{class :class args :args}]
    (class-args-pattern class [args]))
  (-instantiate-reacl [{class :class args :args} binding]
    [(if (and (rcore/reacl-class? class) (rcore/has-app-state? class))
       (apply class binding args)
       (apply class args))]))

(def ^:private non-dynamic-binding (rcore/use-app-state nil))

(defn instantiate
  "Returns a Reacl component/element for the given item and state binding."
  [binding e]
  (cond
    (satisfies? IReacl e) (let [cs (if (or (= binding non-dynamic-binding) (not (base/is-dynamic? e)))
                                     ;; if it is 'static', then isolate it from the changing state from outside
                                     ;; can be a bit faster for large dom trees.
                                     (-instantiate-reacl e non-dynamic-binding)
                                     ;; dynamic:
                                     (-instantiate-reacl e binding))]
                            (if (and (not (empty? cs)) (empty? (rest cs)))
                              (first cs)
                              (apply rdom/fragment cs)))
    (string? e) (rdom/fragment e)
    (nil? e) (rdom/fragment)
    
    :else (throw (ex-info "Expected an item or a string only." {:value e}))))

(defn- instantiate-child [binding e]
  ;; returns an object suitable as a react element child.
  (cond
    (satisfies? IReacl e) (instantiate binding e)
    (string? e) e
    (nil? e) (rdom/fragment)
    :else (throw (ex-info "Expected an item or a string only." {:value e}))))

(defrecord ^:private ActionMessage [action])
(defrecord ^:private StateMessage [value])

(defn- pass-message [child msg]
  (let [comp (rcore/get-dom child)]
    (assert (rcore/component? comp) (str "Not a component: " (pr-str comp) ". Forgot to use refer?"))
    (rcore/return :message [comp msg])))

(defn- message-deadend [type msg]
  (throw (ex-info (str "Cannot send a message to " type " items.") {:value msg})))

(defn ^:no-doc transform-return [r]
  ;; a base/Returned value to a rcore/return value.
  (if (base/returned? r)
    (apply rcore/merge-returned
           (if (not= base/keep-state (:state r))
             (rcore/return :app-state (:state r))
             (rcore/return))
           (concat (map (fn [a] (rcore/return :action a))
                        (:actions r))
                   (map (fn [[target msg]]
                          (let [c (base/deref-message-target target)]
                            (assert (some? c) (str "Target for message not available. Forgot to use refer?"))
                            (assert (rcore/component? c) (str "Target for message is not a component: " (pr-str c)))
                            (rcore/return :message [c msg])))
                        (:messages r))))
    (rcore/return :app-state r)))

(rcore/defclass ^:private toplevel this [state e onchange onaction]
  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction state (rcore/reaction this ->StateMessage)) e)
      (rcore/refer child)
      (rcore/action-to-message this ->ActionMessage))

  handle-message
  (fn [msg]
    (cond
      (instance? StateMessage msg)
      (transform-return (onchange (:value msg)))
      
      (instance? ActionMessage msg)
      (transform-return (onaction (:action msg))) 

      :else (pass-message child msg))))


(defrecord ^:private ReaclApplication [comp]
  base/Application
  (-component [this] comp)
  (-send-message! [this msg]
    (rcore/send-message! comp msg)))

(defn run
  [dom item state onchange onaction]
  (ReaclApplication. (rcore/render-component dom
                                             toplevel
                                             state item
                                             onchange onaction)))



(rcore/defclass ^:private handle-message this state [e f]
  should-component-update?
  (fn [new-state _ new-e new-f]
    (or (not= e new-e)
        (not= state new-state)))
  
  handle-message
  (fn [msg]
    (transform-return (f state msg)))

  render
  (instantiate (rcore/bind this) e))

(extend-type base/HandleMessage
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-message e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-message binding e f)]))

(defn- gen-named [s]
  (rcore/class s this state [e validate-state!]
               validate (when validate-state!
                          (validate-state! state))

               refs [child]

               handle-message
               (fn [msg] (pass-message child msg))

               render (-> (instantiate (rcore/bind this) e)
                          (rcore/refer child))))

(def named (utils/named-generator gen-named))

(extend-type base/Named
  IReacl
  (-xpath-pattern [{e :e name-id :name-id validate-state! :validate-state!}]
    (wrapper-pattern (named name-id) e validate-state!))
  (-instantiate-reacl [{e :e name-id :name-id validate-state! :validate-state!} binding]
    [((named name-id) binding e validate-state!)]))

(def ^:private static-binding
  (rcore/use-reaction nil
                      (rcore/reaction :parent
                                      (fn [state]
                                        (throw (ex-info (str "Static items must not change their state. The item tried to set it to: " (pr-str state))
                                                        {:value state}))))))

(rcore/defclass static this _ [f args]
  should-component-update?
  (fn [_ _ new-f new-args]
    (or (not= f new-f)
        (not= args new-args)))

  refs [child]
  
  handle-message
  (fn [msg]
    (pass-message child msg))

  render
  (-> (instantiate static-binding (apply f args))
      (rcore/refer child)))

(extend-type base/Static
  IReacl
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern static (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply static static-binding f args)]))

(defrecord ^:private EventMessage [ev])

(defn- find-event-handler [ev events]
  (when-not (.-type ev)
    (throw (ex-info "Expected a JavaScript event object, with a property 'type'." {:value ev})))
  (let [en (str/lower-case (.-type ev))]
    ;; OPT: could be done a little faster - try a lookup, or prepare a different map. But it can be 'onchange' and 'onChange'.
    (some (fn [[n f]]
            ;; "change" = :onChange ?
            ;; FIXME: are things like onChangeCapture possible? how to handle them?
            (and (= en (str/lower-case (subs (name n) 2)))
                 f))
          events)))

(defn- dom-message-to-action [state msg events]
  (cond
    (instance? EventMessage msg)
    (let [{ev :ev} msg
          f (find-event-handler ev events)]
      (transform-return (f state ev)))

    :else
    (message-deadend "dom" msg)))

(defn- dom-event-handler [target]
  (fn [ev]
    (rcore/send-message! target (EventMessage. ev))))

(defn- merge-dom-attrs [attrs events handler self-ref]
  ;; Note: events are non-empty here.
  (-> (reduce (fn [attrs k]
                (assoc! attrs k handler))
              (cond-> (transient attrs)
                self-ref (assoc! :ref (reacl-ref self-ref)))
              events)
      (persistent!)))

(defn- native-dom [binding type attrs & children]
  (apply rdom/element type
         attrs
         ;; Note: it's important to have text elements as direct
         ;; children of dom elements. A "<option>...</option>" breaks
         ;; in Chrome for example, when the content is even a
         ;; fragment.
         (map (partial instantiate-child binding) children)))

(def dom-class-for-type
  ;; There should be a finite set of tag names, so using memoize should be ok.
  (memoize
   (fn [type]
     (rcore/class (str "reacl-c.dom/" type) this state [attrs events ref & children]
                  ;; dom with action events and children.
                  local-state [handler (dom-event-handler this)]
  
                  handle-message
                  (fn [msg]
                    (dom-message-to-action state msg events))

                  ;; Note that we could exclude updates when only event-handler functions have changed (if it's worth it to check?)
                  render
                  (apply native-dom (rcore/bind this) type (merge-dom-attrs attrs (keys events) handler ref)
                         children)))))

(defn- dom-class [binding type events ref & children]
  (apply (dom-class-for-type type) binding events ref children))

(defn- dom [binding type attrs events ref & children]
  ;; optimize for no events (makes quite a difference!)
  (if (empty? events)
    (apply native-dom binding type (cond-> attrs
                                     ref (assoc :ref (reacl-ref ref))) children)
    (apply dom-class binding type attrs events ref children)))

(defn- flatten-fragment [item]
  (if (base/fragment? item)
    (mapcat flatten-fragment (:children item))
    (list item)))

(defn- flatten-children [children]
  (mapcat flatten-fragment children))

(extend-type dom/Element
  IReacl
  (-xpath-pattern [{type :type attrs :attrs events :events ref :ref children :children}]
    (let [children (flatten-children children)]
      (xp/comp
       ;; match the tag, or the tag below the matching wrapper class.
       (xp/or (xp/tag type)
              (xp/comp (xp/class (dom-class-for-type type))
                       xp/children))
       ;; attributes match, with specials for class and style.
       (xp/where
        (apply xp/and
               (map (fn [[k v]]
                      (case k
                        (:class :className "class" "className") (xp/css-class? v)
                        (:style "style") (xp/style? v)
                        (xp/where (xp/comp (xp/attr k) (xp/is= v)))))
                    attrs)))
       ;; has the events
       (xp/where
        (apply xp/and
               (map (fn [[k v]]
                      ;; enough to 'have' the event.
                      (xp/where (xp/attr k)))
                    events)))
       ;; ref is ignored for now.
             
       ;; all children must match in any order (not possible to check the order, I think?)
       ;; but the number of matching children must match either - that works around
       ;; two similar children matching the same node
       (xp/where
        (if (empty? children)
          xp/self
          (xp/comp xp/children
                   ;; we want to allow a dom or text child to match further down; 'skipping' any
                   ;; of our wrapper classes in between. Note that multiple nodes may match the same child then.
                   (xp/where (let [child-match (apply xp/or (map xpath-pattern children))]
                               (xp/or child-match
                                      (xp/comp (xp/first-where (xp/not xp/class?))
                                               child-match))))
                   (xp/count= (count children))))))))
  (-instantiate-reacl [{type :type attrs :attrs events :events ref :ref children :children} binding]
    [(apply dom binding type attrs events ref children)]))

(extend-type base/Fragment
  IReacl
  (-xpath-pattern [{children :children}]
    ;; as fragments disappear in the renderes node tree, it's hard to accurately select for them;
    ;; this selects any children of the parent, that has all the specified non-fragment children (but it may have more).
    (let [children (flatten-children children)]
      (if (empty? children)
        ;; empty can be found in everything
        xp/self
        (do #_(assert false "fragment nodes cannot be selected, or can they?")
            (xp/comp xp/parent
                     (apply xp/and
                            (map (fn [c]
                                   (xp/where (xp/comp xp/children (xpath-pattern c))))
                                 children))
                     )))))
  (-instantiate-reacl [{children :children} binding]
    (mapv (partial instantiate binding)
          children)))

(defn- keyed [binding e key]
  ;; TODO: is a keyed fragment/string meaningful?
  (-> (instantiate binding e)
      (rcore/keyed key)))

(extend-type base/Keyed
  IReacl
  (-xpath-pattern [{e :e key :key}]
    ;; FIXME: the key does not work... don't know why.
    (xp/and #_(xp/where (xp/comp xp/key (xp/is= key)))
            (xpath-pattern e)))
  (-instantiate-reacl [{e :e key :key} binding]
    [(keyed binding e key)]))

(rcore/defclass ^:no-doc with-ref this state [f & args]
  refs [child r]

  handle-message
  (fn [msg]
    (pass-message child msg))

  render
  (let [rr (WrapRef. r)]
    (-> (instantiate (rcore/bind this) (apply f rr args))
        (rcore/refer child))))

(extend-type base/WithRef
  IReacl
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern with-ref (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-ref binding f args)]))

(rcore/defclass ^:privte refer this state [e ref]
  handle-message
  (fn [msg]
    (pass-message (reacl-ref ref) msg))

  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer (reacl-ref ref))))

(extend-type base/Refer
  IReacl
  (-xpath-pattern [{e :e ref :ref}]
    (wrapper-pattern refer e ref))
  (-instantiate-reacl [{e :e ref :ref} binding]
    ;; has to be a class for now, because otherwise we would override the ref with all our 'child' refs for passing messages down.
    [(refer binding e ref)]))

(rcore/defclass ^:private dynamic this state [f & args]
  refs [child]
  
  handle-message
  (fn [msg]
    (pass-message child msg))
  
  render
  (-> (instantiate (rcore/bind this) (apply f state args))
      (rcore/refer child)))

(extend-type base/Dynamic
  IReacl
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern dynamic (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply dynamic binding f args)]))

(defn- focus [binding e lens]
  (instantiate (rcore/focus binding lens) e))

(extend-type base/Focus
  IReacl
  (-xpath-pattern [{e :e lens :lens}]
    (wrapper-pattern focus e lens))
  (-instantiate-reacl [{e :e lens :lens} binding]
    ;; if this isn't dynamic, then state binding will be 'nil' - don't run the lens over it.
    (if (base/is-dynamic? e)
      [(focus binding e lens)]
      [(instantiate binding e)])))

(rcore/defclass ^:private handle-action this state [e f pred]
  local-state [action-to-message
               (fn [_ action]
                 (if (pred action)
                   (rcore/return :message [this (ActionMessage. action)])
                   (rcore/return :action action)))]

  refs [child]
  
  handle-message
  (fn [msg]
    (cond
      (instance? ActionMessage msg)
      (do
        (transform-return (f state (:action msg))))
      :else
      (pass-message child msg)))
  
  should-component-update?
  (fn [new-state _ new-e _]
    (or (not= e new-e)
        (not= state new-state)))

  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child)
      (rcore/reduce-action action-to-message)))

(extend-type base/HandleAction
  IReacl
  (-xpath-pattern [{e :e f :f pred :pred}]
    (wrapper-pattern handle-action e f pred))
  (-instantiate-reacl [{e :e f :f pred :pred} binding]
    [(handle-action binding e f pred)]))

(defrecord ^:private NewIsoState [state])

(def rcore-keep-state (rcore/returned-app-state (rcore/return))) ;; prevents a (wrong) warning in newer cljs.

(defn- id-state [st1 st2]
  (if (= st1 st2)
    rcore-keep-state
    st2))

(rcore/defclass ^:private local-state this astate [e initial]
  local-state [lstate {:initial initial
                       :current initial}]

  component-will-receive-args
  (fn [new-e new-initial]
    (if (not= (:initial lstate) new-initial)
      (rcore/return :local-state {:initial new-initial
                                  :current new-initial})
      (rcore/return)))

  refs [child]
  
  render
  (-> (instantiate (rcore/use-reaction [astate (:current lstate)]
                                       (rcore/reaction this ->NewIsoState))
                   e)
      ;; FIXME: something breaks, when child is ultimately a fragment here - I think it then overwrites ref that are already set inside.. maybe.
      (rcore/refer child))
  
  handle-message
  (fn [msg]
    (cond
      (instance? NewIsoState msg)
      (let [{[new-app-state new-local-state] :state} msg]
        (rcore/return :app-state (id-state astate new-app-state)
                      :local-state (id-state lstate (assoc lstate :current new-local-state))))
      :else
      (pass-message child msg))))

(extend-type base/LocalState
  IReacl
  (-xpath-pattern [{e :e initial :initial}]
    (wrapper-pattern local-state e initial))
  (-instantiate-reacl [{e :e initial :initial} binding]
    [(local-state binding e initial)]))

(defrecord ^:private AsyncReturn [ret])

(rcore/defclass ^:private with-async-return this state [f & args]
  local-state [send! (fn [v]
                       ;; Note: if v only contains a message, we might want to optimize and send directly?
                       ;; (didn't work for some unknown reasons though)
                       (rcore/send-message! this (AsyncReturn. v))
                       nil)]
  refs [child]
  
  render
  ;; Note: for some unknown reason, sending messages directs to (get-dom child) does not work; it's not assigned when dereferenced in 'async-msg!'.
  (let []
    (-> (instantiate (rcore/bind this) (apply f send! args))
        (rcore/refer child)))

  handle-message
  (fn [msg]
    (condp instance? msg
      AsyncReturn (transform-return (:ret msg))

      (pass-message child msg))))

(extend-type base/WithAsyncReturn
  IReacl
  (-xpath-pattern [{f :f args :args}]
    (class-args-pattern with-async-return (list* f args)))
  (-instantiate-reacl [{f :f args :args} binding]
    [(apply with-async-return binding f args)]))

(defrecord ^:private Mount []) (def ^:private mount-msg (Mount.))
(defrecord ^:private Unmount []) (def ^:private unmount-msg (Unmount.))
(defrecord ^:private Update []) (def ^:private update-msg (Update.))

(rcore/defclass ^:private lifecycle this state [init finish]
  handle-message
  (fn [msg]
    (condp = msg
      mount-msg
      (transform-return (init state))
      
      update-msg
      (transform-return (init state))

      unmount-msg
      (transform-return (finish state))

      (message-deadend "lifecycle" msg)))

  should-component-update?
  (fn [new-state _ new-init new-finish]
    ;; we can ignore changes of the finish fn.
    (or (not= state new-state)
        (not= init new-init)))

  component-did-update
  (fn [prev-app-state prev-local-state prev-ret prev-cleanup-ret]
    ;; Note: Reacl does not guarantee that the state is accurate in this method (yet); need to go through a message.
    (rcore/return :message [this update-msg]))

  component-did-mount
  (fn []
    (rcore/return :message [this mount-msg]))

  component-will-unmount
  (fn []
    (rcore/return :message [this unmount-msg]))

  render (rdom/fragment))

(extend-type base/Lifecycle
  IReacl
  (-xpath-pattern [{init :init finish :finish}]
    (class-args-pattern lifecycle [init finish]))
  (-instantiate-reacl [{init :init finish :finish} binding]
    [(lifecycle binding init finish)]))

(defrecord ^:private MonitorMessage [new-state])

(rcore/defclass ^:private handle-state-change this state [e f]
  refs [child]
  
  should-component-update?
  (fn [new-state _ new-e _]
    (or (not= e new-e)
        (not= state new-state)))

  render
  (-> (instantiate (rcore/use-reaction state (rcore/reaction this ->MonitorMessage)) e)
      (rcore/refer child))

  handle-message
  (fn [msg]
    (cond
      (instance? MonitorMessage msg)
      (let [new-state (:new-state msg)]
        (transform-return (f state new-state)))
      :else
      (pass-message child msg))))

(extend-type base/HandleStateChange
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-state-change e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-state-change binding e f)]))

(rcore/defclass ^:private handle-error this state [e f]
  
  refs [child]
  
  handle-message (fn [msg] (pass-message child msg))
  
  should-component-update?
  (fn [new-state _ new-e _]
    (or (not= e new-e)
        (not= state new-state)))

  render
  (-> (instantiate (rcore/bind this) e)
      (rcore/refer child))

  component-did-catch
  (fn [error info]
    ;; Note: info is already deprecated in React, in the sense that
    ;; 'getDerivedStateFromError' does not have it. It's also very
    ;; implementation dependant, and less informative in our setup.
    ;; Leave that our for now.
    (transform-return (f state error))))

(extend-type base/HandleError
  IReacl
  (-xpath-pattern [{e :e f :f}]
    (wrapper-pattern handle-error e f))
  (-instantiate-reacl [{e :e f :f} binding]
    [(handle-error binding e f)]))
