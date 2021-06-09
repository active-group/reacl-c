(ns reacl-c.main.wc
  "Functions to define items as a web component."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

;; TODO: extending existing elements?
(defrecord ^:private WebComponent [item initial-state connected disconnected adopted observed-attributes attribute-changed properties methods])

(defn base [item & [initial-state]]
  (WebComponent. item initial-state nil nil nil nil nil {} {}))

(defn ^:no-doc lift [item]
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

(defn connected [wc f]
  (assert (some? f))
  (conc wc :connected f))

(defn disconnected [wc f]
  (conc wc :disconnected f))

(defn adopted [wc f]
  (conc wc :adopted f))

(let [when-f (fn [attr f state changed-attr old-value new-value]
               (if (= attr changed-attr)
                 (f state old-value new-value)
                 (c/return)))]
  (defn attribute-changed [wc attr f]
    (-> wc
        (conc :attribute-changed (f/partial when-f attr f))
        (update :observed-attributes #(conj (or %1 #{}) %2) attr))))

(let [f (fn [lens state old new]
          (lens/shove state lens new))]
  (defn attribute [wc attr & [state-lens]]
    (let [kw (if-not (keyword? attr)
               (keyword attr)
               attr)
          lens (or state-lens kw)]
      (attribute-changed wc (name kw) (f/partial f lens)))))

(defn raw-property [wc property descriptor]
  (update (lift wc) :properties assoc (if (keyword? property) (name property) property) descriptor))

(defn data-property [wc property value & [options]]
  (raw-property wc property (assoc options :value value)))

(defn accessor-property [wc property get & [set options]]
  (raw-property wc property (assoc options :get get :set set)))

(let [get (fn [lens state]
            (lens/yank state lens))
      set (fn [lens state value]
            (lens/shove state lens value))]
  (defn property [wc property & [lens options]]
    (let [lens (or lens
                   ;; e.g. a keyword
                   property)]
      (accessor-property wc
                         property
                         (f/partial get lens)
                         (when-not (:read-only? options)
                           (f/partial set lens))
                         (dissoc options :read-only?)))))

(defn method [wc method f]
  (update (lift wc) :methods assoc (if (keyword? method) (name method) method) f))

(let [g (fn [f state return & args]
          (c/return :action (return (apply f state args))))]
  (defn accessor-method [wc method f]
    (method wc method (f/partial g f))))

(let [g (fn [f state return & args]
          (apply f state args))]
  (defn mutator-method [wc method f]
    (method wc method (f/partial g f))))

;; TODO (defn shadow-root [wc])
;; TODO: event util.

(defrecord ^:private Event [f args])
(defrecord ^:private Access [f args result])

(c/defn-effect ^:private set-atom [a value]
  (reset! a value))

(defn ^:no-doc wrap [item]
  (->> item
       (c/handle-message (fn [state msg]
                           (cond
                             (instance? Event msg) (apply (:f msg) state (:args msg))
                             (instance? Access msg) (c/return :action (set-atom (:result msg) (apply (:f msg) state (:args msg))))
                             ;; TODO: else forward to item?
                             )))))

(let [set-atom-f (base/effect-f (set-atom nil nil))]
  (defn- emulate-set-atom! [returned]
    (let [as (base/returned-actions returned)]
      (lens/overhaul returned base/returned-actions
                     (fn [as]
                       (reduce (fn [res a]
                                 (js/console.log )
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
         (main/send-message! app (Event. f args))
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
           (main/send-message! app (Access. f args result))
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
   (defn property-wc [class [property-name descriptor]]
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
           full-args (cons (f/partial set-atom result) args)
           app (.-app this)]
       (if (some? app)
         ;; as of now, event handling is synchronous
         (do
           (main/send-message! app (Event. f full-args))
           @result)

         (do (eval-event-unmounted class f full-args)
             @result)))))

#?(:cljs
   (defn method-wc [class f]
     (fn [& args]
       (this-as this (call-method-wc class this f args)))))

#?(:cljs
   (defn prototype-of [tag-or-class]
     (if (string? tag-or-class)
       (js/Object.getPrototypeOf (js/document.createElement tag-or-class))
       (.-prototype tag-or-class))))

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
                                         (let [wc (.-definition ctor)]
                                           (set! (.-app this)
                                                 (main/run this (wrap (:item wc)) {:initial-state (:initial-state wc)})))
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
   (defn get-wc [n]
     (js/customElements.get n)))

#?(:cljs
   (defn define-wc* [n wc & [options]]
     (let [f (if-let [f (and (not (:no-hot-update? options))
                             (get-wc n))]
               ;; trying at least some hot updates:
               (do (assert (some? (.-definition f)) "Can only redefine components defined by this library.")
                   ;; Note: this will not update existing elements immediately (only after they update somehow; but we cannot force this here, I think)
                   (set! (.-definition f) (lift wc))
                   f)
               (let [f (new-wc)]
                 (set! (.-definition f) (lift wc))
                 ;; properties cannot be added or remove on a hot update
                 (js/Object.defineProperties (.-prototype f) (apply js-obj (mapcat (partial property-wc f) (:properties wc))))
                 (js/customElements.define n f)
                 f))]
       ;; Note: this will not remove methods on a hot code reload:
       (let [p (.-prototype f)]
         (doseq [[m-name impl] (:methods wc)]
           (aset p m-name (method-wc f impl))))
       
       ;; TODO: return interop/wc item using ?
       f)))

(defmacro define-wc [n wc]
  (assert (symbol? n))
  `(def ~n (define-wc* ~(name n) ~wc)))
