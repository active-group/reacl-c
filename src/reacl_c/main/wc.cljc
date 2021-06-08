(ns reacl-c.main.wc
  "Functions to define items as a web component."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.functions :as f]
            #_[goog.object :as gobj]))

;; TODO: extending existing elements?
(defrecord ^:private WebComponent [item initial-state connected disconnected adopted observed-attributes attribute-changed])

(defn base [item]
  (WebComponent. item nil nil nil nil nil nil))

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

;; TODO (defn shadow-root [wc])
;; TODO: event util.

(defrecord ^:private Call [f args])

(defn ^:no-doc wrap [item]
  (->> item
       (c/handle-message (fn [state msg]
                           (cond
                             (instance? Call msg) (apply (:f msg) state (:args msg))
                             ;; TODO: else forward to item?
                             )))))

#?(:cljs (defn- eval-event-unmounted [class handler args]
           (let [wc (.-definition class)
                 state (:initial-state wc)
                 r (c/as-returned (apply handler state args))]
             (assert (empty? (c/returned-actions r)) "Cannot return actions from this web component handler in the unmounted state.")
             (assert (empty? (c/returned-messages r)) "Cannot return messages from this web component handler in the unmounted state.")
             (let [state (c/returned-state r)]
               (when (not= c/keep-state state)
                 (set! (.-definition class)
                       (assoc wc :initial-state state)))))))

#?(:cljs
   (defn ^:no-doc method-wc [class field]
     (fn [& args]
       (this-as ^js this
         (let [f (field (.-definition class))
               app (.-app this)]
           ;; an attribute change may occur before the connected event
           ;; (the element is mounted), in which case we don't have a
           ;; running app yet; in that case, we change the
           ;; initial-state for now, and complain about actions and
           ;; messages.
           (when f
             (if (some? app)
               (main/send-message! app (Call. f args))
               (eval-event-unmounted class f args))))))))

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
           #js {:forceUpdate (fn []
                               (this-as ^js this
                                 (let [wc (.-definition ctor)]
                                   (set! (.-app this)
                                         (main/run this (wrap (:item wc)) {:initial-state (:initial-state wc)})))))

                :connectedCallback (let [user (method-wc ctor :connected)]
                                     (fn []
                                       (this-as ^js this
                                         ;; Note: we cannot render before 'connected event'.
                                         (.forceUpdate this)
                                         (.call user this))))
                :disconnectedCallback (method-wc ctor :disconnected)
                :adoptedCallback (method-wc ctor :adopted)
                :attributeChangedCallback (method-wc ctor :attribute-changed)}]
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
                 (js/customElements.define n f)
                 f))]
       (assert (some? (lift wc)))
       ;; TODO: return interop/wc item using ?
       f)))

(defmacro define-wc [n wc]
  (assert (symbol? n))
  `(def ~n (define-wc* ~(name n) ~wc)))
