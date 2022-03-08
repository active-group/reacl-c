(ns reacl-c.test-util.core-testing
  (:require [reacl-c.base :as base]
            [reacl-c.dom-base :as dom]
            [reacl-c.core :as c]
            [active.clojure.lens :as lens]
            [clojure.string :as string]
            [clojure.set :as set])
  (:refer-clojure :exclude [contains?]))

(def ^:private clj-contains? clojure.core/contains?)

;; Only pure tests: No local-/state changes, no implicit mounting (init), no effects, no subscriptions.

(defn- reduce-item [mk-ref mk-async f-leaf f-container f-wrapper f-dynamic f-other item state]
  (let [rec (fn [state item]
              (reduce-item mk-ref mk-async f-leaf f-container f-wrapper f-dynamic f-other
                           item state))]
    (cond
      (string? item)
      (f-leaf item state)

      (base/lifecycle? item)
      (f-leaf item state)

      (base/dynamic? item)
      (f-dynamic (rec state (apply (base/dynamic-f item) state (base/dynamic-args item))) item state)

      (or (nil? item) (base/fragment? item))
      (f-container (map #(rec state %)
                        (if (nil? item) nil (base/fragment-children item)))
                   item state)

      (dom/element? item)
      (f-container (map #(rec state %)
                        (dom/element-children item))
                   item state)

      (base/static? item)
      (f-dynamic (rec state (apply (base/static-f item) (base/static-args item))) item state)

      (base/with-ref? item)
      (f-dynamic (rec state (apply (base/with-ref-f item) (mk-ref) (base/with-ref-args item))) item state)

      (base/with-async? item)
      (f-dynamic (rec state (apply (base/with-async-f item) (mk-async) (base/with-async-args item))) item state)

      (base/focus? item)
      (f-wrapper (rec (lens/yank state (base/focus-lens item))
                      (base/focus-e item))
                 item state)

      (base/local-state? item)
      (f-wrapper (rec [state (base/eval-local-state-init (base/local-state-initial item))]
                      (base/local-state-e item))
                 item state)

      (base/handle-action? item)
      (f-wrapper (rec state (base/handle-action-e item))
                 item state)

      (base/refer? item)
      (f-wrapper (rec state (base/refer-e item))
                 item state)

      (base/handle-state-change? item)
      (f-wrapper (rec state (base/handle-state-change-e item))
                 item state)

      (base/handle-message? item)
      (f-wrapper (rec state (base/handle-message-e item))
                 item state)

      (base/named? item)
      (f-wrapper (rec state (base/named-e item))
                 item state)

      (base/handle-error? item)
      (f-wrapper (rec state (base/handle-error-e item))
                 item state)

      (base/keyed? item)
      (f-wrapper (rec state (base/keyed-e item))
                 item state)

      ;; TODO:?
      #_(interop/lift-react? item)
      #_item

      :else
      (f-other item state))))

(defn- unknown-item-error [item]
  (ex-info (str "Unknown item: " (pr-str item)) {:item item}))

(defn- make-dummy-ref []
  (reify base/Ref
    (-deref-ref [this] (throw (ex-info "Cannot derefence in a test environment." {})))))

(defn- dummy-async [r]
  (throw (ex-info "Cannot do an async injection in a test environment." {})))

(defn render
  "Returns how an item looks like in the given state. Returns a list
  of only dom elements and strings." ;; TODO: + react items?
  [item & [state]]
  (reduce-item make-dummy-ref
               (constantly dummy-async)
               (fn leaf [item state]
                 (cond
                   (string? item)
                   (list item)

                   (base/lifecycle? item) nil
                   
                   :else (throw (unknown-item-error item))))
               (fn container [c-res item state]
                 (cond
                   (nil? item) nil
                   (base/fragment? item) (apply concat c-res)
                   (dom/element? item) (list (lens/shove item dom/element-children (apply concat c-res)))
                   :else (throw (unknown-item-error item))))
               (fn wrapper [res item state]
                 (cond
                   (or (base/focus? item)
                       (base/local-state? item)
                       (base/handle-action? item)
                       (base/refer? item)
                       (base/handle-state-change? item)
                       (base/handle-message? item)
                       (base/named? item)
                       (base/handle-error? item)
                       (base/keyed? item))
                   res
                   
                   :else (throw (unknown-item-error item))))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async? item)
                       (base/static? item))
                   res

                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 ;; -> or an IRenderable extension point?
                 (throw (unknown-item-error item)))
               item
               state))

(defn- split-css-classes [s]
  (map string/trim (string/split (or s "") #" ")))

(defn- contains-in-order? [l1 l2] ;; l1 contains l2 ?
  (or (empty? l2)
      (and (not (empty? l1))
           (or (= (first l1) (first l2))
               (contains-in-order? (rest l1) l2)))))

(defn- dom-attrs-contains? [a1 a2]  ;; if a1 contains a2; resp. a2 < a1
  (reduce-kv (fn [res k v]
               (and res
                    (or (= (get a1 k) v)
                        ;; or sub matching for style and class attributes
                        (case k
                          :style
                          (let [st1 (:style a1)]
                            (reduce-kv (fn [res k v]
                                         (and res (= (get st1 k) v)))
                                       true
                                       v))
                          :class
                          (contains-in-order? (split-css-classes (:class a1))
                                              (split-css-classes v))
                          

                          false))))
             true
             a2))

(defn- item-empty? [item]
  (or (nil? item) (and (base/fragment? item) (empty? (base/fragment-children item)))))

(declare like?)

(defn- list-like? [lst sub-lst]
  (let [[missing remaining]
        (reduce (fn [[n remaining] c]
                  (loop [remaining remaining]
                    (if (empty? remaining)
                      [n nil]
                      (if (or (= (first remaining) c)
                              (like? (first remaining) c))
                        [(dec n) remaining]
                        (recur (rest remaining))))))
                [(count sub-lst) lst]
                sub-lst)]
    (zero? missing)))

(defn- dom-like? [item sub-item]
  (assert (dom/element? item) item)
  (assert (dom/element? sub-item) sub-item)
  
  (and (= (dom/element-type item) (dom/element-type sub-item))
       (dom-attrs-contains? (dom/element-attrs item) (dom/element-attrs sub-item))
       ;; item has all events defined that sub-item has:
       (set/subset? (set (keys (dom/element-events sub-item)))
                    (set (keys (dom/element-events item))))
       ;; TODO: maybe flatten out fragments in children? maybe concatenate strings?
       (list-like? (dom/element-children item)
                   (dom/element-children sub-item))))

(defn like?
  "Returns if `sub-item` is like `item`, meaning:

  - if both are a strings, then `sub-item` is a substring in `item`,
  - if both are dom elements, then `sub-item` is the same type of dom element, but may contain less attributes or children,
  - if both have children, then every child of `sub-item` is `like?` a child in `item`, and in the same order.
"
  [item sub-item]
  ;; Note: (string/includes? "foo" "") is true, so we say 'nothing is like anything' too.
  (cond
    (string? item)
    (and (string? sub-item)
         (string/includes? item sub-item))

    (item-empty? item)
    (or (nil? sub-item)
        (and (base/fragment? sub-item)
             (list-like? nil
                         (base/fragment-children sub-item))))

    (base/fragment? item) ;; always a non-empty fragment here
    (and (base/fragment? sub-item)
         (list-like? (base/fragment-children item)
                     (base/fragment-children sub-item)))

    (dom/element? item)
    (and (dom/element? sub-item)
         (dom-like? item sub-item))

    (or (base/lifecycle? item)
        (base/dynamic? item)
        (base/static? item)
        (base/with-ref? item)
        (base/with-async? item))
    (= item sub-item)

    (or (base/focus? item)
        (base/local-state? item)
        (base/handle-action? item)
        (base/refer? item)
        (base/handle-state-change? item)
        (base/handle-message? item)
        (base/named? item)
        (base/handle-error? item)
        (base/keyed? item))
    ;; we could say the :e's must only be 'like?', but rest equal; but
    ;; then again, that does not work for dynamics anyway.
    (= item sub-item)

    ;; TODO:?
    #_(interop/lift-react? item)

    :else
    (throw (unknown-item-error item))))

(defn- contains-by? [f item sub-item & [options]]
  (assert (every? #{:state :sub-item-state} (keys options)) (keys options))
  
  (let [state (get options :state nil)
        g (if (clj-contains? options :sub-item-state)
            (let [sub-item-state (:sub-item-state options)]
              (fn [item sub-item state]
                (and (= state sub-item-state)
                     (f item sub-item))))
            (fn [item sub-item state]
              (f item sub-item)))]
    (reduce-item make-dummy-ref
                 (constantly dummy-async)
                 (fn leaf [item state]
                   (g item sub-item state))
                 (fn container [c-res item state]
                   (or (g item sub-item state)
                       ;; one or more children of item 'contain' the sub-item
                       (reduce #(or %1 %2) false c-res)))
                 (fn wrapper [res item state]
                   (or (g item sub-item state)
                       res))
                 (fn dynamic [res item state]
                   (or (g item sub-item state)
                       res))
                 (fn other [item state]
                   (throw (unknown-item-error item)))
                 item state)))

(defn contains-like?
  "Returns if `item`, or any item 'below' it, is [[like?]] `sub-item`
  in the given state of `item`. Options can be:

  - `:state`: specifies the state of `item` used to resolve dynamic
  items in it. It defaults to `nil`,
  
  - `:sub-item-state`: if specified the sub item only matches if it
  occurs in `item` with that state initially."
  [item sub-item & options]
  (contains-by? like? item sub-item (apply hash-map options)))

(defn contains?
  "Returns if `item`, or any item 'below' it, is equal to `sub-item`
  in the given state of `item`.  Options can be:

  - `:state`: specifies the state of `item` used to resolve dynamic
  items in it. It defaults to `nil`,
  
  - `:sub-item-state`: if specified the sub item only matches if it
  occurs in `item` with that state initially."
  [item sub-item & options]
  (contains-by? = item sub-item (apply hash-map options)))

;; TODO: could/should some of these fns be shared with the 'real' implementations?

(defn- lift-returned [r]
  ;; TODO: move all impls of it to one place?!
  (if (base/returned? r) r (c/return :state r)))

(defn- merge-returned [r1 r2]
  (base/merge-returned r1 (lift-returned r2)))

(defn- de-focus [res item state]
  ;; merge changed state in subitem back into parent state:
  (let [st (base/returned-state res)]
    (if (not= base/keep-state st)
      (lens/shove res base/returned-state
                  (lens/shove state (base/focus-lens item) st))
      res)))

(defn- de-local-state [res item]
  ;; remove local part of changed state:
  (let [st (base/returned-state res)]
    (if (not= base/keep-state st)
      (lens/shove res base/returned-state (first st))
      res)))

(defn- do-handle-actions [res item state]
  (let [f (base/handle-action-f item)
        {handle true pass false} (group-by (base/handle-action-pred item)
                                           (base/returned-actions res))]
    (reduce (fn [res a]
              (let [state (if (not= base/keep-state (base/returned-state res))
                            (base/returned-state res)
                            state)]
                (merge-returned res
                                (f state a))))
            (lens/shove res base/returned-actions (vec pass))
            handle)))

(defn- do-handle-state-change [res item state]
  (let [f (base/handle-state-change-f item)]
    (let [st (base/returned-state res)]
      (if (not= base/keep-state st)
        (merge-returned res (f state st))
        res))))

(defn- run-lifecycle [item state f]
  ;; resolve, find all livecycle (with their state); call that.
  (reduce-item make-dummy-ref
               (constantly dummy-async)
               (fn leaf [item state]
                 (cond
                   (base/lifecycle? item)
                   (lift-returned (f item state))

                   (or (string? item)
                       (base/fragment? item)
                       (dom/element? item))
                   (c/return)

                   :else (throw (unknown-item-error item))))
               (fn container [c-res item state]
                 (cond
                   (or (nil? item)
                       (base/fragment? item)
                       (dom/element? item))
                   (reduce merge-returned (c/return) c-res)

                   :else (throw (unknown-item-error item))))
               (fn wrapper [res item state]
                 (cond
                   (base/focus? item)
                   (de-focus res item state)

                   (base/local-state? item)
                   (de-local-state res item)

                   (base/handle-action? item)
                   (do-handle-actions res item state)

                   (base/handle-state-change? item)
                   (do-handle-state-change res item state)
                   
                   (or (base/refer? item)
                       (base/handle-message? item)
                       (base/named? item)
                       (base/handle-error? item)
                       (base/keyed? item))
                   res
                   
                   :else (throw (unknown-item-error item))))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async? item))
                   res
                          
                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 (throw (unknown-item-error item)))
               
               item
               state))

(defn init
  "Returns what happens when the given item is initialized in the
  given state, which happens when it is first used in an item tree, or
  if it is updated to a new state. Returns a [[core/return]] value."
  [item state]
  (run-lifecycle item state
                 (fn [it state]
                   (if-let [h (base/lifecycle-init it)]
                     (h state)
                     (c/return)))))

(defn finalize
  "Returns what happens when the given item is finalized in the given
  state, which happens when it is now longer used in an item
  tree. Returns a [[core/return]] value."
  [item state]
  (run-lifecycle item state
                 (fn [it state]
                   (if-let [h (base/lifecycle-finish it)]
                     (h state)
                     (c/return)))))

(defn- r-comp [& fs]
  (apply comp (reverse fs)))

(defn- find-handle-message [item state]
  (reduce-item make-dummy-ref
               (constantly dummy-async)
               (fn leaf [item state]
                 (cond
                   (or (string? item)
                       (base/lifecycle? item)
                       (base/fragment? item)
                       (dom/element? item))
                   nil

                   :else (throw (unknown-item-error item))))
               (fn container [c-res item state]
                 (cond
                   ;; containers don't pass messages; so we don't care.
                   (or (nil? item)
                       (base/fragment? item)
                       (dom/element? item))
                   nil

                   :else (throw (unknown-item-error item))))
               (fn wrapper [res item state]
                 (cond
                   (base/handle-message? item)
                   {:f (base/handle-message-f item)
                    :state state
                    :post identity}

                   (base/focus? item)
                   (when res
                     (update res :post r-comp #(de-focus % item state)))

                   (base/local-state? item)
                   (when res
                     (update res :post r-comp #(de-local-state % item)))

                   (base/handle-action? item)
                   (when res
                     (update res :post r-comp #(do-handle-actions % item state)))

                   (base/handle-state-change? item)
                   (when res
                     (update res :post r-comp #(do-handle-state-change % item state)))
                   
                   (or (base/refer? item)
                       (base/named? item)
                       (base/handle-error? item)
                       (base/keyed? item))
                   res
                   
                   :else (throw (unknown-item-error item))))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async? item)
                       (base/static? item))
                   res
                          
                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 (throw (unknown-item-error item)))
               
               item
               state))

(defn handle-message
  "Returns what happens when the given message would be sent to that
  item in the given state. Returns a [[core/return]] value or nil, if
  the message would not be handled at all."
  [item state msg]
  ;; Note: will not recur, e.g. when the message handler returns a new
  ;; :message... (forward-messages for example); to make that work, we
  ;; need to remove explicit ref object from the api, and only allow
  ;; refer-items as the message targets.

  ;; find applicable handle-message:
  (if-let [{f :f state :state post :post} (find-handle-message item state)]
    (post (f state msg))
    ;; message won't be processed:
    nil))
