(ns reacl-c.test-util.core-testing
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.core :as c]
            [active.clojure.lens :as lens]
            [clojure.string :as string])
  (:refer-clojure :exclude [resolve]))

;; Only pure tests: No local-/state changes, no implicit mounting (init), no effects, no subscriptions.

(defn- reduce-item [mk-ref mk-async-return f-leaf f-container f-wrapper f-dynamic f-other item state]
  (let [rec (fn [state item]
              (reduce-item mk-ref mk-async-return f-leaf f-container f-wrapper f-dynamic f-other
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

      (base/with-async-return? item)
      (f-dynamic (rec state (apply (base/with-async-return-f item) (mk-async-return) (base/with-async-return-args item))) item state)

      (base/focus? item)
      (f-wrapper (rec (lens/yank state (base/focus-lens item))
                      (base/focus-e item))
                 item state)

      (base/local-state? item) ;; TODO: implement special local-state initializers
      (f-wrapper (rec [state (base/local-state-initial item)]
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

(defn resolve [item state]
  (reduce-item (constantly :dummy-ref) ;; TODO: better dummies?
               (constantly :dummy-return!)
               (fn leaf [item state]
                 (cond
                   (or (string? item)
                       (base/fragment? item)
                       (dom/element? item)
                       (base/lifecycle? item))
                   item
                   :else (throw (unknown-item-error item))))
               (fn container [c-res item state]
                 (cond
                   (nil? item) item
                   (base/fragment? item) (lens/shove item base/fragment-children (vec c-res))
                   (dom/element? item) (lens/shove item dom/element-children (vec c-res))
                   :else (throw (unknown-item-error item))))
               (fn wrapper [res item state]
                 (lens/shove item
                             (cond
                               (base/focus? item) base/focus-e
                               (base/local-state? item) base/local-state-e
                               (base/handle-action? item) base/handle-action-e
                               (base/refer? item) base/refer-e
                               (base/handle-state-change? item) base/handle-state-change-e
                               (base/handle-message? item) base/handle-message-e
                               (base/named? item) base/named-e
                               (base/handle-error? item) base/handle-error-e
                               (base/keyed? item) base/keyed-e
                               :else (throw (unknown-item-error item)))
                             res))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async-return? item))
                   res

                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 ;; -> or an IResolveable extension point?
                 (throw (unknown-item-error item)))
               item
               state))


#_(defn- contains-0? [item sub-item]
  (loop [item item]
    (or (= item sub-item)
        (cond
          (string? item)
          (and (string? sub-item)
               (string/includes? item sub-item))

          (nil? item)
          (= item sub-item)
    
          (base/fragment? item)
          (reduce #(or %1 %2)
                  false
                  (map #(contains? state % sub-item) (base/fragment-children item)))

          (dom/element? item)
          (reduce #(or %1 %2)
                  false
                  (map #(contains? state % sub-item) (dom/element-children item)))

          (base/focus? item)
          (recur (base/focus-e item))

          (base/local-state? item)
          (recur (base/local-state-e item))

          (base/handle-action? item)
          (recur (base/handle-action-e item))

          (base/refer? item)
          (recur (base/refer-e item))

          (base/handle-state-change? item)
          (recur (base/handle-state-change-e item))

          (base/handle-message? item)
          (recur (base/handle-message-e item))

          (base/named? item)
          (recur (base/named-e item))

          (base/handle-error? item)
          (recur (base/handle-error-e item))

          (base/keyed? item)
          (recur (base/keyed-e item))

          :else
          false))))

;; TODO: could/should some of these fns be shared with the 'real' implementations?

(defn- lift-returned [r]
  ;; TODO: move all impls of it to one place?!
  (if (base/returned? r) r (c/return :state r)))

(defn- merge-returned [r1 r2]
  ;; TODO: need to lift plain states into (c/return) / or should merge-return do that? (at least define it in only one place accross all impls)
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
  (reduce-item (constantly :dummy-ref)
               (constantly :dummy-return!)
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
                       (base/with-async-return? item))
                   res
                          
                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 (throw (unknown-item-error item)))
               
               item
               state))

(defn init [item state]
  (run-lifecycle item state
                 (fn [it state]
                   ((base/lifecycle-init it) state))))

(defn finalize [item state]
  (run-lifecycle item state
                 (fn [it state]
                   ((base/lifecycle-finish it) state))))

(defn- r-comp [& fs]
  (apply comp (reverse fs)))

(defn- find-handle-message [item state]
  (reduce-item (constantly :dummy-ref)
               (constantly :dummy-return!)
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
                       (base/with-async-return? item))
                   res
                          
                   :else
                   (throw (unknown-item-error item))))
               (fn other [item state]
                 (throw (unknown-item-error item)))
               
               item
               state))

(defn message [item state msg]
  ;; find applicable handle-message
  (if-let [{f :f state :state post :post} (find-handle-message item state)]
    (post (f state msg))
    ;; message won't be processed... throw then?
    nil))

;; (defn run-effect [eff])
;; (defn run-subscription [for-a-while])


