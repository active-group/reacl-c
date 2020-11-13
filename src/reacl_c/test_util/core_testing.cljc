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

      (nil? item)
      (f-leaf item state)

      (base/lifecycle? item)
      (f-leaf item state)

      (base/dynamic? item)
      (f-dynamic (rec state (apply (base/dynamic-f item) state (base/dynamic-args item))) item state)

      (base/fragment? item)
      (f-container (reduce (fn [r c]
                             (conj r (rec state c)))
                           []
                           (base/fragment-children item))
                   item state)

      (dom/element? item)
      (f-container (reduce (fn [r c]
                             (conj r (rec state c)))
                           []
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

      #_(base/lift-react? item)
      #_item

      :else
      (f-other item state))))

(defn resolve [item state]
  (reduce-item (constantly :dummy-ref) ;; TODO: better dummies?
               (constantly :dummy-return!)
               (fn leaf [item state]
                 ;; TODO: cond types explicitly
                 item)
               (fn container [c-res item state]
                 (cond
                   (base/fragment? item) (lens/shove item base/fragment-children (vec c-res))
                   (dom/element? item) (lens/shove item dom/element-children (vec c-res))
                   :else (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item}))))
               (fn wrapper [res item state]
                 ;; TODO: cond types explicitly
                 (assert (contains? item :e))
                 (assoc item :e res))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async-return? item))
                   res

                   :else
                   (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item}))))
               (fn other [item state]
                 ;; -> or an IResolveable?
                 (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item})))
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

(defn- lift-returned [r]
  ;; TODO: move all impls of it to one place?!
  (if (base/returned? r) r (c/return :state r)))

(defn- merge-returned [r1 r2]
  ;; TODO: need to lift plain states into (c/return) / or should merge-return do that? (at least define it in only one place accross all impls)
  (base/merge-returned r1 (lift-returned r2)))

(defn init [item state] ;; TODO: or call mount? reuse for finish and message. (this is almost a 'run' fn)
  ;; TODO: what about handling messages? does that make sense?
  ;; resolve, find all livecycle (with their state); call that.
  (reduce-item (constantly :dummy-ref)
               (constantly :dummy-return!)
               (fn leaf [item state]
                 (cond
                   (base/lifecycle? item)
                   (lift-returned ((base/lifecycle-init item) state))

                   :else ;; TODO: list all
                   (c/return)))
               (fn container [c-res item state]
                 (reduce merge-returned (c/return) c-res))
               (fn wrapper [c-res item state]
                 (cond
                   (base/focus? item)
                   ;; merge changed state in subitem back into parent state:
                   (if (not= base/keep-state (base/returned-state c-res))
                     (let [st (base/returned-state c-res)]
                       (lens/shove c-res base/returned-state
                                   (lens/shove state (base/focus-lens item) st)))
                     c-res)

                   (base/local-state? item)
                   ;; remove local part of changed state:
                   (if (not= base/keep-state (base/returned-state c-res))
                     (lens/overhaul c-res base/returned-state first)
                     c-res)

                   (base/handle-action? item)
                   ;; do handle actions
                   (let [f (base/handle-action-f item)
                         {handle true pass false} (group-by (base/handle-action-pred item)
                                                            (base/returned-actions c-res))]
                     (reduce (fn [res a]
                               (let [state (if (not= base/keep-state (base/returned-state res))
                                             (base/returned-state res)
                                             state)]
                                 (merge-returned res
                                                 (f state a))))
                             (lens/shove c-res base/returned-actions (vec pass))
                             handle))

                   :else ;; TODO: list all explicitly.
                   c-res))
               (fn dynamic [res item state]
                 (cond
                   (or (base/dynamic? item)
                       (base/with-ref? item)
                       (base/with-async-return? item))
                   res
                          
                   :else
                   (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item}))))
               (fn other [item state]
                 (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item})))
               
               item
               state))

(defn finish [item state])

(defn message [item state msg]
  ;; find first handle-message, lensing state though and back, calling handler on msg
  
  )

;; (defn run-effect [eff])
;; (defn run-subscription [for-a-while])


