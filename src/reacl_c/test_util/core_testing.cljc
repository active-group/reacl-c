(ns reacl-c.test-util.core-testing
  (:require [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.core :as c]
            [active.clojure.lens :as lens]
            [clojure.string :as string])
  (:refer-clojure :exclude [resolve]))

;; Only pure tests: No local-/state changes, no implicit mounting (init), no effects, no subscriptions.

(defn- reduce-item [f-leaf f-container f-wrapper f-dynamic f-other init item state]
  (let [rec (fn [state init item]
              (reduce-item f-leaf f-container f-wrapper f-dynamic f-other
                           init item state))]
    (cond
      (string? item)
      (f-leaf init item state)

      (nil? item)
      (f-leaf init item state)

      (base/lifecycle? item)
      (f-leaf init item state)

      (base/dynamic? item)
      (f-dynamic init item state)

      (base/fragment? item)
      (f-container (reduce (partial rec state)
                           init
                           (base/fragment-children item))
                   item state)

      (dom/element? item)
      (f-container (reduce (partial rec state)
                           init
                           (dom/element-children item))
                   item state)

      (base/static? item)
      (f-dynamic init item state)

      (base/with-ref? item)
      (f-dynamic init item state)

      (base/with-async-return? item)
      (f-dynamic init item state)

      (base/focus? item)
      (f-wrapper (rec (lens/yank state (base/focus-lens item))
                      init (base/focus-e item))
                 item state)

      (base/local-state? item) ;; TODO: implement special local-state initializers
      (f-wrapper (rec [state (base/local-state-initial item)]
                      init
                      (base/local-state-e item))
                 item state)

      (base/handle-action? item)
      (f-wrapper (rec state init (base/handle-action-e item))
                 item state)

      (base/refer? item)
      (f-wrapper (rec state init (base/refer-e item))
                 item state)

      (base/handle-state-change? item)
      (f-wrapper (rec state init (base/handle-state-change-e item))
                 item state)

      (base/handle-message? item)
      (f-wrapper (rec state init (base/handle-message-e item))
                 item state)

      (base/named? item)
      (f-wrapper (rec state init (base/named-e item))
                 item state)

      (base/handle-error? item)
      (f-wrapper (rec state init (base/handle-error-e item))
                 item state)

      (base/keyed? item)
      (f-wrapper (rec state init (base/keyed-e item))
                 item state)

      #_(base/lift-reacl? item)
      #_item

      :else
      (f-other init item state))))

(defn resolve [item state]
  (first (reduce-item (fn leaf [res item state]
                        (conj res item))
                      (fn container [res item state]
                        (cond
                          (base/fragment? item) [(lens/shove item base/fragment-children (vec res))]
                          (dom/element? item) [(lens/shove item dom/element-children (vec res))]
                          :else (assert false)))
                      (fn wrapper [res item state]
                        ;; TODO: explicit
                        (assert (contains? item :e))
                        (conj res (assoc item :e (first res))))
                      (fn dynamic [res item state]
                        (cond
                          (base/dynamic? item)
                          (conj res (resolve (apply (base/dynamic-f item) state (base/dynamic-args item))
                                             state))
                          
                          (base/with-ref? item)
                          (conj res (resolve (apply (base/with-ref-f item) :dummy-ref (base/with-ref-args item))
                                             state))
                          
                          (base/with-async-return? item)
                          (conj res (resolve (apply (base/with-async-return-f item) :dummy-return! (base/with-async-return-args item))
                                             state))
                          
                          :else
                          (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item}))))
                      (fn other [res item state]
                        ;; -> or an IResolveable?
                        (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item})))
                      []
                      item
                      state)))


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

(defn- merge-returned [r1 r2]
  ;; TODO: need to lift plain states into (c/return) / or should merge-return do that? (at least define it in only one place accross all impls)
  (let [r2 (if (base/returned? r2) r2 (c/return :state r2))]
    (base/merge-returned r1 r2)))

(defn init [item state] ;; TODO: or call mount?
  ;; resolve, find all livecycle (with their state); call that.
  (reduce-item (fn leaf [res item state]
                 (cond
                   (base/lifecycle? item)
                   ;; FIXME: would need to untangle returned states in 'focus'
                   (merge-returned res ((base/lifecycle-init item) state))

                   :else
                   res))
               (fn container [res item state]
                 res)
               (fn wrapper [res item state]
                 (cond
                   (base/focus? item)
                   (if (not= base/keep-state (base/returned-state res))
                     (let [st (base/returned-state res)]
                       (lens/shove res base/returned-state
                                   (lens/shove state (base/focus-lens item) st)))
                     res)

                   (base/local-state? item)
                   (if (not= base/keep-state (base/returned-state res))
                     (lens/overhaul res base/returned-state first)
                     res)

                   (base/handle-action? item)
                   (let [f (base/handle-action-f item)
                         {handle true pass false} (group-by (base/handle-action-pred item)
                                                            (base/returned-actions res))]
                     (reduce (fn [res a]
                               (let [state (if (not= base/keep-state (base/returned-state res))
                                             (base/returned-state res)
                                             state)]
                                 (merge-returned res
                                                 (f state a))))
                             (lens/shove res base/returned-actions pass)
                             handle))

                   :else
                   res))
               (fn dynamic [res item state]
                 ;; TODO: why this? (and same in resolve?) can't sub result be done in reduce-item?
                 (cond
                   (base/dynamic? item)
                   (merge-returned res (init (apply (base/dynamic-f item) state (base/dynamic-args item))
                                             state))
                          
                   (base/with-ref? item)
                   (merge-returned res (init (apply (base/with-ref-f item) :dummy-ref (base/with-ref-args item))
                                             state))
                          
                   (base/with-async-return? item)
                   (merge-returned res (init (apply (base/with-async-return-f item) :dummy-return! (base/with-async-return-args item))
                                             state))
                          
                   :else
                   (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item}))))
               (fn other [res item state]
                 (throw (ex-info (str "Unknown item: " (pr-str item)) {:item item})))
               
               (c/return)
               item
               state)
  )

(defn finish [item state])

(defn message [item state msg]
  ;; find first handle-message, lensing state though and back, calling handler on msg
  
  )

;; (defn run-effect [eff])
;; (defn run-subscription [for-a-while])


