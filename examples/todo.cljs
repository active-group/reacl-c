(ns examples.todo
  (:require [reacld.core :as r :include-macros true]
            [reacld.browser :as browser]
            [reacld.dom :as dom]))

(r/def-interactive checkbox checked set-checked
  ;; TODO: make callback static (active.clojure.function?)
  (dom/input {:type "checkbox"
              :value checked
              :onchange (fn [e] (set-checked (.. e -target -checked)))}))

(r/def-interactive textbox value set-value
  ;; TODO: make callback static (active.clojure.function?)
  (dom/input {:type "text"
              :value value
              :onchange (fn [e]
                          (set-value (.. e -target -value)))}))

(defrecord TodosApp [next-id todos])
(defrecord Todo [id text done?])

(defrecord Submit [])
(defrecord Reset [])

(r/defn-dynamic add-item-form state [add-item]
  ;; TODO: Make fns static
  (-> (dom/form {:onsubmit (fn [e]
                             (.preventDefault e)
                             (->Submit))}
                (-> textbox (r/focus :new-text))
                (dom/button {:type "submit"} "Add"))
      (r/map-actions (fn [a]
                       (cond
                         (instance? Submit a)
                         (r/multi-action (add-item (:new-text state))
                                         (->Reset))
                        
                         :else a)))
      (r/handle-actions (fn [state a]
                          (cond
                            (instance? Reset a)
                            (assoc state :new-text "")

                            :else r/pass-action)))))

(defn add-item [add-item]
  (-> (add-item-form add-item)
      (r/hide-merged-state {:new-text ""})))

(defn button [label action]
  ;; TODO: make callback static (active.clojure.function?)
  (dom/button {:onclick (constantly action)}
              label))

(r/defn-dynamic item todo [delete]
  (dom/div (-> checkbox
               (r/focus :done?))
           (button "Zap" delete)
           " " (:text todo)))

(r/defn-dynamic item-list todos [delete-item]
  (apply dom/div
         (map-indexed (fn [idx id]
                        (-> (item (delete-item id))
                            (r/focus idx)
                            (r/keyed id)))
                      (map :id todos))))

(defrecord DeleteItem [id])
(defrecord AddItem [text])

(def todo-app
  (-> (dom/div {}
               (dom/h3 "TODO")
               (-> (item-list ->DeleteItem)
                   (r/focus :todos))
               (add-item ->AddItem))
      (r/handle-actions (fn [state action]
                          (condp instance? action
                            DeleteItem
                            (-> state
                                (update :todos #(vec (remove (fn [todo] (= (:id action) (:id todo)))
                                                             %))))
                            AddItem
                            (-> state
                                (update :todos conj (->Todo (:next-id state) (:text action) false))
                                (update :next-id inc))
                            
                            r/pass-action)))
      (r/monitor-state (fn [old new]
                         (js/console.log old "=>" new)
                         r/no-action))))

(browser/run (.getElementById js/document "app-todo")
  todo-app
  (TodosApp. 0 [(Todo. -1 "test" false)]))
