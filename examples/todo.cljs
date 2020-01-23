(ns ^:no-doc examples.todo
  (:require [reacld.core :as r :include-macros true]
            [reacld.browser :as browser]
            [reacld.dom :as dom]))

(r/def-interactive checkbox checked set-checked
  (dom/input {:type "checkbox"
              :value checked
              :onchange (fn [e] (set-checked (.. e -target -checked)))}))

(r/def-interactive textbox value set-value
  (dom/input {:type "text"
              :value value
              :onchange (fn [e]
                          (set-value (.. e -target -value)))}))

(defrecord TodosApp [next-id todos])
(defrecord Todo [id text done?])

(defrecord Submit [])
(defrecord Reset [])

(defn add-item-form [add-item]
  (r/isolate-state ""
                   (-> (dom/form {:onsubmit (fn [e]
                                              (.preventDefault e)
                                              (->Submit))}
                                 textbox
                                 (dom/button {:type "submit"} "Add"))
                       (r/handle-action (fn [state a]
                                          (condp instance? a
                                            Submit (r/return :state ""
                                                             :action (add-item state))
                                            (r/return :action a)))))))

(defn button [label action]
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

(def main
  (-> (r/fragment (dom/h3 "TODO")
                  (-> (item-list ->DeleteItem)
                      (r/focus :todos))
                  (dom/br)
                  (add-item-form ->AddItem))
      (r/handle-action (fn [state action]
                         (condp instance? action
                           AddItem
                           (r/return :state
                                     (-> state
                                         (update :todos conj (->Todo (:next-id state) (:text action) false))
                                         (update :next-id inc)))
                            
                           DeleteItem
                           (r/return :state
                                     (-> state
                                         (update :todos #(vec (remove (fn [todo] (= (:id action) (:id todo)))
                                                                      %)))))
                            
                           (r/return :action action))))))

(browser/run (.getElementById js/document "app-todo")
  main
  (TodosApp. 0 [(Todo. -1 "Example" false)]))

