(ns ^:no-doc examples.todo
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.browser :as browser]
            [reacl-c.dom :as dom]))

(c/def-interactive checkbox checked set-checked
  (dom/input {:type "checkbox"
              :value checked
              :onchange (fn [e] (set-checked (.. e -target -checked)))}))

(c/def-interactive textbox value set-value
  (dom/input {:type "text"
              :value value
              :onchange (fn [e]
                          (set-value (.. e -target -value)))}))

(defrecord TodosApp [next-id todos])
(defrecord Todo [id text done?])

(defrecord Submit [])
(defrecord Reset [])

(defn add-item-form [add-item]
  (c/isolate-state ""
                   (-> (dom/form {:onsubmit (fn [e]
                                              (.preventDefault e)
                                              (->Submit))}
                                 textbox
                                 (dom/button {:type "submit"} "Add"))
                       (c/handle-action (fn [state a]
                                          (condp instance? a
                                            Submit (c/return :state ""
                                                             :action (add-item state))
                                            (c/return :action a)))))))

(defn button [label action]
  (dom/button {:onclick (constantly action)}
              label))

(c/defn-dynamic item todo [delete]
  (dom/div (-> checkbox
               (c/focus :done?))
           (button "Zap" delete)
           " " (:text todo)))

(c/defn-dynamic item-list todos [delete-item]
  (apply dom/div
         (map-indexed (fn [idx id]
                        (-> (item (delete-item id))
                            (c/focus idx)
                            (c/keyed id)))
                      (map :id todos))))

(defrecord DeleteItem [id])
(defrecord AddItem [text])

(def main
  (-> (c/fragment (dom/h3 "TODO")
                  (-> (item-list ->DeleteItem)
                      (c/focus :todos))
                  (dom/br)
                  (add-item-form ->AddItem))
      (c/handle-action (fn [state action]
                         (condp instance? action
                           AddItem
                           (c/return :state
                                     (-> state
                                         (update :todos conj (->Todo (:next-id state) (:text action) false))
                                         (update :next-id inc)))
                            
                           DeleteItem
                           (c/return :state
                                     (-> state
                                         (update :todos #(vec (remove (fn [todo] (= (:id action) (:id todo)))
                                                                      %)))))
                            
                           (c/return :action action))))))

(browser/run (.getElementById js/document "app-todo")
  main
  (TodosApp. 0 [(Todo. -1 "Example" false)]))

