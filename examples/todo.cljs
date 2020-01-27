(ns ^:no-doc examples.todo
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.browser :as browser]
            [reacl-c.dom :as dom]))

(defn checked-state [e]
  (c/return :state (.. e -target -checked)))

(c/def-dynamic checkbox checked
  (dom/input {:type "checkbox"
              :value checked
              :onchange checked-state}))

(defn value-state [e]
  (c/return :state (.. e -target -value)))

(c/def-dynamic textbox value
  (dom/input {:type "text"
              :value value
              :onchange value-state}))

(defrecord TodosApp [next-id todos])
(defrecord Todo [id text done?])

(defn add-item-submit [act e]
  (.preventDefault e)
  (c/return :state ""
            :action act))

(c/defn-dynamic add-item text [submit]
  (dom/form {:onsubmit (c/partial add-item-submit (submit text))}
            textbox
            (dom/button {:type "submit"} "Add")))

(defn add-item-form [submit]
  (c/isolate-state "" (add-item submit)))

(defn button-action [action ev]
  (c/return :action action))

(defn button [label action]
  (dom/button {:onclick (c/partial button-action action)}
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

(defn list-actions [state action]
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
                            
    (c/return :action action)))

(c/def-dynamic main state
  (-> (c/fragment (dom/h3 "TODO")
                  (-> (item-list ->DeleteItem)
                      (c/focus :todos))
                  (dom/br)
                  (add-item-form ->AddItem))
      (c/handle-action (c/partial list-actions state))))

(browser/run (.getElementById js/document "app-todo")
  main
  (TodosApp. 0 [(Todo. -1 "Example" false)]))

