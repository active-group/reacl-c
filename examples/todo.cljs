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

(c/defn-dynamic add-item text [submit]
  ;; TODO: static fns.
  (dom/form {:onsubmit (fn [e]
                         (.preventDefault e)
                         (c/return :state ""
                                   :action (submit text)))}
            textbox
            (dom/button {:type "submit"} "Add")))

(defn add-item-form [submit]
  (c/isolate-state "" (add-item submit)))

(defn button [label action]
  ;; TODO: static fns.
  (dom/button {:onclick (constantly (c/return :action action))}
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

(defn list-actions [action state]
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
      (c/handle-action list-actions state)))

(browser/run (.getElementById js/document "app-todo")
  main
  (TodosApp. 0 [(Todo. -1 "Example" false)]))

