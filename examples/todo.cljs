(ns ^:no-doc examples.todo
    (:require [reacl-c.core :as c :include-macros true]
              [active.clojure.functions :as f]
              [active.clojure.lens :as lens]
              [reacl-c.browser :as browser]
              [reacl-c.dom :as dom]))

(defn checked-state [_ e]
  (.. e -target -checked))

(c/def checkbox
  (c/with-state-as checked
    (dom/input {:type "checkbox"
                :value checked
                :onchange checked-state})))

(defn value-state [_ e]
  (.. e -target -value))

(c/def textbox
  (c/with-state-as value
    (dom/input {:type "text"
                :value value
                :onchange value-state})))

(defrecord TodosApp [next-id todos])
(defrecord Todo [id text done?])

(defn add-item-submit [act text e]
  (.preventDefault e)
  (c/return :state ""
            :action (act text)))

(c/defn add-item [submit]
  (dom/form {:onsubmit (f/partial add-item-submit submit)}
            textbox
            (dom/button {:type "submit"} "Add")))

(defn add-item-form [submit]
  (c/isolate-state "" (add-item submit)))

(defn button [label action]
  (dom/button {:onclick (f/constantly (c/return :action action))}
              label))

(c/defn item [delete]
  (c/with-state-as todo
    (dom/div (c/focus :done? checkbox)
             (button "Zap" delete)
             " " (:text todo))))

(c/defn item-list [delete-item]
  (c/with-state-as todos
    (apply dom/div
           (map-indexed (fn [idx id]
                          (-> (c/focus (lens/at-index idx) (item (delete-item id)))
                              (c/keyed id)))
                        (map :id todos)))))

(defrecord DeleteItem [id])
(defrecord AddItem [text])

(defn list-actions [state action]
  (condp instance? action
    AddItem
    (-> state
        (update :todos conj (->Todo (:next-id state) (:text action) false))
        (update :next-id inc))
                            
    DeleteItem
    (-> state
        (update :todos #(vec (remove (fn [todo] (= (:id action) (:id todo)))
                                     %))))
                            
    (c/return :action action)))

(c/def main
  (-> (c/fragment (dom/h3 "TODO")
                  (c/focus :todos (item-list ->DeleteItem))
                  (dom/br)
                  (add-item-form ->AddItem))
      (c/handle-action list-actions)))

(browser/run (.getElementById js/document "app-todo")
  main
  (TodosApp. 0 [(Todo. -1 "Example" false)]))

