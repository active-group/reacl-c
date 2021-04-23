(ns ^:no-doc todo.core
    (:require [reacl-c.core :as c :include-macros true]
              [active.clojure.functions :as f]
              [active.clojure.lens :as lens]
              [reacl-c.main :as main]
              [reacl-c.dom :as dom]))

(defn checked-state [_ e]
  (.. e -target -checked))

(c/def-item checkbox
  (c/with-state-as checked
    (dom/input {:type "checkbox"
                :value checked
                :onChange checked-state})))

(defn value-state [_ e]
  (.. e -target -value))

(c/def-item textbox
  (c/with-state-as value
    (dom/input {:type "text"
                :value value
                :onChange value-state})))

;; ----

(c/defn-item add-text [on-submit]
  (c/with-bind
    (fn [bind]
      (let [on-submit (bind on-submit)]
        (c/with-state-as [st text :local ""]
          (c/focus lens/second
                   (dom/form {:onSubmit (fn [text e]
                                          (.preventDefault e)
                                          (c/call on-submit text))}
                             textbox
                             (dom/button {:type "submit"
                                          :disabled (empty? text)}
                                         "Add"))))))))
;; ----

(defrecord Todo [id text done?])

(c/defn-item item [on-delete]
  (c/with-state-as todo
    (dom/div (c/focus :done? checkbox)
             (dom/button {:onClick on-delete} "Zap")
             " " (:text todo))))

(c/defn-item item-list [on-delete-item]
  (c/with-state-as todos
    (c/with-bind
      (fn [bind]
        (let [on-delete-item (bind on-delete-item)]
          (apply dom/div
                 (map-indexed (fn [idx id]
                                (-> (c/focus (lens/at-index idx)
                                             (item (fn [_]
                                                     (c/call on-delete-item id))))
                                    (c/keyed id)))
                              (map :id todos))))))))

;; ----

(defrecord TodosApp [next-id todos])

(defn delete-item [app id]
  (-> app
      (assoc :todos (vec (remove (fn [todo] (= id (:id todo)))
                                 (:todos app))))))

(defn add-item [app text]
  (-> app
      (assoc :todos (conj (:todos app)
                          (->Todo (:next-id app)
                                  text false)))
      (update :next-id inc)))

(c/def-item main
  (c/with-bind (fn [bind]
                 (c/fragment (dom/h3 "TODO")
                             (c/focus :todos (item-list (bind delete-item)))
              
                             (dom/br)
                             (add-text (bind add-item))))))

(main/run (.getElementById js/document "content")
  main
  {:initial-state (TodosApp. 1 [(Todo. 0 "Example" false)])})

