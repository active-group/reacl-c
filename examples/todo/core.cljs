(ns ^:no-doc todo.core
    (:require [reacl-c.core :as c :include-macros true]
              [active.clojure.functions :as f]
              [active.clojure.lens :as lens]
              [reacl-c.main :as main]
              [reacl-c.dom :as dom]))

(defrecord Call [id f args])

(def get-unique-id
  (c/handle-effect-result (fn [_ uuid]
                            uuid)
                          (c/effect random-uuid)))

(let [binder (fn [id h & [translate-args]] ;; translate-args -> fn[inner-args]
               (when (some? h)
                 (fn [inner-state & args]
                   (c/return :action (Call. id h (if (some? translate-args) ;; TODO: translation based on inner-state? desireable? (can also use bind in a local handler then if that's equivalent?)
                                                   [(apply translate-args args)] ;; TODO: allow multiple values (with special return type); although handler should usually only have one arg.
                                                   args))))))
      handler (fn [id state a]
                (if (and (instance? Call a)
                         (= id (:id a)))
                  (apply (:f a) state (:args a))
                  (c/return :action a)))]
  (c/defn-item with-bind [f] ;; ... and as a macro? with-bind-as
    (c/with-state-as [_ id :local nil]
      (c/fragment (c/focus lens/second get-unique-id)
                  (when id
                    (c/focus lens/first
                             (-> (f (f/partial binder id))
                                 (c/handle-action (f/partial handler id)))))))))

(defn- lift-return [r]
  (if (c/returned? r) r (c/return :state r)))

(defn- right-state [state ret]
  (if (= c/keep-state (c/returned-state ret))
    state
    (c/returned-state ret)))

(defn merge-handlers [& handlers]
  (when-let [hs (not-empty (remove nil? handlers))]
    (fn [state & args]
      (reduce (fn [ret h]
                (let [ret2 (lift-return (apply h (right-state state ret) args))]
                  (c/merge-returned ret ret2)))
              (c/return)
              hs))))

;; -----

(defn checked-state [_ e]
  (.. e -target -checked))

(c/def-item checkbox
  (c/with-state-as checked
    (dom/input {:type "checkbox"
                :value checked
                :onchange checked-state})))

(defn value-state [_ e]
  (.. e -target -value))

(c/def-item textbox
  (c/with-state-as value
    (dom/input {:type "text"
                :value value
                :onchange value-state})))

;; ----

(c/defn-item add-text [on-submit]
  (with-bind
    (fn [bind]
      (c/with-state-as [st text :local ""]
        (c/focus lens/second
                 (dom/form {:onsubmit (merge-handlers (fn [text e]
                                                        (.preventDefault e)
                                                        (c/return :state ""))
                                                      (bind on-submit (f/constantly text)))}
                           textbox
                           (dom/button {:type "submit"
                                        :disabled (empty? text)}
                                       "Add")))))))
;; ----

(defrecord Todo [id text done?])

(c/defn-item item [on-delete]
  (c/with-state-as todo
    (dom/div (c/focus :done? checkbox)
             (dom/button {:onclick on-delete} "Zap")
             " " (:text todo))))

(c/defn-item item-list [on-delete-item]
  (c/with-state-as todos
    (with-bind
      (fn [bind]
        (apply dom/div
               (map-indexed (fn [idx id]
                              (-> (c/focus (lens/at-index idx)
                                           (item (bind on-delete-item (f/constantly id))))
                                  (c/keyed id)))
                            (map :id todos)))))))

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
  (with-bind (fn [bind]
               (c/fragment (dom/h3 "TODO")
                           (c/focus :todos (item-list (bind delete-item)))
              
                           (dom/br)
                           (add-text (bind add-item))))))

(main/run (.getElementById js/document "content")
  main
  {:initial-state (TodosApp. 1 [(Todo. 0 "Example" false)])})

