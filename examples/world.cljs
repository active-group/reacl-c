(ns ^:no-doc examples.world
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.browser :as browser]
            [reacl-c.dom :as dom]))

(c/defn-subscription interval-timer deliver! [ms]
  {:pre [(integer? ms)]}
  (println "starting timer!")
  (let [id (.setInterval js/window (fn [] (deliver! (js/Date.))) ms)]
    (fn stop []
      (println "stopping timer!")
      (.clearInterval js/window id))))

(c/def-dynamic show-date date
  (.toLocaleTimeString date))

(c/defn-interactive show-error state set-state [reset-value]
  ;; change to (deliver! nil) to see try-catch in action.
  (dom/div "An error occurred: " (pr-str (second state))
           "with the state being: " (pr-str (first state))
           " "
           (dom/button {:onclick (constantly (set-state [reset-value nil]))} "Reset")))

(def clock
  (c/isolate-state (js/Date.)
                   (c/try-catch (dom/div (-> (interval-timer 1000)
                                             (c/handle-action (fn [state date] (c/return :state date))))
                                         show-date)
                                (show-error (js/Date.)))))

(defrecord Effect [f args])

(defn reload [force?]
  (c/return :action (Effect. #(.reload (.-location js/window) force?) nil)))

(defn effects [_ {f :f args :args}]
  (apply f args)
  (c/return))

(defrecord Show [_])
(defrecord Hide [_])

(defn show-hide [_ a]
  (cond
    (instance? Show a) (c/return :state true)
    (instance? Hide a) (c/return :state false)
    :else a))

(c/def-dynamic world-app show?
  (-> (if show?
        (dom/div (dom/button {:onclick ->Hide} "Hide")
                 clock
                 (dom/button {:onclick (constantly (reload true))} "Reload"))
        (dom/button {:onclick ->Show} "Show"))
      (c/handle-action show-hide)
      (c/handle-action effects)))

(browser/run (.getElementById js/document "app-world")
  world-app
  true)
