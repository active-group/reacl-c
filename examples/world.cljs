(ns ^:no-doc examples.world
    (:require [reacl-c.core :as c :include-macros true]
              [active.clojure.functions :as f]
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

(c/def-dynamic show-error state
  ;; change to (deliver! nil) to see try-catch in action.
  (dom/div "An error occurred: " (pr-str (second state))
           "with the state being: " (pr-str (first state))
           " "
           (dom/button {:onclick (fn [] (c/return :state [(js/Date.) nil]))} "Reset")))

(def clock
  (c/isolate-state (js/Date.)
                   (c/try-catch (dom/div (-> (interval-timer 1000)
                                             (c/handle-action (fn [_ date]
                                                                (c/return :state date))))
                                         show-date)
                                show-error)))

(c/defn-effect reload []
  (.reload (.-location js/window) true)
  (c/return))

(defn hide [_] (c/return :state false))
(defn show [_] (c/return :state true))

(c/def-dynamic world-app show?
  (if show?
    (dom/div (dom/button {:onclick hide} "Hide")
             clock
             (dom/button {:onclick (f/constantly (c/return :action (reload)))} "Reload"))
    (dom/button {:onclick show} "Show")))

(browser/run (.getElementById js/document "app-world")
  world-app
  true)
