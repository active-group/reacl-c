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

(c/defn-effect now! []
  (js/Date.))

(defn show-date [date]
  (dom/div (if date (.toLocaleTimeString date) "")))

(defn- set-date [_ date]
  (c/return :state date))

(c/def clock
  (c/isolate-state nil
                   (c/fragment
                    (c/handle-effect-result set-date (now!))
                    (-> (interval-timer 1000)
                        (c/handle-action set-date))
                    (c/dynamic show-date))))

(defn hide [_ _] (c/return :state false))
(defn show [_ _] (c/return :state true))

(c/def world-app
  (c/with-state-as show?
    (if show?
      (dom/div (dom/button {:onclick hide} "Hide")
               clock)
      (dom/button {:onclick show} "Show"))))

(browser/run (.getElementById js/document "app-world")
  world-app
  true)
