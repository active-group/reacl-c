(ns examples.world
  (:require [reacld.core :as r :include-macros true]
            [reacld.browser :as browser]
            [reacld.dom :as dom]))

(r/defn-subscription interval-timer deliver! [ms]
  (println "starting timer!")
  (let [id (.setInterval js/window (fn [] (deliver! (js/Date.))) ms)]
    (fn stop []
      (println "stopping timer!")
      (.clearInterval js/window id))))

(r/def-dynamic show-date date
  (.toLocaleTimeString date))

(def clock
  (r/isolate-state (js/Date.)
                   (dom/div (-> (interval-timer 1000)
                                (r/handle-actions (fn [state date] date)))
                            show-date)))

(r/defn-effect reload [force?]
  (.reload (.-location js/window) force?))

(defrecord Show [_])
(defrecord Hide [_])

(defn show-hide [_ a]
  (cond
    (instance? Show a) true
    (instance? Hide a) false
    :else a))

(r/def-dynamic world-app show?
  (-> (if show?
        (dom/div (dom/button {:onclick ->Hide} "Hide")
                 clock
                 (dom/button {:onclick (constantly (reload true))} "Reload"))
        (dom/button {:onclick ->Show} "Show"))
      (r/handle-actions show-hide)))

(browser/run (.getElementById js/document "app-world")
  world-app
  true)
