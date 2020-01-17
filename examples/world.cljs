(ns examples.world
  (:require [reacld.core :as r :include-macros true]))

(r/defn-subscription interval-timer deliver! [ms]
  (let [id (.setInterval js/window (fn [] (deliver! (js/Date.))) ms)]
    (fn stop [] (.clearInterval js/window id))))

(r/def-dynamic show-date date
  (.toLocaleTimeString date))

(def clock
  (r/isolate-state (js/Date.)
                   (r/div (-> (interval-timer 1000)
                              (r/handle-actions (fn [state date] date)))
                          show-date)))

(r/defn-effect reload [force?]
  (.reload (.-location js/window) force?))

(r/run (.getElementById js/document "app-world")
  (r/div clock (r/button {:onclick (constantly (reload true))} "Reload"))
  nil)
