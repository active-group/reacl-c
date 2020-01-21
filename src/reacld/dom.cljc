(ns reacld.dom
  (:require [reacld.base :as base]
            [clojure.string :as str]))

;; TODO: some standard event handlers? constantly, value, checked.

(defrecord ^:no-doc Element [type attrs events children] base/E)

(defn ^:no-doc dom-attributes? [v]
  (and (map? v)
       (not (satisfies? base/E v))))

(defn- analyze-dom-args [args]
  (if (empty? args)
    [{} args]
    (let [x (first args)]
      (if (dom-attributes? x)
        [x (rest args)]
        [{} args]))))

(defn- event? [k]
  (str/starts-with? (name k) "on"))

(defn- split-events [attrs]
  ;; OPT
  [(into {} (remove #(event? (first %)) attrs))
   (into {} (filter #(event? (first %)) attrs))])

(defn- dom [type]
  ;; Note: could also use (with-async-actions (fn [deliver! ])) and event handlers that call deliver! - but then they aren't pure anymore (at least after a translation)
  (fn [& args]
    (let [[attrs_ children] (analyze-dom-args args)
          [attrs events] (split-events attrs_)]
      (Element. type attrs events children))))

(def div (dom "div"))
(def input (dom "input"))
(def form (dom "form"))
(def button (dom "button"))
(def h3 (dom "h3"))
;; TODO: rest of dom
