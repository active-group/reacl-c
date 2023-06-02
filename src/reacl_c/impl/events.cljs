(ns reacl-c.impl.events
  (:require [clojure.data :as data]
            [clojure.string :as str]
            [active.clojure.functions :as f]))

(defn- bind-handler-fn [f]
  ;; this is to allow g to be an IFn, when a Fn is needed.
  (fn [ev]
    (f ev)))

(defn init-state! [state]
  (aset state "bound_handlers" {})
  (aset state "previous_events" nil))

(defn update-bound-event-handlers
  "Returns an update to state, or nil."
  [events call-handler! state]
  (let [events (into {} (remove (comp nil? second) events)) ;; nil is like non existant (OPT: don't create new map for that?)
        [removed new unchanged] (data/diff (aget state "previous_events")
                                           events)]
    (if (or (not-empty new)
            (not-empty removed))
      ;; Note: keep bound fn as stable as possible
      (let [previous-bound (aget state "bound_handlers")
            new-bound  (as-> (transient {}) $
                         ;; for unchanged event handlers, reuse the identical bound fn
                         (reduce-kv (fn [m k _]
                                      (assoc! m k (get previous-bound k)))
                                    $
                                    unchanged)
                         ;; for changed or new event handlers, generate a new fn
                         (reduce-kv (fn [m name handler]
                                      (assoc! m name (bind-handler-fn (f/partial call-handler! handler))))
                                    $
                                    new)
                         (persistent! $))]
        (assert (= (set (keys new-bound)) (set (keys events)))
                (str "bound handlers and events don't match: " (set (keys new-bound)) " " (set (keys events))))
        
        #js {"bound_handlers" new-bound
             "previous_events" events})
      nil)))

(defn get-bound-event-handlers [state]
  (aget state "bound_handlers"))

(defn custom-event-name [n]
  ;; :onFooBar => "fooBar"
  ;; TODO: we used to do all-lowercase here, but although event names should be all lowercase, they are case-sensitive and can be mixed.
  (let [s (name n)]
    #_(str (str/lower-case (subs s 2 3)) (subs s 3))
    (str/lower-case (subs s 2))))

(defn update-custom-event-listeners! [^js/Element elem prev-handlers new-handlers]
  ;; TODO: ...Capture events?
  
  ;; Note: handlers must be Fns - i.e. bound
  (let [[removed new unchanged] (data/diff prev-handlers new-handlers)]
    (doseq [[name handler] removed]
      (.removeEventListener elem (custom-event-name name) handler))
    (doseq [[name handler] new]
      (.addEventListener elem (custom-event-name name) handler))))
