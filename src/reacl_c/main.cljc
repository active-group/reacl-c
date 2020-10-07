(ns reacl-c.main
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            #?(:cljs [reacl-c.impl.reacl :as impl])))

;; webcomponent(hybrids.js?), 
;; hmtl/hiccup ?

(let [h (fn [recursion-limit monitor _ eff]
          (loop [effs (list eff)
                 res (core/return)
                 n 0]
            (cond
              (empty? effs) res
              ;; bail out if an effects keeps on returning new effects over and over.
              (> n recursion-limit) (throw (ex-info "Maximum recursion limit exceeded in handling effects." {}))
              :else
              (let [[res ret] (base/run-effect! eff)
                    {more-effs true more-acts false} (group-by base/effect? (base/returned-actions ret))
                    msgs (base/returned-messages ret)]
                (when monitor (monitor eff res ret))
                (recur (concat (rest effs) more-effs)
                       (base/merge-returned res (base/make-returned base/keep-state more-acts msgs))
                       (inc n))))))]
  (defn execute-effects [item & [options]]
    (core/handle-effect item (f/partial h
                                        (or (:recursion-limit options) 1000)
                                        (:monitor options)))))

(defn ^:no-doc state-error [_]
  ;; TODO or a warning?
  (throw (ex-info "Unhandled state change at toplevel." {})))

(defn ^:no-doc action-error [_]
  ;; TODO: warning is enough (utils/warn "Unhandled action:" action)
  (throw (ex-info "Unhandled action at toplevel." {})))

#?(:cljs
   (defn run-controlled
     "Runs the given item as an application underneath the given
  native `dom` node, and with the current state and state changes
  controlled via the given arguments. Actions, including effects are
  passed to the given action handling function."
     [dom item state set-state! handle-action!]
     (impl/run dom
       item
       state
       set-state!
       handle-action!)))

#?(:cljs
   (defn run
     "Runs the given item as an application underneath the given
  native `dom` node, and with the given `initial-state`."
     [dom item initial-state & [handle-action!]]
     (run-controlled dom
                     (core/local-state initial-state (core/focus lens/second (execute-effects item)))
                     ;; the item's state is fully local; to toplevel state can be nil and should never change:
                     nil
                     state-error
                     (or handle-action! action-error))))


(defn send-message!
  "Sends a message to a running item, i.e. `app` must be the value
  returned from [[run]] or the element returned
  by [[main.react.run-controlled]] for example. This can be used
  together with [[handle-message]] in situations where the application
  is not running standalone, but integrated in a different
  framework. The optional callback is invoked when any update
  triggered by the message is completed."
  [app msg & [callback]]
  #?(:clj (do (assert (satisfies? base/Application app) app)
              (base/-send-message! app msg callback)))
  #?(:cljs (if (satisfies? base/Application app)
             (base/-send-message! app msg callback)
             (impl/react-send-message! app msg callback))))
