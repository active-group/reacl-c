(ns reacl-c.main
  "Functions for using reacl-c within a browser application."
  (:require #?(:cljs [reacl-c.core :as core :include-macros true])
            #?(:clj [reacl-c.core :as core])
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            #?(:cljs [reacl-c.impl.react :as impl])))

(let [h (fn [recursion-limit monitor _ eff]
          (loop [effs (list eff)
                 final (core/return) ;; collects (toplevel) actions other than effects and messages.
                 n 0]
            (cond
              (empty? effs) final
              ;; bail out if an effects keeps on returning new effects over and over.
              (> n recursion-limit) (throw (ex-info "Maximum recursion limit exceeded in handling effects." {}))
              :else
              (let [eff (first effs)
                    [res ret] (base/run-effect! eff)
                    {more-effs true more-acts false} (group-by base/effect? (base/returned-actions ret))]
                (when monitor (monitor eff res ret))
                (recur (concat (rest effs) more-effs)
                       (base/merge-returned final (lens/shove ret base/returned-actions more-acts))
                       (inc n))))))]
  (defn execute-effects
    "Returns an item that will intercept and execute all effects
  emitted by the given `item`. This is automatically wrapped around
  the toplevel item when using [[run]], but not when
  using [[run-controlled]].
  
  Options can be a map with the following settings:

  - `:recursion-limit`  When effects continue to return new effects, an exception is thrown when this limit is reached (defaults to 1000),
  - `:monitor`  A function called with the effect, its result value and a [[reacl-c.core/return]] value of actions and messages emitted by that effect."
    [item & [options]]
    (core/handle-effect item (f/partial h
                                        (or (:recursion-limit options) 1000)
                                        (:monitor options)))))

(defn ^:no-doc state-error [st]
  (throw (ex-info (str "Unhandled state change at toplevel: " (pr-str st) ".") {:state st})))

(defn ^:no-doc action-error [a]
  (throw (ex-info (str "Unhandled action at toplevel: " (pr-str a) ".") {:action a})))

#?(:cljs
   (defn root
     "Sets up the given dom node to render item. Returns an object which can
be passed to [[run]]."
     [dom]
     (impl/run dom nil nil state-error action-error)))

#?(:cljs
   (defn run-controlled
     "Runs the given item underneath the given native `dom` node or [[root]].

  Options are:

  `:state`: specifying the state of the item, which defaults to nil.
  
  `set-state!`: a function that should handle a state change of the
  item; if not specified, and the item wants to change its state, an
  error is thrown.

  `handle-action!`: a function called when the item emits an
  action (including effects); if not specified, and the item does emit
  an action, an error is thrown."
     [dom-or-root item & [options]]
     (assert (every? #{:state :set-state! :handle-action!} (keys options)))
     (let [{state :state set-state! :set-state! handle-action! :handle-action!} options]
       (impl/run dom-or-root
         item
         state
         (or set-state! state-error)
         (or handle-action! action-error)))))

#?(:cljs
   (defn run
     "Runs the given item underneath the given native `dom` node or [[root]],
  automatically managing its state and executing effect actions.
  
  Options are:

  `:initial-state`: specifying the initial state of the item, which defaults to nil.

  `:handle-action!`: a function called when the item emits an
  action (excluding effects); if not specified, and the item does emit
  an action, an error is thrown."
     [dom-or-root item & [options]]
     (assert (every? #{:handle-action! :initial-state} (keys options)))
     (let [{initial-state :initial-state} options]
       (run-controlled dom-or-root
                       (-> (core/local-state initial-state (core/focus lens/second item))
                           ;; should be 'toplevel':
                           (execute-effects))
                       ;; Note: the item's state is fully local; so toplevel state will never change
                       (dissoc options :initial-state)))))

#?(:cljs
   (defn flush-sync! "Calls thunk and returns its result, and flushes changes to states
  causing a synchronous rerendering." [thunk]
     (impl/flush-sync! thunk)))

#?(:cljs
   (defn transition! "Calls thunk and returns its result, and changes to states
  made during this call are handled with lower priority." [thunk]
     (impl/transition! thunk)))

(defn stop!
  "Stops the given application (the value returned by [[root]], [[run]]
  or [[run-controlled]], removing all DOM nodes rendered by it."
  [app]
  (base/-stop! app))

(defn send-message!
  "Sends a message to a running item, i.e. `app` must be the value
  returned from [[run]] or [[run-controlled]]. This can be used together
  with [[reacl-c.core/handle-message]] in situations where the
  application is not running standalone, but integrated in a different
  framework."
  [app msg]
  (base/-send-message! app msg))
