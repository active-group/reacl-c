(ns reacl-c.main
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [active.clojure.lens :as lens]
            #?(:cljs [reacl2.core :as reacl :include-macros true])
            #?(:cljs [reacl-c.impl.reacl :as impl])))

(let [h (fn [_ eff]
          (loop [effs (list eff)
                 res (core/return)
                 n 0]
            (cond
              (empty? effs) res
              ;; bail out if an effects keeps on returning new effects. (TODO: make configurable)
              (> n 1000) (throw (ex-info "Maximum recursion limit exceeded in handling effects." {}))
              :else
              (let [[_ ret] (base/run-effect! eff)
                    {more-effs true more-acts false} (group-by base/effect? (base/returned-actions ret))
                    msgs (base/returned-messages ret)]
                (recur (concat (rest effs) more-effs)
                       (base/merge-returned res (base/make-returned base/keep-state more-acts msgs))
                       (inc n))))))]
  (defn execute-effects [item] ;; + optional monitor for basic effects?
    (core/handle-effect item h)))

(defn- state-error [_]
  ;; TODO or a warning?
  (throw (ex-info "Unhandled state change at toplevel." {})))

(defn- action-error [_]
  ;; TODO: warning is enough (utils/warn "Unhandled action:" action)
  (throw (ex-info "Unhandled action at toplevel." {})))

#?(:cljs
   (defn run
     "Runs the given item as an application underneath the given
  native `dom` node, and with the given `initial-state`."
     [dom item initial-state]
     (impl/run dom
       (core/local-state initial-state (core/focus lens/second (execute-effects item)))
       nil
       state-error
       action-error)))

;; TODO: add a controlled run?

(defn send-message!
  "Sends a message to a running application, i.e. `app` must be the
  value returned from [[run]] for example. This can be
  used together with [[handle-message]] in situations where the
  application is not running standalone, but integrated in a different
  framework."
  [app msg]
  {:pre [(satisfies? base/Application app)]}
  (base/-send-message! app msg))

#?(:cljs
   ;; previously named reacl-render
   (defn reacl
     "Returns a Reacl component running the given item with the given
  Reacl `state binding`.

  Messages sent to the returned component are passed to the given
  item. Actions emitted from the item are emitted from the returned
  component. Note that this includes effect actions. If you want
  effects to be executed instead, use [[execute-effects]]"
     [binding item]
     ;; TODO: make it independant from impl.
     (impl/instantiate binding item)))

#_(:cljs
   (defn react-uncontrolled
     "Returns a React element running the given item"
     [item initial-state]
     ;; TODO: also offer controlled-react?
     ;; TODO: how to send messages to it?
     ;; TODO: don't use reacl
     (reacl (reacl/use-app-state nil)
            (-> (core/local-state initial-state
                                  (core/focus lens/second item))
                (execute-effects)))))

#_(defn react-controlled
  [item state onChange onAction]
  )

;; webcomponent(hybrids.js?), om, reagent, etc.
;; hmtl/hiccup ?
