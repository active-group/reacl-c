(ns reacl-c.interop.reacl
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.interop.react :as react]
            [reacl-c.impl.react0 :as r0 :include-macros true]
            [reacl2.core :as r]
            [active.clojure.lens :as lens]))

(defn- reacl-wrapper [props]
  ;; a React function component
  (let [[class args state return! ref] (r0/extract-args props)]
    (r/react-element class (cond-> {:args args
                                    :handle-action! (fn [a]
                                                      (return! (c/return :action a)))
                                    :ref ref}
                             (r/has-app-state? class)
                             (merge {:app-state state
                                     :set-app-state! (fn [st cb]
                                                       (when-not (r/keep-state? st)
                                                         (return! (c/return :state st)))
                                                       ;; TODO: add callback to return! ?
                                                       (when cb (cb)))})))))

(c/defn-effect ^:private make-ref! []
  #js {:current nil})

(c/defn-effect ^:private send-message! [ref msg]
  (r/send-message! (.-current ref) msg))

(let [ar (fn [return! state class args ref]
           (react/lift* reacl-wrapper (r0/mk-props [class args state return! ref])
                        (r/has-app-state? class)))
      d (fn [[state ref] class args]
          (when (some? ref) ;; ...before the effect is executed.
            (c/focus lens/first
                     (c/with-async-return ar state class args ref))))
      
      hm (fn [[state ref] msg]
           (c/return :action (send-message! ref msg)))
      set-ref #(assoc %1 1 %2)]
  (defn lift
    "Returns an item implemented by the given Reacl class and
  arguments. Messages sent to the item are forwarded to the component,
  actions emitted are passed though, and the app-state of the
  component can be bound to the state of the item."
    [class & args]
    (assert (r/reacl-class? class) class)
    (c/local-state nil
                   (c/handle-message hm
                                     (c/fragment
                                      (c/handle-effect-result set-ref (make-ref!))
                                      (c/dynamic d class args))))))

;; TODO: ref/bridge to allow reacl components to send messages to reacl-c items?
