(ns reacl-c.interop.reacl
  (:require [reacl-c.core :as c]
            [reacl-c.interop.react :as react]
            [reacl-c.impl.react0 :as r0 :include-macros true]
            [reacl2.core :as r]
            [active.clojure.lens :as lens]))

#_(extend-type base/LiftReacl
  IReacl
  (-xpath-pattern [{class :class args :args}]
    (class-args-pattern class [args]))
  (-instantiate-reacl [{class :class args :args} binding]
    [(if (and (rcore/reacl-class? class) (rcore/has-app-state? class))
       (apply class binding args)
       (apply class args))]))

(r0/defclass ^:private reacl-wrapper ;; TODO: make it a function component?
  "render"
  (fn [this]
    (let [[class args state return! ref] (r0/get-args this)]
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
                                                         (when cb (cb)))}))))))

(c/defn-effect ^:private make-ref! []
  #js {:current nil})

(c/defn-effect ^:private send-message! [ref msg]
  (r/send-message! (.-current ref) msg))

(let [ar (fn [return! state class args ref]
           (react/react reacl-wrapper (r0/mk-props [class args state return! ref])))
      d (fn [[state ref] class args]
          (when (some? ref) ;; ...before the effect is executed.
            (c/focus lens/first
                     (c/with-async-return ar state class args ref))))
      
      hm (fn [[state ref] msg]
           (c/return :action (send-message! ref msg)))
      set-ref #(assoc %1 1 %2)]
  (defn reacl
    "Returns an item implemented by the given Reacl class and arguments."
    [class & args]
    (assert (r/reacl-class? class) class)
    (c/local-state nil
                   (c/handle-message hm
                                     (c/fragment
                                      (c/handle-effect-result set-ref (make-ref!))
                                      (c/dynamic d class args))))))

;; TODO: ref/bridge to allow reacl components to send messages to reacl-c items?
