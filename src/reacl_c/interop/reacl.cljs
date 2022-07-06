(ns reacl-c.interop.reacl
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.base :as base]
            [reacl-c.interop.react :as react]
            [reacl2.core :as r]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

(defn- handle-action [invoke! a]
  (invoke! (f/constantly (c/return :action a))))

 (defn- set-state [invoke! st cb]
   (when-not (r/keep-state? st)
     (invoke! (f/constantly (c/return :state st))))
   ;; TODO: add callback to invoke! ?
   (when cb (cb)))

(defn- reacl-wrapper [props]
  ;; a React function component
  (let [class (aget props "class")
        args (aget props "args")
        state (aget props "state")
        invoke! (aget props "invoke")
        ref (aget props "c_ref")]
    (r/react-element class (cond-> {:args args
                                    :handle-action! (f/partial handle-action invoke!)
                                    :ref ref}
                             (r/has-app-state? class)
                             (merge {:app-state state
                                     :set-app-state! (f/partial set-state invoke!)})))))

(defn- mk-ref! []
  #js {:current nil})

(c/defn-effect ^:private send-message! [ref msg]
  (r/send-message! (.-current ref) msg))

(let [ar (fn [invoke! state class args ref]
           (react/lift* reacl-wrapper #js {"class" class
                                           "args" args
                                           "state" state
                                           "invoke" invoke!
                                           "c_ref" ref}
                        (r/has-app-state? class)))
      d (fn [[state ref] class args]
          (c/focus lens/first
                   (c/with-async ar state class args ref)))
      
      hm (fn [[state ref] msg]
           (c/return :action (send-message! ref msg)))
      set-ref #(assoc %1 1 %2)]
  (defn lift
    "Returns an item implemented by the given Reacl class and
  arguments. Messages sent to the item are forwarded to the component,
  actions emitted are passed though, and the app-state of the
  component (if it has one) is bound to the state of the item."
    [class & args]
    (assert (r/reacl-class? class) class)
    (c/local-state (base/make-initializer mk-ref! nil)
                   (c/handle-message hm
                                     (c/dynamic d class args)))))

;; TODO: ref/bridge to allow reacl components to send messages to reacl-c items?
