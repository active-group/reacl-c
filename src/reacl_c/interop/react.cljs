(ns reacl-c.interop.react
  (:require [reacl-c.base :as base]
            [active.clojure.cljs.record :as r :include-macros true]))

(r/define-record-type ^:no-doc LiftReact
  (make-lift-react class props)
  lift-react?
  [class lift-react-class
   props lift-react-props]
  base/E
  (-is-dynamic? [this]
                false
                #_(and (reacl/reacl-class? class) (reacl/has-app-state? class))))

(defn react
  "Returns an item implemented by the given React class and props."
  [class props]
  ;; TODO: 'clojure props' and/or 'js props'?
  (make-lift-react class props))
