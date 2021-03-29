(ns reacl-c.interop.react
  (:require [reacl-c.base :as base]
            [active.clojure.cljs.record :as r :include-macros true]))

(r/define-record-type ^:no-doc LiftReact
  (make-lift-react class props dynamic?)
  lift-react?
  [class lift-react-class
   props lift-react-props
   dynamic? lift-react-dynamic?]
  base/E
  (-is-dynamic? [this] dynamic?))

(defn ^:no-doc lift* [class props dynamic?]
  ;; Note: I thought, lifted Reacl class with app-state should be considered 'dynamic'; but somehow it does not make any difference (yet)
  (make-lift-react class props dynamic?))

(defn lift
  "Returns an item implemented by the given React class or function component and props."
  [class props]
  (lift* class props false))
