(ns reacl-c.interop.react
  (:require [reacl-c.base :as base]
            [active.clojure.cljs.record :as r :include-macros true]))

(r/define-record-type ^:no-doc LiftReact
  (make-lift-react class-or-fn props children dynamic?)
  lift-react?
  [class-or-fn lift-react-class
   props lift-react-props
   children lift-react-children
   dynamic? lift-react-dynamic?]
  base/E
  (-is-dynamic? [this] dynamic?))

(r/define-record-type ^:no-doc LiftReactElement
  (make-lift-react-element element)
  lift-react-element?
  [element lift-react-element-element]
  base/E
  (-is-dynamic? [this] false))

(defn ^:no-doc lift* [class props dynamic? & children]
  ;; Note: this is used in reacl-c-reacl.
  ;; Note: I thought, lifted Reacl class with app-state should be considered 'dynamic'; but somehow it does not make any difference (yet)
  (make-lift-react class props children dynamic?))

(defn lift
  "Returns an item implemented by the given React class or function
component and props. Note that children must be React element or
  strings. See [[reacl-c.main.react/embed]] to embed Reacl-C items as
  children."
  [class props & children]
  ;; Note: Unfortunately, using a :children prop with an array makes a
  ;; small difference to the children list. In the former case, React
  ;; issues a warning when the children don't have keys. Otherwise
  ;; it's equivalent.
  (apply lift* class props false children))

(defn element
  "Return an item that renders as the given React element. Note that this
   means the item can only be used once in an item tree, unlike other
   items. Prefer using [[lift]] if you can."
  [element]
  (make-lift-react-element element))
