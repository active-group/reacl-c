(ns ^:no-doc reacl-c.impl.macros)

(def ^:private dom-alist '([attrs & children] [& children]))

(defn- dom-doc [tag]
  (str "Returns a dom element item corresponding to
                       a `" tag "` tag. The `attrs` argument is an
                       optional map of attributes. Any attribute
                       starting with `on`, is expected to be a
                       function taking the state and an event and return
                       a [[reacl-c.core/return]] value.  The remaining
                       `children` arguments must be other items or
                       strings."))

(defmacro defdom [tag]
  `(def ~(vary-meta tag assoc
                    :doc (dom-doc tag)
                    :arglists `'~dom-alist)
     (reacl-c.dom/dom-function ~(name tag))))
