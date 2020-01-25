(ns ^:no-doc reacl-c.impl.macros)

(defmacro defdom [tag]
  `(def ~(vary-meta tag assoc
                    :doc (str "Returns a dom element corresponding to
                       a `" tag "` tag. The `attrs` argument is an
                       optional map of attributes. Any attribute
                       starting with `on`, is expected to be a
                       function taking an event and return
                       a [[reacl-c.core/return]] value.  The remaining
                       `children` arguments must be other elements or
                       strings.")
                    :arglists `'([attrs & children] [& children])
                    )
     (reacl-c.dom/dom-function ~(name tag))))
