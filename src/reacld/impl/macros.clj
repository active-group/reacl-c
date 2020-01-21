(ns ^:no-doc reacld.impl.macros)

(defmacro defdom [tag]
  `(def ~(vary-meta tag assoc
                    :doc (str "Returns a dom element corresponding to
                       a `" tag "` tag. The `attrs` argument is an
                       optional map of attributes. Any attribute
                       starting with `on`, is expected to be a
                       function taking an event and return an action
                       or `nil`. The remaining `children` arguments
                       must be other elements or strings.")
                    :arglists `'([attrs & children] [& children])
                    )
     (reacld.dom/dom-function ~(name tag))))
