(ns ^:no-doc reacl-c.impl.react0
  (:refer-clojure :exclude [class]))

(defmacro class [name & decls]
  `(make-class ~name
               ~(vec (map vec
                          (partition-all 2 decls)))))

(defmacro defclass [name & decls]
  `(def ~name (class ~(str *ns* "/" name)
                     ~@decls)))
