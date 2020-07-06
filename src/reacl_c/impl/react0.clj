(ns reacl-c.impl.react0
  (:refer-clojure :exclude [class]))

(defn- make-method [this args f]
  `(fn [& args#]
     (cljs.core/this-as ~this
                        (let [~args (extract-args (.-props ~this))]
                          (apply ~f args#)))))

(defn- static? [k]
  (and (vector? k)
       (= :static (first k))))

(defmacro class [name this args & decls]
  `(make-class ~name
               ~(into {}
                      (map (fn [[k v]]
                             (if (static? k)
                               [k v]
                               [k (make-method this args v)]))
                           (partition-all 2 decls)))))

(defmacro defclass [name this args & decls]
  `(def ~name (class ~(str *ns* "/" name) ~this ~args ~@decls)))
