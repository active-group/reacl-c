(ns ^:no-doc reacl-c.impl.react0
  (:refer-clojure :exclude [class]))

(defmacro class [name & decls]
  `(make-class ~name
               ~(vec (map vec
                          (partition-all 2 decls)))))

(defmacro defclass [name & decls]
  `(def ~name (class ~(str *ns* "/" name)
                     ~@decls)))

(defmacro update-on [prop-keys & [state-keys]]
  ;; Note: although O(identical?) < O(=), both seem to be constant, and
  ;; not depend on the size of a map for example. So probably not worth
  ;; to add an identical? check.
  (let [state (gensym "state")
        props (gensym "props")
        new-props (gensym "new-props")
        new-state (gensym "new-state")]
    `(fn ~'should-update-on [this# ~new-props ~new-state]
       (let [~state (.-state this#)
             ~props (.-props this#)]
         (or ~@(map (fn [k]
                      `(not= (aget ~new-state ~k)
                             (aget ~state ~k)))
                    state-keys)
             ~@(map (fn [k]
                      `(not= (aget ~new-props ~k)
                             (aget ~props ~k)))
                    prop-keys))))))
