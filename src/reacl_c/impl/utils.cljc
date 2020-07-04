(ns reacl-c.impl.utils
  (:require [reacl-c.base :as base]))

#?(:cljs
   (defn named-generator [gen]
     (let [cache (js/WeakMap.)]
       (fn [name-id]
         (assert (base/name-id? name-id))
         (or (.get cache name-id)
             (let [c (gen (base/name-id-name name-id))]
               (.set cache name-id c)
               c))))))

#?(:cljs
   (defn warn [& args]
     (if (and js/console js/console.warn)
       (apply js/console.warn args)
       (apply println args))))
