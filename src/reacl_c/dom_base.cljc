(ns ^:no-doc reacl-c.dom-base
  ;; Internal namespace; don't depend on it.
  (:require [reacl-c.base :as base]
            #?(:cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [active.clojure.record :as r])))

(r/define-record-type ^:no-doc Element
  (make-element type attrs events ref children)
  element?
  [type element-type
   attrs element-attrs
   events element-events
   ref element-ref
   children element-children]
  base/E
  (-is-dynamic? [{events :events children :children}]
                ;; OPT: maybe worth to cache this? calculate in
                ;; advance?
                ;; OPT: if all event handlers are created by 'with-bind',
                ;; then it does not have to be dynamic.
                (or (not (empty? events)) (some base/is-dynamic? children))))

(defn ^:no-doc set-ref [e ref]
  (assert (element? e))
  (assoc e :ref ref))
