(ns ^:no-doc reacl-c.impl.react0
  (:require ["create-react-class" :as createReactClass]
            ["react" :as react]
            #_["react-dom/client" :as react-dom]
            ["react-dom" :as react-dom]
            [clojure.string :as str]
            goog.object))

(defn create-ref []
  (react/createRef))

(defn current-ref [r]
  (.-current r))

(defn- static? [[k v]] (and (vector? k) (= :static (first k))))

(defn set-statics! [class decls]
  (doseq [[k v] decls]
    (aset class k v)))

(defn make-class [name decls]
  (let [method-decls (remove static? decls)
        static-decls (map (fn [[[_ k] v]] [k v])
                          (filter static? decls))]
    (doto (createReactClass
           (apply js-obj (mapcat identity
                                 (-> (into {}
                                           (map (fn [[n f]]
                                                  ;; add 'this' as first argument to all methods:
                                                  [n (fn [& args]
                                                       (this-as this
                                                                (apply f this args)))])
                                                method-decls))
                                     (assoc "displayName" name)
                                     ))))
      (set-statics! (cons ["displayName" name] (cons ["name" name] static-decls))))))

(defn render-component [comp dom]
  (do (react-dom/render comp dom)
      dom)
  #_(doto (react-dom/createRoot dom)
    (.render comp)))

(defn unmount-component! [handle]
  (react-dom/unmountComponentAtNode handle)
  #_(.unmount handle))

(defn elem
  [class props]
  (when (and (= nil (aget props "key")) (goog.object/containsKey props "key"))
    (goog.object/remove props "key"))
  (react/createElement class props))

(defn- camelize
  "Camelcases a hyphenated string, for example:
  > (camelize \"background-color\")
  < \"backgroundColor\""
  [s]
  (str/replace s #"-(.)" (fn [[_ c]] (str/upper-case c))))

(def ^:private adjust-dom-style-name
  (memoize (fn [n]
             ;; needed for border-style => borderStyle. Not sure if correct for -moz-column-role ?
             (camelize n))))

(defn- adjust-dom-attr-value [n v]
  (case n
    "style" (clj->js (into {} (map (fn [[k v]] [(adjust-dom-style-name (name k)) v]) v)))
    v))

(defn dom-elem [type attrs & children]
  (let [aobj (reduce-kv (fn [r k v]
                          (let [n (name k)]
                            (aset r n (adjust-dom-attr-value n v)))
                          r)
                        #js {}
                        attrs)]
    (apply react/createElement type aobj children)))

(defn fragment [key & children]
  (apply react/createElement react/Fragment (if (some? key) #js {"key" key} #js {}) children))
