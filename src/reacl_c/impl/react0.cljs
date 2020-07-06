(ns reacl-c.impl.react0
  (:require [create-react-class :as createReactClass]
            [react :as react]
            [react-dom :as react-dom]
            [clojure.string :as str]))

(defn- mk-props [args]
  #js {"args" args})

(defn extract-args [props]
  (aget props "args"))

(defn mk-state [v]
  #js {"state" v})

(defn extract-state [state]
  (aget state "state"))

(defn get-state [this]
  (extract-state (.-state this)))

(defn set-state [this f & [cb]]
  (.setState this
             (fn [state props]
               (mk-state (apply f (extract-state state) (extract-args props))))
             cb))

(defn create-ref []
  (react/createRef))

(defn current-ref [r]
  (.-current r))

(defn child-ref [this]
  (aget this "child_ref"))

(defn- static? [[k v]] (and (vector? k) (= :static (first k))))

(defn set-statics! [class decls]
  (doseq [[k v] decls]
    (aset class k v)))

(defn- make-class [name decls]
  ;; TODO: "shouldComponentUpdate" (fn [...])
  (let [method-decls (remove static? decls)
        static-decls (map (fn [[[_ k] v]] [k v]) (filter static? decls))]
    (doto (createReactClass
           (apply js-obj (mapcat identity
                                 (-> (into {} method-decls)
                                     (assoc "displayName" name)
                                     (update "getInitialState"
                                             (fn [f]
                                               (fn []
                                                 (this-as this
                                                          (aset this "child_ref" (create-ref))
                                                          (when f (mk-state (.call f this)))))))
                                     #_(update "shouldComponentUpdate"
                                               (fn [p]
                                                 ;; TODO: = default?
                                                 )
                                               )
                                     ))))
      (set-statics! static-decls))))

(defn render-component [comp dom]
  (react-dom/render comp dom))

(defn elem
  ([class ref args]
   (react/createElement class (doto (mk-props (vec args))
                                (aset "ref" ref))))
  ([class key ref args]
   (react/createElement class (doto (mk-props (vec args))
                                (aset "key" key)
                                (aset "ref" ref)))))

(defn- legacy-adjust-dom-attr-name [n]
  (if (str/starts-with? n "on")
    (apply str "on" (str/upper-case (str (first (drop 2 n)))) (drop 3 n))
    n))

(defn- adjust-dom-attr-name [n]
  (case n
    "for" "htmlFor"
    "class" "className"
    (legacy-adjust-dom-attr-name n) ;; TODO: get rid of this.
    ))

(defn- adjust-dom-attr-value [n v]
  (case n
    "style" (clj->js v)
    v))

(defn dom-elem [type attrs & children]
  #_(js/console.log "dom attrs" attrs)
  ;; TODO: more attr translations (style...)
  (let [aobj (reduce-kv (fn [r k v]
                          (let [n (name k)]
                            (aset r (adjust-dom-attr-name n) (adjust-dom-attr-value n v)))
                          r)
                        #js {}
                        attrs)]
    (apply react/createElement type aobj children)))

(defn fragment [& children]
  (apply react/createElement react/Fragment #js {} children))
