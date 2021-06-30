(ns ^:no-doc reacl-c.impl.react0
  (:require ["create-react-class" :as createReactClass]
            ["react" :as react]
            ["react-dom" :as react-dom]
            [clojure.string :as str]))

(defn mk-props [args]
  #js {"args" args})

(defn extract-args [props]
  (aget props "args"))

(defn mk-state [v]
  #js {"state" v})

(defn extract-state [state]
  (aget state "state"))

(defn get-state [this]
  (extract-state (.-state this)))

(defn get-args [this]
  (extract-args (.-props this)))

(defn set-state [this f & [cb]]
  (.setState this
             (fn [state props]
               (mk-state (apply f (extract-state state) (extract-args props))))
             cb))

(defn create-ref []
  (react/createRef))

(defn current-ref [r]
  (.-current r))

(defn- static? [[k v]] (and (vector? k) (= :static (first k))))

(defn set-statics! [class decls]
  (doseq [[k v] decls]
    (aset class k v)))

(defn- default-should-component-update? [new-props new-state]
  (this-as this
    (or (and (some? new-state) ;; class has no local-state...
             (not= (extract-state new-state)
                   (get-state this)))
        (not= (extract-args new-props)
              (get-args this)))))

(defn make-class [name decls]
  (let [method-decls (remove static? decls)
        static-decls (map (fn [[[_ k] v]] [k v])
                          (filter static? decls))]
    (doto (createReactClass
           (apply js-obj (mapcat identity
                                 (-> (into {}
                                           (map (fn [[n f]]
                                                  [n (fn [& args]
                                                       (this-as this
                                                                (apply f this args)))])
                                                method-decls))
                                     (assoc "displayName" name)
                                     (update "getInitialState"
                                             (fn [f]
                                               (when f
                                                 (fn []
                                                   (this-as this (mk-state (.call f this)))))))
                                     (update "shouldComponentUpdate"
                                             (fn [p]
                                               (if p
                                                 (fn [new-props new-state]
                                                   ;; call p with args and state.
                                                   (this-as this
                                                            (.call p this (extract-args new-props) (when (some? new-state) (extract-state new-state)))))
                                                 default-should-component-update?)))
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
    (case n
      "auto-focus" "autofocus"
      n)))

(def ^:private adjust-dom-attr-name
  (memoize (fn [n]
             (case n
               "for" "htmlFor"
               "class" "className"
               (legacy-adjust-dom-attr-name n) ;; TODO: get rid of this.
               ))))

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
  (let [;; Note: for html/dom notes, React *requires* attrs like
        ;; "className", and warns about "class"; but for web
        ;; components (dom/custom elements) is passes them on as
        ;; attributes unchanged, so the user has to use "class"
        ;; instead of "className" :-/ We should probably remove all
        ;; mangling of attributes (and style names); but that would be
        ;; a bit of a breaking change as users have their code. For
        ;; now, try to distinguish between native and custom elements
        ;; (they must have a '-' in the name):
        adjust-attr-name (if (and (string? type) (str/includes? type "-"))
                           identity
                           adjust-dom-attr-name)
        aobj (reduce-kv (fn [r k v]
                          (let [n (name k)]
                            (aset r (adjust-attr-name n) (adjust-dom-attr-value n v)))
                          r)
                        #js {}
                        attrs)]
    (apply react/createElement type aobj children)))

(defn fragment [& children]
  ;; Note: fragments can have a key! (but no ref)
  (apply react/createElement react/Fragment #js {} children))
