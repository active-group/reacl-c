(ns reacl-c.dom
  "This namespace contains functions for all HTML and SVG tags, which
  all return corresponding dom items. Additionally it contains the
  function [[h]], a generic function that creates dom items."
  (:require [reacl-c.base :as base]
            [reacl-c.dom-base :as dom-base]
            #?(:clj [reacl-c.core :as core]
               :cljs [reacl-c.core :as core :include-macros true])
            #?(:clj [clojure.core :as clj]
               :cljs [cljs.core :as clj])
            [clojure.string :as str]
            [active.clojure.functions :as f]
            #?(:clj [reacl-c.impl.macros :as m]))
  #?(:cljs (:require-macros [reacl-c.impl.macros :as m]))
  (:refer-clojure :exclude (meta map time use set symbol)))

;; TODO: add a set-properties effect for custom elements.

(defn element? "Returns if `v` is a dom element." [v]
  (dom-base/element? v))

(defn dom-attributes? "Returns if v is a map, and not an item." [v]
  ;; Note: nil is not an attribute map, but an item: nil=c/empty
  (and (map? v)
       (not (satisfies? base/E v))))

(defn- analyze-dom-args [args]
  (if (empty? args)
    (cons {} args)
    (let [x (first args)]
      (if (dom-attributes? x)
        args
        (cons {} args)))))

(let [none [{} {}]]
  (defn- split-events [attrs]
    ;; optimized on having no or few events; which is the usual case.
    (cond
      (not (some dom-base/event-attribute? (keys attrs))) [attrs {}]
      :else
      (let [[attrs events]
            (reduce-kv (fn [[attrs events] k v]
                         (if (dom-base/event-attribute? k)
                           [(dissoc! attrs k) (assoc! events k v)]
                           [attrs events]))
                       [(transient attrs) (transient {})]
                       attrs)]
        [(persistent! attrs) (persistent! events)]))))

(defn- dom-element* [type attrs events & children]
  {:pre [(string? type)
         (map? attrs)
         (map? events)
         (every? #(or (ifn? %) (nil? %)) (vals events))]}
  (base/assert-item-list type children)
  (let [[attrs ref] (if (contains? attrs :ref)
                      [(dissoc attrs :ref) (:ref attrs)]
                      [attrs nil])]
    (dom-base/make-element type attrs events ref children)))

(defn ^:no-doc dom-element** [type & args]
  {:pre [(string? type)]}
  (let [[attrs_ & children] (analyze-dom-args args)
        [attrs events] (split-events attrs_)]
    (apply dom-element* type attrs events children)))

(defn ^:no-doc dom-element [type & args]
  (apply dom-element** type args))


(defn- dom-function [type]
  {:pre [(string? type)]}
  ;; Note: could also use (with-async-actions (fn [deliver! ])) and
  ;; event handlers that call deliver! - but then they aren't pure
  ;; anymore (at least after a translation)
  
  ;; Note: DOM uses upper-case for the type (.nodeName), but React
  ;; enforces lower-case, in general; but for something like
  ;; 'clipPath' is complains only when used outside of an svg; inside
  ;; camelCase is ok.
  (fn [& args]
    (apply dom-element type args)))

(let [k (fn [f attrs events children bind]
          (apply f
                 (reduce-kv (fn [attrs ev h]
                              (assoc attrs ev
                                     (if (some? h)
                                       (bind h)
                                       nil)))
                            attrs
                            events)
                 children))]
  (defn ^:no-doc fn-dom-wrapper [f]
    (fn [& args]
      (let [[attrs & children] (analyze-dom-args args)
            [attrs-ne events] (split-events attrs)]
        ;; Note: not checking for nil events handlers here, to give the
        ;; user the chance to have a stable tree if needed (important
        ;; for focus etc), if he uses something like :onclick (when ok ...)
        (if (or (empty? events)
                ;; check if we actually need the extra classes (not
                ;; sure if worth it - if we could inline
                ;; local-state+handle-action into defn-item, then not)
                (every? core/bound-handler? (clj/map second events)))
          (apply f attrs children)
          (core/with-bind (f/partial k f attrs-ne events children)))))))

(defmacro ^{:arglists '([name [attrs & children] & body])} defn-dom
  "Defines a function that works like the dom element functions in this
  namespace (e.g. [[div]]), in that the first argument is an optional
  attributes map, followed by arbitrarily many child items.

  If the defined function is called without an attribute map, then
  `{}` will be passed implicitly as the first argument.

  Additionally, all attributes starting with `:on` must be event
  handlers and are automatically bound to the state of the returned
  item, via [[reacl-c.core/with-bind]]. That means you can assign
  these event handlers to any item in the body, or
  use [[reacl-c.core/call]], irrespective of the state that those
  items get.

  Tip: Pass the attributes to the toplevel dom element that is
  returned, and use [[merge-attributes]] to add some default
  attributes in your function body."
  [name params & body]
  (let [[name static? state-schema? docstring? params & body] (apply core/parse-defn-item-args name params body)]
    `(def ~(vary-meta name #(merge {:arglists `'(~params ~(vec (rest params)))
                                    :doc docstring?} %))
       (fn-dom-wrapper (core/fn-item* ~name ~static? ~state-schema? ~params ~@body)))))

(defn- join-classes
  "Joins multiple class strings (or nils) into one."
  [& cs]
  (let [cs (remove empty? cs)]
    (cond
      (empty? cs) ""
      (empty? (rest cs)) (first cs)
      :else
      (str/join " " cs))))

(defn- unify-class-attr [m]
  (if (contains? m :className)
    (-> m
        (dissoc :className)
        (assoc :class (get m :className)))
    m))

(letfn [(merge-a2! [res a2]
          (reduce-kv (fn [res k v]
                       (case k
                         ;; merge class names (preferring :class over :className)
                         ;; Note, allegedly: "the styles are applied in the order they are declared in the document, the order they are listed in the element has no effect."
                         (:class :className)
                         (-> res
                             (assoc! :class (apply join-classes [(get res :class) v])))
                         ;; Merging styles absolutely correct is very hard (like merging :border and :border-with)
                         ;; This will only cover simple cases.
                         :style
                         (assoc! res :style (merge (get res :style) v))
                         ;; for any other attribute, overwrite
                         (assoc! res k v)))
                     res
                     a2))]
  (defn merge-attributes
    "Merge two or more attribute maps into one. This handles merging
  multiple `:style` maps into one, and concatenates `:class` and
  `:className` strings."
    [& attrs]
    (assert (every? #(or (nil? %) (dom-attributes? %)) attrs) (vec (remove #(or (nil? %) (dom-attributes? %)) attrs)))
    (let [attrs (remove nil? attrs)]
      (cond
        (empty? attrs)
        {}
        (empty? (rest attrs))
        (first attrs)

        :else
        (persistent! (reduce merge-a2!
                             (transient (unify-class-attr (first attrs)))
                             (rest attrs)))
        ))))

(defmacro def-dom
  "Defines an alias for a dom function, for example:

```
  (def-dom page div)
```

  The var `page` will be bound to function that is essentially
  identical to `div`, but additionally, the name of the var is attached
  to the returned items, which can be helpful in testing and debugging
  utilities (see [[reacl-c.core/named]]).

  Also, some default attributes can be specified, for example:

```
  (def-dom page div
    {:style {:margin \"10px\")})
```

  These attributes will be merged with attributes passed by the caller
  of `page` using [[merge-attributes]].
  "
  [name base & [attrs]]
  `(let [base# ~base
         attrs# ~attrs]
     (core/defn-item ~(vary-meta name #(merge {:arglists '([attrs & children] [& children])} %))
       [& args#]
       (let [[attrs2# & children#] (analyze-dom-args args#)]
         (apply base# (merge-attributes attrs# attrs2#) children#)))))

(defn ^{:arglists '([type attrs & children]
                    [type & children])}
  h
  "Returns a DOM item of the specified `type`, like \"div\" for
  example. Arguments are the same as the specific DOM functions,
  like [[div]]."
  [type & args]
  (apply dom-element type args))

;; The following HTML elements are supported by react (http://facebook.github.io/react/docs/tags-and-attributes.html)
(m/defdom a)
(m/defdom abbr)
(m/defdom address)
(m/defdom area)
(m/defdom article)
(m/defdom aside)
(m/defdom audio)
(m/defdom b)
(m/defdom base)
(m/defdom bdi)
(m/defdom bdo)
(m/defdom big)
(m/defdom blockquote)
(m/defdom body)
(m/defdom br)
(m/defdom button)
(m/defdom canvas)
(m/defdom caption)
(m/defdom cite)
(m/defdom code)
(m/defdom col)
(m/defdom colgroup)
(m/defdom data)
(m/defdom datalist)
(m/defdom dd)
(m/defdom del)
(m/defdom details)
(m/defdom dfn)
(m/defdom div)
(m/defdom dl)
(m/defdom dt)
(m/defdom em)
(m/defdom embed)
(m/defdom fieldset)
(m/defdom figcaption)
(m/defdom figure)
(m/defdom footer)
(m/defdom form)
(m/defdom h1)
(m/defdom h2)
(m/defdom h3)
(m/defdom h4)
(m/defdom h5)
(m/defdom h6)
(m/defdom head)
(m/defdom header)
(m/defdom hr)
(m/defdom html)
(m/defdom i)
(m/defdom iframe)
(m/defdom img)
(m/defdom input)
(m/defdom ins)
(m/defdom kbd)
(m/defdom keygen)
(m/defdom label)
(m/defdom legend)
(m/defdom li)
(m/defdom link)
(m/defdom main)
(m/defdom map)
(m/defdom mark)
(m/defdom menu)
(m/defdom menuitem)
(m/defdom meta)
(m/defdom meter)
(m/defdom nav)
(m/defdom noscript)
(m/defdom object)
(m/defdom ol)
(m/defdom optgroup)
(m/defdom option)
(m/defdom output)
(m/defdom p)
(m/defdom param)
(m/defdom pre)
(m/defdom progress)
(m/defdom q)
(m/defdom rp)
(m/defdom rt)
(m/defdom ruby)
(m/defdom s)
(m/defdom samp)
(m/defdom script)
(m/defdom section)
(m/defdom select)
(m/defdom small)
(m/defdom source)
(m/defdom span)
(m/defdom strong)
(m/defdom style)
(m/defdom sub)
(m/defdom summary)
(m/defdom sup)
(m/defdom table)
(m/defdom tbody)
(m/defdom td)
(m/defdom textarea)
(m/defdom tfoot)
(m/defdom th)
(m/defdom thead)
(m/defdom time)
(m/defdom title)
(m/defdom tr)
(m/defdom track)
(m/defdom u)
(m/defdom ul)
(m/defdom var)
(m/defdom video)
(m/defdom wbr)
(m/defdom svg)
(m/defdom polygon)
(m/defdom line)
(m/defdom rect)
(m/defdom circle)
(m/defdom ellipse)
(m/defdom polyline)
(m/defdom text)
(m/defdom path)
(m/defdom defs)
(m/defdom clipPath)
(m/defdom g)
(m/defdom linearGradient)
(m/defdom radialGradient)
(m/defdom stop)
(m/defdom image)
(m/defdom animate)
(m/defdom animateColor)
(m/defdom animateMotion)
(m/defdom animateTransform)
(m/defdom set)
(m/defdom cursor)
(m/defdom desc)
(m/defdom feBlend)
(m/defdom feColorMatrix)
(m/defdom feComponentTransfer)
(m/defdom feComposite)
(m/defdom feConvolveMatrix)
(m/defdom feDiffuseLighting)
(m/defdom feDisplacementMap)
(m/defdom feDistantLight)
(m/defdom feFlood)
(m/defdom feFuncA)
(m/defdom feFuncB)
(m/defdom feFuncG)
(m/defdom feFuncR)
(m/defdom feGaussianBlur)
(m/defdom feImage)
(m/defdom feMerge)
(m/defdom feMergeNode)
(m/defdom feMorphology)
(m/defdom feOffset)
(m/defdom fePointLight)
(m/defdom feSpecularLighting)
(m/defdom feSpotLight)
(m/defdom feTile)
(m/defdom feTurbulence)
(m/defdom font)
(m/defdom marker)
(m/defdom mask)
(m/defdom metadata)
(m/defdom mpath)
(m/defdom pattern)
(m/defdom switch)
(m/defdom symbol)
(m/defdom textPath)
(m/defdom tspan)
(m/defdom use)
(m/defdom view)
