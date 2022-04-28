(ns reacl-c.dom
  "This namespace contains functions for all HTML and SVG tags, which
  all return corresponding dom items. Additionally it contains the
  function [[h]], a generic function that creates dom items."
  (:require [reacl-c.base :as base]
            [reacl-c.dom-base :as dom-base]
            [reacl-c.core :as core]
            [clojure.string :as str]
            [active.clojure.functions :as f]
            #?(:clj [reacl-c.impl.macros :refer (defdom)]))
  #?(:cljs (:require-macros [reacl-c.impl.macros :refer (defdom)]))
  (:refer-clojure :exclude (meta map time use set symbol)))

;; TODO: add some way to lift new primitives (with React getSnapshot.... and/or webcomponents)

(defn element? "Returns if `v` is a dom element." [v]
  (dom-base/element? v))

(defn dom-attributes? "Returns if v is a map, and not an item." [v]
  (and (map? v)
       (not (satisfies? base/E v))))

(defn- analyze-dom-args [args]
  (if (empty? args)
    (cons {} args)
    (let [x (first args)]
      (if (dom-attributes? x)
        args
        (cons {} args)))))

(defn- event? [k]
  (str/starts-with? (name k) "on"))

(let [none [{} {}]]
  (defn- split-events [attrs]
    ;; optimized on having no or few events; which is the usual case.
    (cond
      (not (some event? (keys attrs))) [attrs {}]
      :else
      (let [[attrs events]
            (reduce-kv (fn [[attrs events] k v]
                         (if (event? k)
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
    ;; possible optimization: if all event handlers are already 'bound', we don't need with-bind.
    (fn [& args]
      (let [[attrs & children] (analyze-dom-args args)
            [attrs events] (split-events attrs)]
        ;; Note: not checking for nil events handlers here, to give the
        ;; user the chance to have a stable tree if needed (important
        ;; for focus etc)
        (if (empty? events)
          (apply f attrs children)
          (core/with-bind (f/partial k f attrs events children)))))))

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
    `(def ~(vary-meta name #(merge {:arglists '(params)
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
(defdom a)
(defdom abbr)
(defdom address)
(defdom area)
(defdom article)
(defdom aside)
(defdom audio)
(defdom b)
(defdom base)
(defdom bdi)
(defdom bdo)
(defdom big)
(defdom blockquote)
(defdom body)
(defdom br)
(defdom button)
(defdom canvas)
(defdom caption)
(defdom cite)
(defdom code)
(defdom col)
(defdom colgroup)
(defdom data)
(defdom datalist)
(defdom dd)
(defdom del)
(defdom details)
(defdom dfn)
(defdom div)
(defdom dl)
(defdom dt)
(defdom em)
(defdom embed)
(defdom fieldset)
(defdom figcaption)
(defdom figure)
(defdom footer)
(defdom form)
(defdom h1)
(defdom h2)
(defdom h3)
(defdom h4)
(defdom h5)
(defdom h6)
(defdom head)
(defdom header)
(defdom hr)
(defdom html)
(defdom i)
(defdom iframe)
(defdom img)
(defdom input)
(defdom ins)
(defdom kbd)
(defdom keygen)
(defdom label)
(defdom legend)
(defdom li)
(defdom link)
(defdom main)
(defdom map)
(defdom mark)
(defdom menu)
(defdom menuitem)
(defdom meta)
(defdom meter)
(defdom nav)
(defdom noscript)
(defdom object)
(defdom ol)
(defdom optgroup)
(defdom option)
(defdom output)
(defdom p)
(defdom param)
(defdom pre)
(defdom progress)
(defdom q)
(defdom rp)
(defdom rt)
(defdom ruby)
(defdom s)
(defdom samp)
(defdom script)
(defdom section)
(defdom select)
(defdom small)
(defdom source)
(defdom span)
(defdom strong)
(defdom style)
(defdom sub)
(defdom summary)
(defdom sup)
(defdom table)
(defdom tbody)
(defdom td)
(defdom textarea)
(defdom tfoot)
(defdom th)
(defdom thead)
(defdom time)
(defdom title)
(defdom tr)
(defdom track)
(defdom u)
(defdom ul)
(defdom var)
(defdom video)
(defdom wbr)
(defdom svg)
(defdom polygon)
(defdom line)
(defdom rect)
(defdom circle)
(defdom ellipse)
(defdom polyline)
(defdom text)
(defdom path)
(defdom defs)
(defdom clipPath)
(defdom g)
(defdom linearGradient)
(defdom radialGradient)
(defdom stop)
(defdom image)
(defdom animate)
(defdom animateColor)
(defdom animateMotion)
(defdom animateTransform)
(defdom set)
(defdom cursor)
(defdom desc)
(defdom feBlend)
(defdom feColorMatrix)
(defdom feComponentTransfer)
(defdom feComposite)
(defdom feConvolveMatrix)
(defdom feDiffuseLighting)
(defdom feDisplacementMap)
(defdom feDistantLight)
(defdom feFlood)
(defdom feFuncA)
(defdom feFuncB)
(defdom feFuncG)
(defdom feFuncR)
(defdom feGaussianBlur)
(defdom feImage)
(defdom feMerge)
(defdom feMergeNode)
(defdom feMorphology)
(defdom feOffset)
(defdom fePointLight)
(defdom feSpecularLighting)
(defdom feSpotLight)
(defdom feTile)
(defdom feTurbulence)
(defdom font)
(defdom marker)
(defdom mask)
(defdom metadata)
(defdom mpath)
(defdom pattern)
(defdom switch)
(defdom symbol)
(defdom textPath)
(defdom tspan)
(defdom use)
(defdom view)
