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

(defn- dom-element* [type custom? attrs events & children]
  {:pre [(string? type)
         (map? attrs)
         (map? events)
         (every? #(or (ifn? %) (nil? %)) (vals events))
         (base/assert-item-list type children)]}
  (dom-base/make-element type custom? attrs events nil children))

(defn ^:no-doc dom-element** [custom? type & args]
  {:pre [(string? type)]}
  (let [[attrs_ & children] (analyze-dom-args args)
        [attrs events] (split-events attrs_)]
    (apply dom-element* type custom? attrs events children)))

(defn ^:no-doc dom-element [type & args]
  (apply dom-element** false type args))

(defn custom
  "Returns a DOM item of the specified `type`, which is intended to be
  the name of a custom web component element registered
  before. Arguments are the same as the specific DOM functions,
  like [[div]], but events are handled outside of the React
  framework. That enables to define a handler for an event of type
  \"myevent\" as `:onMyEvent` for example."
  [type & args]
  (apply dom-element** true type args))

(defn- dom-function [type]
  {:pre [(string? type)]}
  ;; Note: could also use (with-async-actions (fn [deliver! ])) and event handlers that call deliver! - but then they aren't pure anymore (at least after a translation)
  ;; Note: DOM uses upper-case for the type (.nodeName), but React enforces lower-case :-/
  ;; (assert (= type (str/lower-case type)) type)  -- TODO: what about clipPath ?
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
  (defn ^:no-doc defn-dom-impl [f args]
    (let [[attrs & children] (analyze-dom-args args)
          [attrs events] (split-events attrs)]
      (if (empty? events)
        (apply f attrs children)
        (core/with-bind (f/partial k f attrs events children))))))

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
    `(let [f# (fn ~params ~@body)]
       (core/defn-item ~(vary-meta name assoc :arglists '(params)) ;; TODO: opt-assoc (only if not set yet);
         ;; ~@(when static? [:static]) ;; FIXME: with-bind must be ouside of static.
         ~@(when state-schema? [:- state-schema?])
         ~@(when docstring? [docstring?])
         [& args#]
         ;; TODO: validate arguments earlier (arity and schema).
         ;; TODO: optimize toplevel with-bind/with-state-as like defn-item.
         (defn-dom-impl f# args#)))))

(defn- join-classes [& cs]
  (str/join " " (filter not-empty
                        (mapcat #(str/split % #"\s+") cs))))
(letfn [(merge-a2 [a1 a2]
          (reduce-kv (fn [res k v]
                       (case k
                         ;; merge class names
                         ;; Note, allegedly: "the styles are applied in the order they are declared in the document, the order they are listed in the element has no effect."
                         (:class :className)
                         (-> res
                             (dissoc :class :className)
                             (assoc :className (join-classes (apply join-classes (clojure.core/map second (filter #(#{:class :className} (first %))
                                                                                                                  res)))
                                                             v)))
                         ;; Merging styles absolutely correct is very hard (like merging :border and :border-with)
                         ;; This will only cover simple cases.
                         :style
                         (update res :style merge v)
                         ;; for any other attribute, overwrite   (TODO: maybe compose event handlers?)
                         (assoc res k v)))
                     a1
                     a2))]
  (defn merge-attributes [& attrs]
    (assert (every? #(or (nil? %) (dom-attributes? %)) attrs) (vec (remove #(or (nil? %) (dom-attributes? %)) attrs)))
    (reduce merge-a2
            {}
            attrs)))

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
