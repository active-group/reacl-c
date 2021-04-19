(ns reacl-c.dom
  "This namespace contains functions for all HTML and SVG tags, which
  all return corresponding dom items. Additionally it contains the
  function [[h]], a generic function that creates dom items."
  (:require [reacl-c.base :as base]
            [clojure.string :as str]
            #?(:cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [active.clojure.record :as r])
            #?(:clj [reacl-c.impl.macros :refer (defdom)]))
  #?(:cljs (:require-macros [reacl-c.impl.macros :refer (defdom)]))
  (:refer-clojure :exclude (meta map time use set symbol)))

;; TODO: some standard event handlers? constantly, value, checked.
;; TODO: add some way to lift new primitives (with React getSnapshot.... and/or webcomponents)

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
    ;; TODO: maybe worth to cache this? calculate in advance?
    (or (not (empty? events)) (some base/is-dynamic? children))))

(defn ^:no-doc set-ref [e ref]
  (assoc e :ref ref))

(defn dom-attributes? "Returns if v is a map, and not an item." [v]
  (and (map? v)
       (not (satisfies? base/E v))))

(defn- analyze-dom-args [args]
  (if (empty? args)
    [{} args]
    (let [x (first args)]
      (if (dom-attributes? x)
        [x (rest args)]
        [{} args]))))

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
         (every? #(or (ifn? %) (nil? %)) (vals events))
         (base/assert-item-list type children)]}
  (make-element type attrs events nil children))

(defn ^:no-doc dom-element [type & args]
  {:pre [(string? type)]}
  (let [[attrs_ children] (analyze-dom-args args)
        [attrs events] (split-events attrs_)]
    (apply dom-element* type attrs events children)))

(defn- dom-function [type]
  {:pre [(string? type)]}
  ;; Note: could also use (with-async-actions (fn [deliver! ])) and event handlers that call deliver! - but then they aren't pure anymore (at least after a translation)
  ;; Note: DOM uses upper-case for the type (.nodeName), but React enforces lower-case :-/
  ;; (assert (= type (str/lower-case type)) type)  -- TODO: what about clipPath ?
  (fn [& args]
    (apply dom-element type args)))

(defn ^{:arglists '([type attrs & children]
                    [type & children])}
  h
  "Returns a DOM item of the specified `type`, like \"div\" for example. Arguments are the same as the specific DOM functions, like [[div]]."
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
