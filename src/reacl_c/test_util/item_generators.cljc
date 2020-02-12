(ns reacl-c.test-util.item-generators
  (:require [clojure.test.check.generators :as gen]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom])
  (:refer-clojure :exclude [ref]))

(def non-empty-string (gen/fmap #(str "_" %) gen/string))

(def ref (gen/fmap #(hash-map :reacl-ref %) ;; FIXME should be more fixed in base what a ref is?
                   non-empty-string))

(def style
  (gen/map gen/keyword gen/string))

(def attr
  (gen/one-of [(gen/fmap (fn [c] [:class c]) gen/string)
               (gen/fmap (fn [st] [:style st]) style)
               (gen/tuple gen/keyword gen/string)])
  #_(gen/frequency [[1 (gen/fmap (fn [c] [:class c]) gen/string)]
                  [1 (gen/fmap (fn [st] [:style st]) style)]
                  [1 (gen/tuple gen/keyword gen/string)]]))

(def attrs
  (gen/fmap #(into {} %)
            (gen/list attr)))

(def empty-dom   ;; TODO: take rest as gens?
  (gen/fmap (fn [[f attrs]]
              (f attrs))
            (gen/tuple (gen/elements [dom/input dom/a dom/br])
                       attrs)))

(defn dom [item-gen]  ;; TODO: take rest as gens?
  (gen/fmap (fn [[f attrs children]]
              (apply f attrs children))
            (gen/tuple (gen/elements [dom/div dom/span dom/p])
                       attrs
                       (gen/list item-gen))))

(defn dynamic [item-gen]  ;; TODO: fn?
  (gen/fmap (fn [it]
              (c/dynamic (c/constantly it)))
            item-gen))

(defn keyed [key-gen item-gen]
  (gen/fmap (fn [[s it]]
              (c/keyed it s))
            (gen/tuple key-gen item-gen)))

(defn handle-message [item-gen]  ;; TODO: fn?
  (gen/fmap (fn [it]
              (c/handle-message (c/constantly (c/return))
                                it))
            item-gen))

(defn fragment [item-gen]
  (gen/fmap #(apply c/fragment %)
            (gen/list item-gen)))

(def once
  (gen/fmap #(apply c/once %)
            (gen/tuple (gen/elements [(c/return)])
                       (gen/elements [nil (c/return)]))))

(defn with-async-return [item-gen]  ;; TODO: fn?
  (gen/fmap #(c/with-async-return (c/constantly %))
            item-gen))

(defn focus [item-gen]  ;; TODO lens?
  (gen/fmap #(c/focus c/id-lens %)
            item-gen))

(defn local-state [item-gen] ;; TODO initial-state?
  (gen/fmap #(c/local-state nil %)
            item-gen))

(defn handle-action [item-gen]  ;; TODO: fn?
  (gen/fmap #(c/handle-action % (c/constantly (c/return)))
            item-gen))

(defn capture-state-change [item-gen] ;; TODO: fn?
  (gen/fmap #(c/capture-state-change % (c/constantly (c/return)))
            item-gen))

(defn error-boundary [item-gen] ;; TODO: fn?
  (gen/fmap (fn [i1]
              (c/error-boundary i1 (c/constantly (c/return))))
            item-gen))

(declare with-ref)
(declare node-item)

(let [ids (map c/name-id
               ["a" "b" "c"])]
  (defn named [item-gen]
    (gen/fmap #(apply c/named %)
              (gen/tuple (gen/elements ids)
                         item-gen))))

(defn container-item [item-gen]
  ;; TODO: move those not using item-gen out of here.
  (gen/one-of
   [(dom item-gen)
    (dynamic item-gen)
    (fragment item-gen)
    (keyed non-empty-string node-item) ;; Note: keyed string or fragment is prob. not meaningful?
    (handle-message item-gen) ;; function?

    (named item-gen)
                        
    with-ref
    (with-async-return item-gen)
    (focus item-gen)
    (local-state item-gen)
    (handle-action item-gen)
    
    (capture-state-change item-gen)
    (error-boundary item-gen)
    ;;
    ]))


(def item
  (gen/recursive-gen container-item
                     (gen/one-of [non-empty-string ;; FIXME: empty string can't be found?
                                  once])))

(defn node-container-item [item-gen]
  (dom (gen/one-of [(container-item item-gen)
                    item])))

(def ^{:doc "Generator for an item that renders as at least one dom node."}
  node-item
  (->> (gen/recursive-gen node-container-item
                          (gen/one-of [empty-dom]))
       ;; larger sizes create wayy to much time.
       (gen/scale #(mod % 3))))

(def with-ref
  (gen/fmap (fn [item]
              (c/with-ref (fn [r]
                            ;; TODO: wrap it a little deeper?
                            (c/set-ref item r))))
            node-item))

