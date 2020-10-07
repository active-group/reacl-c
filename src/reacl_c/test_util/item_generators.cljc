(ns reacl-c.test-util.item-generators
  (:require [clojure.test.check.generators :as gen]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.base :as base]
            [clojure.set :as set])
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
              (c/dynamic (f/constantly it)))
            item-gen))

(defn static [item-gen]
  (gen/fmap (fn [it]
              (c/static (f/constantly it)))
            item-gen))

(defn keyed [key-gen item-gen]
  (gen/fmap (fn [[s it]]
              (c/keyed it s))
            (gen/tuple key-gen item-gen)))

(defn handle-message [item-gen]  ;; TODO: fn?
  (gen/fmap (fn [it]
              (c/handle-message (f/constantly (c/return))
                                it))
            item-gen))

(defn fragment [item-gen]
  (gen/fmap #(apply c/fragment %)
            (gen/list item-gen)))

(def lifecycle
  (gen/fmap #(apply c/lifecycle %)
            (gen/tuple (gen/elements [(f/constantly (c/return))])
                       (gen/elements [(f/constantly (c/return))]))))

(defn with-async-return [item-gen]  ;; TODO: fn?
  (gen/fmap #(c/with-async-return (f/constantly %))
            item-gen))

(defn focus [item-gen]  ;; TODO lens?
  (gen/fmap #(c/focus lens/id %)
            item-gen))

(defn local-state [item-gen] ;; TODO initial-state?
  (gen/fmap #(c/local-state nil %)
            item-gen))

(defn handle-action [item-gen]  ;; TODO: fn?
  (gen/fmap #(c/handle-action % (f/constantly (c/return)))
            item-gen))

(defn handle-state-change [item-gen] ;; TODO: fn?
  (gen/fmap #(c/handle-state-change % (f/constantly (c/return)))
            item-gen))

(defn handle-error [item-gen] ;; TODO: fn?
  (gen/fmap (fn [i1]
              (c/handle-error i1 (f/constantly (c/return))))
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
    (static item-gen)
    (fragment item-gen)
    (keyed non-empty-string node-item) ;; Note: keyed string or fragment is prob. not meaningful?
    (handle-message item-gen) ;; function?

    (named item-gen)
                        
    with-ref
    (with-async-return item-gen)
    (focus item-gen)
    (local-state item-gen)
    (handle-action item-gen)
    
    (handle-state-change item-gen)
    (handle-error item-gen)
    ;;
    ]))


(def item
  (gen/recursive-gen container-item
                     (gen/one-of [non-empty-string ;; FIXME: empty string can't be found?
                                  lifecycle])))

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
                            (c/refer item r))))
            node-item))

;;;;;;;;;;;;;;;;;;;;

(declare smaller-than)

(defn- permutations [lst]
  (if (empty? lst)
    []
    (let [[v & more] lst
          r (permutations more)]
      (concat (list (list v))
              (map #(cons v %) r)
              r))))

(defn- list-of-smaller-items [items]
  (assert (not-empty items))
  (apply gen/tuple (map smaller-than items)))

(defn- smaller-list [lst]
  (assert (not-empty lst))
  ;; remove a random number of any list items.
  ;; can get quite heavy with longer lists...
  (gen/bind (gen/choose 1 (count lst))
            (fn [cnt]
              (let [all-indices (set (range (count lst)))
                    indices (filter #(= (count %) cnt)
                                    (permutations all-indices))]
                (gen/elements (map (fn [is]
                                     (let [keep-is (set/difference all-indices is)]
                                       (map #(nth lst %) keep-is)))
                                   (map set indices)))))))

(defn- smaller-map [mp]
  (assert (not-empty mp))
  (gen/fmap (fn [items]
              (into {} items))
            (smaller-list (seq mp))))

(def ^:private f-empty (f/constantly c/empty))
(def ^:private void (gen/elements [c/empty])) ;; should be 'nothing' really; but empty generator not possible?
(def ^:private empty-item (gen/elements [c/empty]))

(defn- change-score [changes]
  (apply + (map (fn [c]
                  ;; meaning: first change smth in children, then remove events, then attributes, then whole children.
                  (case c
                    :cc 1
                    :c 4
                    :a 3
                    :e 2))
                changes)))

(defn smaller-than [item]
  (let [wr (fn [item]
             (if (= c/empty (:e item))
               empty-item
               (gen/fmap #(assoc item :e %)
                         (smaller-than (:e item)))))]
    (if (string? item)
      empty-item
      (condp apply [item]
        base/dynamic? (if (= (:f item) f-empty)
                        empty-item
                        (gen/elements [(c/dynamic f-empty)]))
        base/fragment? (let [children (remove #(= c/empty %) (:children item))]
                         (if (empty? (:children item))
                           void
                           (gen/fmap #(apply c/fragment %)
                                     (gen/one-of [(list-of-smaller-items children)
                                                  (smaller-list children)]))))
        dom/element? (let [children (remove #(= c/empty %) (:children item))
                           attrs (:attrs item)
                           events (:events item)

                           changes_ (map set
                                         (permutations (cond->> nil
                                                         (not-empty children) (concat (list :c :cc))
                                                         (not-empty attrs) (cons :a)
                                                         (not-empty events) (cons :e))))
                           ;; move around the potential changes - doing 'smaller' changes first.
                           ;; Note: might be superfluous, as 'one-of' does not try in order - TODO: is there an alternative?
                           changes (sort-by change-score changes_)]
                       (if (empty? changes)
                         void
                         ;; generate any non-empty combination of children, event and attr changes:
                         (gen/one-of
                          (map (fn [change]
                                 (assert (not-empty change))
                                 (cond-> (gen/return item)
                                   (:c change) (as-> $
                                                   (gen/fmap (fn [[item v]]
                                                               (assoc item :children v))
                                                             (gen/tuple $ (smaller-list children))))
                                   (:cc change) (as-> $
                                                    (gen/fmap (fn [[item v]]
                                                                (assoc item :children v))
                                                              (gen/tuple $ (list-of-smaller-items children))))
                                   (:a change) (as-> $
                                                   (gen/fmap (fn [[item v]]
                                                               (assoc item :attrs v))
                                                             (gen/tuple $ (smaller-map attrs))))
                                   (:e change) (as-> $
                                                   (gen/fmap (fn [[item v]]
                                                               (assoc item :events v))
                                                             (gen/tuple $ (smaller-map events))))))
                               changes))))
      
        base/with-ref? empty-item
        base/with-async-return? empty-item
        base/focus? (wr item)
        base/local-state? (wr item)
        base/handle-action? (wr item)
        base/refer? (wr item)
        base/keyed? (wr item)
        base/named? (wr item)
        base/handle-error? (wr item)
        base/handle-message? (wr item)
        base/lifecycle? empty-item
        
        ;; anything unknown:
        empty-item))))
