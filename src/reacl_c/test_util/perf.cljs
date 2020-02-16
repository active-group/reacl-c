(ns reacl-c.test-util.perf
  (:require [reacl2.core :as rcore :include-macros true]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [clojure.data :as data]
            [active.clojure.lens :as lens]
            [reacl-c.base :as base]
            cljs.test))

(def ^:private dummy-ref (reify base/Ref
                           (-deref-ref [this] (throw (ex-info "References must only be dereferenced in handlers, not during rendering." {})))))

(defn- dummy-return! [v]
  (throw (ex-info "Asynchronous return delivery must only be done asynchronously, not during rendering." {:value v})))

(defn- yank [state lens]
  (lens/yank state (rcore/lift-lens lens)))

(defn ^:no-doc resolve-1-shallow
  "Shallowly replaces all dynamic items with the items they resolve to for the given state."
  [item state]
  {:post [#(not (instance? base/Dynamic %))
          #(not (instance? base/WithRef %))
          #(not (instance? base/WithAsyncReturn %))]}
  (if (string? item)
    item
    (condp instance? item
      ;; the dynamics
      base/Dynamic (apply (:f item) state (:args item))
      base/WithRef (apply (:f item) dummy-ref (:args item))
      base/WithAsyncReturn (apply (:f item) dummy-return! (:args item))

      item)))

(defn ^:no-doc resolve-*
  [item state resolve-dyn]
  (if (string? item)
    item
    (let [w (fn []
              (update item :e #(resolve-* % state resolve-dyn)))
          wc (fn []
               (update item :children (fn [es]
                                        (mapv #(resolve-* % state resolve-dyn) es))))]
      (condp instance? item
        ;; the dynamics
        base/Dynamic (resolve-dyn item state)
        base/WithRef (resolve-dyn item state)
        base/WithAsyncReturn (resolve-dyn item state)

        ;; the wrappers
        base/Focus (update item :e #(resolve-* % (yank state (:lens item)) resolve-dyn))
        base/LocalState (update item :e #(resolve-* % [state (:initial item)] resolve-dyn))
        
        base/HandleAction (w)
        base/SetRef (w)
        base/Keyed (w)
        base/Named (w)
        base/ErrorBoundary (w) ;; and the error fn?
        base/HandleMessage (w)

        base/Fragment (wc)
        dom/Element   (wc)

        ;; the leafs
        base/Once item))))

(defn ^:no-doc resolve-1
  "Replaces one level of dynamicity from the given item, or just returns it if there is none."
  [item state]
  (resolve-* item state resolve-1-shallow))

(defn ^:no-doc resolve-deep
  "Deeply replaces all dynamic items with the items they resolve to for the given state."
  [item state]
  (resolve-* item state (fn [item state]
                          (resolve-deep (resolve-1 item state) state))))

(defn- map-diff [m1 m2]
  (let [[only-1 only-2 _] (data/diff m1 m2)]
    [only-1 only-2]))

(defn- seq-diff [s1 s2]
  ;; TODO: something else, like [_ _ x _] ...?
  (let [[only-1 only-2 _] (data/diff s1 s2)]
    [only-1 only-2]))

(defn- wrapper-type [t]
  (condp = t
    base/Dynamic 'dynamic
    base/WithRef 'with-ref
    base/WithAsyncReturn 'with-async-return
    base/Focus 'focus
    base/LocalState 'local-state
    base/HandleAction 'handle-action
    base/SetRef 'set-set
    base/Keyed 'keyed
    base/Named 'named
    base/ErrorBoundary 'error-boundary
    base/HandleMessage 'handle-message
    base/Fragment 'fragment
    dom/Element 'element
    base/Once 'once
    (str t)))

(defn ^:no-doc find-first-difference [item1 item2 & [path]]
  ;; returns [path differences] path=[item ...] and differences {:name [left right]}
  (let [t1 (type item1)
        t2 (type item2)
        path (or path [])
        w (fn [e1 e2 e-k res-k]
            (let [path (conj path (wrapper-type (type e1)))]
              (if (= (e-k item1) (e-k item2))
                (find-first-difference (:e item1) (:e item2) path)
                [path {res-k [(e-k item1) (e-k item2)]}])))]
    (cond
      (= item1 item2) nil
      
      (not= t1 t2) [path {:types [t1 t2]}]

      ;; dynamics
      (or (= t1 base/Dynamic) (= t1 base/WithRef) (= t1 base/WithAsyncReturn))
      (let [path (conj path 'dynamic)]
        (if (= (:f item1) (:f item2))
          [path {:arguments (seq-diff (:args item1) (:args item2))}]
          [path {:function [(:f item1) (:f item2)]}]))

      ;; wrappers
      (or (= t1 base/HandleAction) (= t1 base/ErrorBoundary) (= t1 base/HandleStateChange) (= t1 base/HandleMessage))
      (w item1 item2 :f :function)

      (= t1 base/SetRef)
      (w item1 item2 :ref :reference)

      (= t1 base/Focus)
      (w item1 item2 :lens :lens)
      
      (= t1 base/Named)
      (let [id1 (:name-id item1)
            id2 (:name-id item2)]
        (assert (base/name-id? id1))
        (assert (base/name-id? id2))
        (if (= id1 id2)
          (find-first-difference (:e item1) (:e item2) (conj path (symbol (base/name-id-name id1))))
          (if (= (base/name-id-name id1) (base/name-id-name id2))
            [(conj path 'named) {:name-id [id1 id2]}]
            [(conj path 'named) {:name [(base/name-id-name id1) (base/name-id-name id2)]}])))
      
      (= t1 base/Keyed)
      (w item1 item2 :key :key)

      ;; containers
      (or (= t1 base/Fragment) (= t1 dom/Element))
      (let [cs1 (:children item1)
            cs2 (:children item2)
            in-path path
            path (conj path (if (= t1 dom/Element) (symbol (:type item1)) 'fragment))]
        (cond
          (and (= t1 dom/Element) (not= (:type item1) (:type item2)))
          [in-path {:tag [(:type item1) (:type item2)]}]

          (not= (clojure.core/count cs1) (clojure.core/count cs2))
          ;; could look for keys, if there is an additional in front or back, etc. here.
          [path {:child-count [(clojure.core/count cs1) (clojure.core/count cs2)]}]

          (and (= t1 dom/Element) (not= (:attrs item1) (:attrs item2)))
          [path {:attributes (map-diff (:attrs item1) (:attrs item2))}]

          (and (= t1 dom/Element) (not= (:events item1) (:events item2)))
          [path {:events (map-diff (:events item1) (:events item2))}]

          (and (= t1 dom/Element) (not= (:ref item1) (:ref item2)))
          [path {:ref [(:ref item1) (:ref item2)]}] ;; the native/raw ref.
              
          :else
          (reduce (fn [res [idx [c1 c2]]]
                    (or res
                        (find-first-difference c1 c2 (conj path idx))))
                  nil
                  (map-indexed vector (map vector cs1 cs2)))))

      ;; leafs
      (= t1 base/Once)
      (if (not= (:f item1) (:f item2))
        [path {:once [(:f item1) (:f item2)]}]
        [path {:once-cleanup [(:cleanup-f item1) (:cleanup-f item2)]}])
      
      :else ;; string?!
      [path {:not= [item1 item2]}])))

(defn ^:no-doc resolve-differences
  "Resolve up to the first difference, returns nil if equal, or a tuple of resolved items."
  [item state]
  ;; Resolving more and more levels of dynamicity, until we find an impure item (or there is nothing dynamic left)
  (loop [e1 (resolve-1 item state)
         e2 (resolve-1 item state)]
    (if (not= e1 e2)
      [e1 e2]
      (let [e1_ (resolve-1 e1 state)
            e2_ (resolve-1 e2 state)]
        (if (and (= e1 e1_)
                 (= e2 e2_))
          nil ;; fully resolved, no differences
          (recur e1_ e2_) ;; search deeper
          )))))

(defn ^:no-doc check-pure
  "Checks if the given item always resolves to the same for the
  given state, i.e. that rendering has no side effects."
  [item state]
  ;; Note: ideal performance also depends on our reacl implementation; but for what the user can do, this is even a better test.
  (nil? (resolve-differences item state)))

(defn ^:no-doc check-minimal
  "Checks if the given item resolves to something different, given the two different states."
  [item s1 s2]
  ;; Note: only makes sense if check-pure is true
  (assert (not= s1 s2) "States must be different to check.")
  (let [e1 (resolve-deep item s1)
        e2 (resolve-deep item s2)]
    (not= e1 e2)))

(defn- performance-check*
  [item state-seq bad-example-f non-ideal-example-f]
  (assert (base/item? item) item)
  (assert (not (empty? state-seq)))
  (loop [state-seq state-seq
         prev-state nil
         minimal? true]
    (if (not (seq state-seq))
      (if minimal? :ideal :good)
      (let [s1 (first state-seq)]
        (if (check-pure item s1)
          (if (and minimal? (some? prev-state) (not= (first prev-state) s1))
            (let [minimal? (check-minimal item (first prev-state) s1)]
              (when (not minimal?)
                (non-ideal-example-f item (first prev-state) s1))
              (recur (rest state-seq)
                     [s1]
                     minimal?))
            (recur (rest state-seq)
                   [s1]
                   minimal?))
          (do (bad-example-f item s1)
              :bad))))))

(defn performance
  "For all the given states, this checks that for the same state, the
  item renders to the equal item; i.e. rendering has no side
  effects. If that it true, it also checks that for different states,
  it renders to different items; i.e. the state is minimal for this
  item. Note that this test makes most sense for 'dynamic'
  items. Returns [:bad _], [:good _] or [:ideal nil], with '_' being
  information about why this conclusion was drawn."
  [item state-seq]
  #_(assert (not (empty? (rest (distinct state-seq)))) "To test test for :ideal performance, at least two different state values are needed.")
  (let [bad-example (atom nil)
        non-ideal-example (atom nil)
        
        actual (performance-check* item state-seq
                                   (fn [item state]
                                     ;; will currently be called at most once.
                                     (reset! bad-example [item state]))
                                   (fn [item state-1 state-2]
                                     ;; will be called multiple time; keep only one for now.
                                     (reset! non-ideal-example [item state-1 state-2])))]
    (cond
      (= actual :bad)
      (let [[item state] @bad-example ;; item = always the main item currently.
            diff (let [[e1 e2] (resolve-differences item state)]
                   (find-first-difference e1 e2))
            ;; Meaning: with this state, item resolved differently on repeated calls, with at least one difference at the specified place
            ;; = (some (fn) or other side effect?)
            cause {:state state
                   :different-at diff}]
        [:bad [cause]])

      (= actual :good)
      (let [[item state-1 state-2] @non-ideal-example
            ;; Meaning: with these states that are different, the item resolved to the same thing. The state could be made smaller.
            ;; TODO: we could try mutation testing; reduce the state, while still resolving to same (and without errors); then say "this part of the state might be unused".
            cause {:state-1 state-1
                   :state-2 state-2
                   :resolved (resolve-deep item state-1)}]
        [:good [cause]])

      (= actual :ideal)
      [:ideal nil]

      :else (assert false actual))))

(defn ^:no-doc level->= [l1 l2]
  (or (= l1 l2)
      (case l1
        :bad false
        :good (= :bad l2)
        :ideal (or (= :good l2) (= :bad l2)))))

(defn performance= [expected-level item state-seq]
  (level->= (first (performance item state-seq))
            expected-level))

(defn ^:no-doc describe-performance-result [expected-level res]
  (when-let [[actual causes] res]
    (cond
      (= actual :bad)
      (let [cause (first causes)
            [path diff] (:different-at cause)
            state (:state cause)]
        (str "Performance should be " (name expected-level) ", but was actually bad. \n"
             "What makes it bad is at\n  " (pr-str path) "\nwhere it differs in \n  " (pr-str diff) " \nfor the state \n  " (pr-str state)))
      (= actual :good)
      (let [cause (first causes)
            state-diff (data/diff (:state-1 cause) (:state-2 cause))]
        (str "Performance should be ideal, but was actually only good. \n"
             ;; TODO: a 'path' into the state difference or something?
             "What prevents an ideal performance, is that states with a difference of \n" (pr-str state-diff) " \nthe item looks and behaves the same."))

      (= actual :ideal)
      (str "Performance is ideal")
      
      :else (assert false actual))))
