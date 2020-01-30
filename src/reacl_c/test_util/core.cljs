(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [clojure.data :as data]
            [active.clojure.lens :as lens]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath])
  (:refer-clojure :exclude [resolve]))

;; Note: just reusing rcore/test-util is not a good fit, esp. because
;; when/if xpath reveals much of the internals (dom class wrapper,
;; other classes that are an implementation detail). Maybe this should
;; be replaced by our own simulator.

(defn- ->ret [r]
  ;; reacl return => reacl-c return
  (apply core/return
         (apply concat
                (let [s (rcore/returned-app-state r)]
                  (when-not (rcore/keep-state? s)
                    [:state s]))
                (map (fn [a]
                       [:action a])
                     (rcore/returned-actions r)))))

(defn env
  "Returns a new test environment to test the behavior of the given element."
  [element & [options]]
  ;; Note: this tests elements using their Reacl implementation, and
  ;; ultimately Reacts test-renderer.
  (let [class (rcore/class "env" this state []
                           refs [child]
                           handle-message (fn [msg]
                                            (rcore/return :message [(rcore/get-dom child) msg]))
                           render (-> (impl/instantiate (rcore/bind this) element)
                                      (rcore/refer child)))]
    (r-tu/env class options)))

(def get-component r-tu/get-component)

(defn mount!
  "Mounts the element of the given test environment with the given
  state, and returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/mount! env state)))

(defn update!
  "Updates the state of the element of the given test environment, and
  returns actions and maybe a changed state."
  [env state]
  (->ret (r-tu/update! env state)))

(def ^:dynamic *max-update-loops* 100)

(defn update!!
  "Updates the state of the element of the given test environment, and
  if the state is changed in reaction to that, then keeps on updating
  it. Returns actions and the final changed state, if it was changed
  at all. Throws if there are more than *max-update-loops* recursions,
  which are a sign for bug in the element."
  [env state]
  (loop [r (core/return)
         state state
         n 1]
    (let [r2 (update! env state)
          rm (base/merge-returned r r2)]
      (if (:opt-state r2)
        (let [state (first (:opt-state r2))]
          (when (> n *max-update-loops*)
            (throw (ex-info "Elements keeps on updating. Check any [[did-update]] elements, which should eventually reach a fixed state." {:intermediate-state state})))
          (recur rm state (inc n)))
        rm))))

(defn unmount!
  "Unmounts the element of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (->ret (r-tu/unmount! env)))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  {:per [(some? comp)]}
  ;; TODO: better check the comp? sending a message to a fragment/dom/string element gives weird reacl errors.
  (->ret (r-tu/send-message! comp msg)))

(defn invoke-callback! [comp callback event]
  ;; TODO: enable this on dom class elements?! then we can remove the xpath case for the raw dom element.
  (->ret (r-tu/invoke-callback! comp callback event)))

(defn inject-action! [comp action]
  ;; Note: for dom tags in an xpath, the user will find the native element; so actions cannot be injected into them :-/
  ;; Rule: dom element => invoke-callback; non-dom elements => inject-action + inject-state-change.
  ;; TODO: document that, if it cannot be changed.
  (->ret (r-tu/inject-return! comp (rcore/return :action action))))

(defn inject-state-change! [comp state]
  (->ret (r-tu/inject-change! comp state)))

(def ^:private dummy-ref (reify base/Ref
                           (-deref-ref [this] (throw (ex-info "References must only be dereferenced in handlers, not during rendering." {})))))

(defn- dummy-deliver! [v]
  (throw (ex-info "Asynchronous action delivery must only be done asynchronously, not during rendering." {:action v})))

(defn- yank [state lens]
  ;; TODO: add a way in reacl or core, to do a manual focus (respecting keywords and numbers; or add numbers to active.clojure?)
  (if (integer? lens)
    (first (drop lens state))
    (lens/yank state lens)))

;; Note: part of the resolve-x functions could probably defined via this fold; but blows my mind currently.
#_(defn ^:no-doc fold-element
  [init elem dynamics wrapper container leaf]
  (let [rec (fn [init elem]
              (fold-element init elem dynamics wrapper container leaf))
        w (fn [elem]
            (wrapper (rec init (:e elem)) elem))
        wc (fn [elem]
             ;; map or fold?
             (container init elem (map (partial rec init) (:children elem))))]
    (if (string? elem)
      (leaf init elem)
      (condp instance? elem
        ;; the dynamics
        base/Dynamic (dynamics init elem)
        base/WithRef (dynamics init elem)
        base/WithAsyncActions (dynamics init elem)

        ;; the wrappers
        base/Focus (w elem)
        base/HandleAction (w elem)
        base/LocalState (w elem)
        base/SetRef (w elem)
        base/Keyed (w elem)
        base/Named (w elem)
        base/ErrorBoundary (w elem) ;; and the error fn?
        base/HandleMessage (w elem)
        base/DidUpdate (w elem)

        base/Fragment (wc elem)
        dom/Element   (wc elem)

        ;; the leafs
        base/DidMount (leaf init elem)
        base/WillUnmount (leaf init elem)))))

(defn ^:no-doc resolve-1-shallow
  "Shallowly replaces all dynamic elements with the elements they resolve to for the given state."
  [elem state]
  {:post [#(not (instance? base/Dynamic %))
          #(not (instance? base/WithRef %))
          #(not (instance? base/WithAsyncActions %))]}
  (if (string? elem)
    elem
    (condp instance? elem
      ;; the dynamics
      base/Dynamic (apply (:f elem) state (:args elem))
      base/WithRef (apply (:f elem) dummy-ref (:args elem))
      base/WithAsyncActions (apply (:f elem) dummy-deliver! (:args elem))

      elem)))

(defn ^:no-doc resolve-*
  [elem state resolve-dyn]
  (if (string? elem)
    elem
    (let [w (fn []
              (update elem :e #(resolve-* % state resolve-dyn)))
          wc (fn []
               (update elem :children (fn [es]
                                        (mapv #(resolve-* % state resolve-dyn) es))))]
      (condp instance? elem
        ;; the dynamics
        base/Dynamic (resolve-dyn elem state)
        base/WithRef (resolve-dyn elem state)
        base/WithAsyncActions (resolve-dyn elem state)

        ;; the wrappers
        base/Focus (update elem :e #(resolve-* % (yank state (:lens elem)) resolve-dyn))
        base/LocalState (update elem :e #(resolve-* % [state (:initial elem)] resolve-dyn))
        
        base/HandleAction (w)
        base/SetRef (w)
        base/Keyed (w)
        base/Named (w)
        base/ErrorBoundary (w) ;; and the error fn?
        base/HandleMessage (w)
        base/DidUpdate (w)

        base/Fragment (wc)
        dom/Element   (wc)

        ;; the leafs
        base/DidMount elem
        base/WillUnmount elem))))

(defn ^:no-doc resolve-1
  "Replaces one level of dynamicity from the given elemt, or just returns it if there is none"
  [elem state]
  (resolve-* elem state resolve-1-shallow))

(defn ^:no-doc resolve-deep
  "Deeply replaces all dynamic elements with the elements they resolve to for the given state."
  [elem state]
  (resolve-* elem state (fn [elem state]
                          (resolve-deep (resolve-1 elem state) state))))

(defn ^:no-doc check-pure
  "Checks if the given element always resolves to the same for the
  given state, i.e. that rendering has no side effects."
  [elem state]
  ;; Note: ideal performance also depends on our reacl implementation; but for what the user can do, this is even a better test.
  ;; Note: error boundary alternative elements are not checked.
  ;; Resolving more and more levels of dynamicity, until we find an impure element (or there is nothing dynamic left)
  (loop [e1 (resolve-1 elem state)
         e2 (resolve-1 elem state)]
    (and (= e1 e2)
         (let [e1_ (resolve-1 e1 state)
               e2_ (resolve-1 e2 state)]
           ;; either nothing more to resolve, or recur
           (or (and (= e1 e1_)
                    (= e2 e2_))
               (recur e1_ e2_))))))

(defn ^:no-doc check-minimal
  "Checks if the given element resolves to something different, given the two different states."
  [elem s1 s2]
  ;; Note: only makes sense if check-pure is true
  (assert (not= s1 s2) "States must be different to check.")
  (let [e1 (resolve-deep elem s1)
        e2 (resolve-deep elem s2)]
    (not= e1 e2)))

(defn- performance-check*
  [element state-seq bad-example-f non-ideal-example-f]
  (assert (base/element? element))
  (assert (not (empty? state-seq)))
  (loop [state-seq state-seq
         prev-state nil
         minimal? true]
    (if (not (seq state-seq))
      (if minimal? :ideal :good)
      (let [s1 (first state-seq)]
        (if (check-pure element s1)
          (if (and minimal? (some? prev-state) (not= (first prev-state) s1))
            (let [minimal? (check-minimal element (first prev-state) s1)]
              (when (not minimal?)
                (non-ideal-example-f element (first prev-state) s1))
              (recur (rest state-seq)
                     [s1]
                     minimal?))
            (recur (rest state-seq)
                   [s1]
                   minimal?))
          (do (bad-example-f element s1)
              :bad))))))

(defn performance-check
  "For all the given states, this checks that for the same state, the
  elements renders to the equal element; i.e. rendering has no side
  effects. If that it true, it also checks that for different states,
  it renders to different elements; i.e. the state is minimal for this
  element. Note that this test makes most sense for 'dynamic'
  elements. Returns :bad, :good, :ideal depending on these results."
  [element state-seq]
  (performance-check* element state-seq
                      (fn [& args])
                      (fn [& args])))

(defn verify-performance! [level element state-seq]
  (assert (base/element? element))
  (assert (or (= level :good) (= level :ideal)))
  (assert (or (= level :good) (not (empty? (rest state-seq)))) "To test test for :ideal performance, at least two state values are needed.")
  (let [bad-example (atom nil)
        non-ideal-example (atom nil)
        
        actual (performance-check* element state-seq
                                   (fn [element state]
                                     ;; will currently be called at most once.
                                     (reset! bad-example [element state]))
                                   (fn [element state-1 state-2]
                                     ;; will be called multiple time; keep only one for now.
                                     (reset! non-ideal-example [element state-1 state-2])))]
    (cond
      (= actual :bad)
      ;; TODO: add a 'path of named and dom tag to the first difference' and what is different.
      (throw (ex-info (str "Performance should be " level ", but was actually :bad.")
                      (let [[element state] @bad-example]
                        ;; Note: with this state, elem resolved to different elements on repeated calls (some (fn) or other side effect?)
                        {:state state
                         :element element ;; always the main element currently.
                         :element-diff (data/diff (resolve-1 element state) (resolve-1 element state))
                         })))

      (and (= level :ideal) (= actual :good))
      (throw (ex-info (str "Performance should be :ideal, but was actually only :good")
                      (let [[element state-1 state-2] @non-ideal-example]
                        ;; Note: on state-1 and state-2, the element always looks and behaves the same - like 'resolved'
                        {:state-1 state-1
                         :state-2 state-2
                         :state-diff (data/diff state-1 state-2)
                         :resolved (resolve-deep element state-1)
                         :element element})))

      :else nil)))
