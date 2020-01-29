(ns reacl-c.test-util.core
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [reacl-c.impl.reacl :as impl]
            [clojure.data :as data]
            [active.clojure.lens :as lens]
            [reacl2.core :as rcore :include-macros true]
            [reacl2.test-util.beta :as r-tu]
            [reacl2.test-util.xpath :as r-xpath]))

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

(defn ^:no-doc resolve-element-1
  "Resolves the given element to only dynamic and leaf elements (dom elements, strings, fragments, dynamic and with-*)
  that it renders to for the given state."
  [elem state]
  (if (string? elem)
    elem
    (condp instance? elem
      ;; the dynamics
      base/Dynamic [(apply (:f elem) state (:args elem)) state]
      base/WithRef [(apply (:f elem) dummy-ref (:args elem)) state]
      base/WithAsyncActions [(apply (:f elem) dummy-deliver! (:args elem)) state]

      ;; the wrappers
      base/Focus (recur (:e elem) (yank state (:lens elem)))
      base/HandleAction (recur (:e elem) state)
      base/LocalState (recur (:e elem) [state (:initial elem)])
      base/SetRef (recur (:e elem) state)
      base/Keyed (recur (:e elem) state)
      base/Named (recur (:e elem) state)
      base/ErrorBoundary (recur (:e elem) state) ;; and the error fn?
      base/HandleMessage (recur (:e elem) state)
      base/DidUpdate (recur (:e elem) state)

      base/Fragment [(assoc elem :children (mapv #(resolve-element-1 % state) (:children elem))) state]
      dom/Element   [(assoc elem :children (mapv #(resolve-element-1 % state) (:children elem))) state]

      ;; the leafs
      base/DidMount [core/empty state]
      base/WillUnmount [core/empty state])))

(defn ^:no-doc resolve-element
  "Resolves the given element to only the leaf elements (dom elements and fragments)
  that it renders to for the given state."
  [elem state]
  (loop [[elem state] (resolve-element-1 elem state)]
    (if (string? elem)
      elem
      (condp instance? elem
        ;; the dynamics
        base/Dynamic (recur (resolve-element-1 elem state))
        base/WithRef (recur (resolve-element-1 elem state))
        base/WithAsyncActions (recur (resolve-element-1 elem state))

        base/Fragment elem
        dom/Element   elem))))

(defn ^:no-doc check-pure [elem state]
  ;; Note: ideal performance also depends on our reacl implementation; but for what the user can do, this is even a better test.
  ;; Note: error boundary alternative elements are not checked.
  (let [e1 (resolve-element-1 elem state)
        e2 (resolve-element-1 elem state)]
    (= e1 e2)))

(defn ^:no-doc check-minimal [elem s1 s2]
  ;; Note: only makes sense if check-pure is true
  (assert (not= s1 s2) "States must be different to check.")
  (let [e1 (resolve-element elem s1)
        e2 (resolve-element elem s2)]
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
      (throw (ex-info (str "Performance should be " level ", but was actually :bad.")
                      (let [[element state] @bad-example]
                        ;; Note: with this state, elem resolved to different elements on repeated calls (some (fn) or other side effect.
                        {:state state
                         :element element ;; always the main element currently.
                         :element-diff (data/diff (resolve-element-1 element state) (resolve-element-1 element state))})))

      (and (= level :ideal) (= actual :good))
      (throw (ex-info (str "Performance should be :ideal, but was actually only :good")
                      (let [[element state-1 state-2] @non-ideal-example]
                        ;; Note: on state-1 and state-2, the element looked live the resolved element.    FIXME: but there is also behavior? :-/
                        {:state-1 state-1
                         :state-2 state-2
                         :state-diff (data/diff state-1 state-2)
                         :resolved (resolve-element element state-1)
                         :element element})))

      :else nil)))
