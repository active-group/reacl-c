(ns ^:deprecated reacl-c.test-util.test-renderer
  (:require [reacl-c.core :as core]
            [reacl-c.base :as base]
            [reacl-c.impl.tu-reacl :as impl]
            [reacl-c.test-util.item-generators :as item-gen]
            [clojure.test.check.generators :as gen]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [find]))

(defn env
  "Returns a new test environment to test the behavior of the given item."
  [item & [options]]
  (impl/env item options))

(defn get-components [env]
  (impl/get-components env))

(defn get-component [env]
  (let [cs (get-components env)]
    (cond
      (empty? cs) (do (assert false "empty env") nil)
      (empty? (rest cs)) (first cs)
      :else
      (do (assert false "no unique toplevel component - is it a fragment?")
          nil))))

;; TODO: helpers for when something cannot be found? (assert-exprs maybe?)

(defn- ensure-some [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) (first lst)
    :else (throw (js/Error. "More than one node matches the given path."))))

(defn find-all [env item]
  (impl/find-all env item))

(defn find [env item]
  (ensure-some (find-all env item)))

(defn find-all-named [env thing] ;; TODO: remove?
  (impl/find-all-named env thing))

(defn find-named [env thing] ;; TODO: remove?
  (ensure-some (find-all-named env thing)))

(defn describe-failed-find [env item]
  ;; this is still to be considered 'experimental'; hard to given a "good" tip
  
  (assert (nil? (find env item)))
  ;; TODO: if 'smaller-than' works correctly, then the limit of 1000 should not be necessary.
  ;; FIXME: how to set seed for sample-seq? might need to make our own via 'generate'.
  (loop [n 0
         smaller-list (take 1000 (gen/sample-seq (item-gen/smaller-than item)))]
    (if (or (empty? smaller-list) (= core/empty (first smaller-list)))
      nil ;; "Could not find out why."
      (if (find env (first smaller-list))
        ;; TODO: an item pretty print would be nice.
        ;; TODO: or use find-first-different between the first that can be found?
        (str "I could find the following item though: " (pr-str (first smaller-list)))
        (recur (inc n) (rest smaller-list))))))

(defn with-env-return [env f]
  (impl/with-env-return env f))

(defn mount!
  "Mounts the item of the given test environment with the given
  state, and returns actions and maybe a changed state."
  [env state]
  (impl/mount! env state))

(defn update!
  "Updates the state of the item of the given test environment, and
  returns actions and maybe a changed state."
  [env state]
  (impl/update! env state))

(defn unmount!
  "Unmounts the item of the given test environment, and return
  actions and maybe a changed state."
  [env]
  (impl/unmount! env))

(defn push!
  "Apply the state change in the given 'return' value, if there is
  any, and merge the return value resulting from that - pushing the
  update cycle one turn."
  [env ret]
  (let [st (base/returned-state ret)]
    (if (not (= base/keep-state st))
      (base/merge-returned ret (update! env st))
      ret)))

(def ^{:deprecated true :dynamic true} *max-update-loops* 100)

(defn push!!
  "Recursively apply the state change in the given 'return' value,
  until the state does not change anymore."
  [env ret]
  (loop [r ret
         state base/keep-state
         n 1]
    (when (> n *max-update-loops*)
      (throw (ex-info "Component keeps on updating. Check the livecylcle methods, which should eventually reach a fixed state." {:intermediate-state state})))
    (let [st (base/returned-state r)]
      (if (not= state st)
        (recur (push! env r) st (inc n))
        r))))

(defn update!!
  "Updates the state of the item of the given test environment, and
  if the state is changed in reaction to that, then keeps on updating
  it. Returns actions and the final changed state, if it was changed
  at all. Throws if there are more than *max-update-loops* recursions,
  which are a sign for bug in the item."
  [env state]
  (->> (update! env state)
       (push!! env)))

(defn mount!!
  "Like [[mount!]], but also recursively update the item to new states that are returned."
  [env state]
  (->> (mount! env state)
       (push!! env)))

(defn unmount!!
  "Like [[unmount!]], but also recursively update the item to new states that are returned."
  [env]
  (->> (unmount! env)
       (push!! env)))

(defn invoke-callback!
  "Invokes the function assiciated with the given `callback` of the
  given test component of a dom element (e.g. `:onclick`), with the
  given event object, and returns a changed app-state and actions from
  the toplevel item, in the form of a `reacl/return` value."
  [comp callback event]
  (impl/invoke-callback! comp callback event))

(defn invoke-callback!!
  "Like [[invoke-callback!]], but also recursively update the item to new states that are returned."
  [comp callback event]
  (impl/invoke-callback!! comp callback event))

(defn inject-return!
  "Does the things that would happen if the given component returned
  the given 'return' value in reaction to some discrete event, and
  returns what would be emitted (state and/or actions) from the tested
  item, in the form of a 'return' value."
  [comp ret]
  (impl/inject-return! comp ret))

(defn inject-return!!
  "Like [[inject-return!]], but also recursively update the item to new states that are returned."
  [comp ret]
  (impl/inject-return!! comp ret))

(defn send-message!
  "Sends a message to the given component or the toplevel component of
  the given test environment, and returns actions and maybe a changed
  state."
  [comp msg]
  (impl/send-message! comp msg))

(defn send-message!!
  "Like [[send-message!]], but also recursively update the item to new states that are returned."
  [comp msg]
  (impl/send-message!! comp msg))

(defn inject-action!
  "Does the things that would happen if the given component emitted
  the given action, and returns what would be emitted (state and/or
  actions) from the tested item, in the form of a 'return' value."
  [comp action]
  (inject-return! comp (core/return :action action)))

(defn inject-action!!
  "Like [[inject-action!]], but also recursively update the item to new states that are returned."
  [comp action]
  (inject-return!! comp (core/return :action action)))

(defn inject-state-change!
  "Does the things that would happen if the given component emitted
  the given new state, and returns what would be emitted (state and/or
  actions) from the tested item, in the form of a 'return' value."
  [comp state]
  (inject-return! comp (core/return :state state)))

(defn inject-state-change!!
  "Like [[inject-state-change!]], but also recursively update the item to new states that are returned."
  [comp state]
  (inject-return!! comp (core/return :state state)))

(defn execute-effect!
  "Executed the given effect in the given test environment, and return
  toplevel changes as a 'return' value."
  [env eff]
  (assert (base/effect? eff) eff)
  (inject-return! (get-component env)
                  (let [[value ret] (base/run-effect! eff)]
                    ret)))
