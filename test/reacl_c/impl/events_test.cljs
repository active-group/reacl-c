(ns reacl-c.impl.events-test
  (:require [reacl-c.impl.events :as sut]
            [clojure.data :as data]
            [active.clojure.functions :as f]
            [cljs.test :refer (deftest is) :include-macros true]))


(deftest update-bound-event-handlers-test
  (let [changed (fn [state-a events]
                  (let [state @state-a
                        before (sut/get-bound-event-handlers state)
                        n (let [upd (sut/update-bound-event-handlers events identity state)]
                            (js/Object.assign state upd))
                        after (sut/get-bound-event-handlers n)]
                    (reset! state-a n)
                    ;; returns [removed added], resp the key set of those
                    (let [[removed added _] (data/diff before after)]
                      [(set (keys removed)) (set (keys added))])))]
    (let [upd! (partial changed (atom (doto #js {} (sut/init-state!))))]

      (is (= [#{} #{:onTest}]        (upd! {:onTest +})) "added")
      (is (= [#{} #{}]               (upd! {:onTest +})) "unchanged")
      (is (= [#{:onTest} #{:onTest}] (upd! {:onTest -})) "changed")
      (is (= [#{:onTest} #{}]        (upd! {})) "removed")
      )

    ;; nil is like not existing
    (let [upd! (partial changed (atom (doto #js {} (sut/init-state!))))]

      (is (= [#{} #{:onTest}]        (upd! {:onTest +})) "added")
      (is (= [#{:onTest} #{}]        (upd! {:onTest nil})) "removed")
      (is (= [#{} #{:onTest}]        (upd! {:onTest +})) "added back")
      )
    ))

(deftest map-diff-test
  ;; note: 'changed' is removed and added.

  (is (= [{:a 1
           :b 2}
          {:b 3
           :d 5}
          {:c 3}]
         (sut/map-diff {:a 1
                        :b 2
                        :c 3}
                       {:b 3
                        :c 3
                        :d 5})))

  (is (= [{:b 2}
          {}
          {:a 1}]
         (sut/map-diff {:a 1
                        :b 2}
                       {:a 1})))
  
  (is (= [{:a (f/partial + 1)
           :b (f/partial + 2)}
          {:b (f/partial + 3)
           :d (f/partial + 5)}
          {:c (f/partial + 3)}]
         (sut/map-diff {:a (f/partial + 1)
                        :b (f/partial + 2)
                        :c (f/partial + 3)}
                       {:b (f/partial + 3)
                        :c (f/partial + 3)
                        :d (f/partial + 5)}))))
