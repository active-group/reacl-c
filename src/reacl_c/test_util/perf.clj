(ns reacl-c.test-util.perf
  (:require [cljs.test :as cst]))

(defn- assert-performance [_env ?msg ?form]
  (let [[_ level item state-seq] ?form]
    `(let [level# ~level]
       (let [res# (performance ~item ~state-seq)
             actual# (first res#)]
         (if (level->= (first res#) level#)
           (cst/do-report {:type :pass})
           (let [message# (describe-performance-result level# res#)]
             (cst/do-report {:type :fail
                             :message (or ~?msg message#)
                             :expected level#
                             :actual actual#})))))))

;; Note: cljs.test does not expand the the symbol; so it has to be written exacly like the method matcher :-/

(defmethod cst/assert-expr 'reacl-c.test-util.perf/performance= [_env ?msg ?form]
  (assert-performance _env ?msg ?form))

(defmethod cst/assert-expr 'perf/performance= [_env ?msg ?form]
  (assert-performance _env ?msg ?form))
