(ns hooks.reacl-c
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]))

(defn- rewrite-list [expr f]
  ;; rewrite children list of a list-node to a single new node.
  (-> expr
      (update :node
              (fn [node]
                (if (api/list-node? node)
                  (let [cs (:children node)]
                    (let [res (f cs)]
                      (println "xxxxx" cs "=>" res)
                      res))
                  ;; just keep? or an error?
                  (do #_(assert false node) ;; TODO: proper error
                      node))))))

(defn- is-keyword? [node kw]
  (and (api/keyword-node? node)
       (= kw (:k node))
       (not (:namespaced? node))))

(defn- remove-schemas [params]
  (-> (reduce (fn [[res drop-next?] p]
                (if drop-next?
                  [res false]
                  (if (is-keyword? p :-)
                    [res true]
                    [(conj res p) false])))
              [[] false]
              params)
      (first)))

(defn- schema-fn-0 [params & body]
  #_(assert (api/vector-node? params) (pr-str params))
  ;; How to f*ing reuse what there is already for schema.core???
  #_(api/list-node (list* (api/token-node 'schema.core/fn)
                          params
                          body))
  (api/list-node (list* (api/token-node 'fn)
                        (api/vector-node (remove-schemas (:children params)))
                        body)))

(defn- schema-fn-n [params-bodies]
  ;; multi arity
  (api/list-node (list* (api/token-node 'fn)
                        (map (fn [[params body]]
                               (api/list-node (list* (api/vector-node (remove-schemas (:children params)))
                                                     body)))
                             params-bodies))))

(defn- schema-defn-0 [name params & more]
  #_(assert (api/vector-node? params) (pr-str params))
  (api/list-node (list (api/token-node 'def)
                       name
                       (apply schema-fn-0 params more))))

(defn- schema-defn-n [name params-bodies]
  (api/list-node (list (api/token-node 'def)
                       name
                       (schema-fn-n params-bodies))))

(defn- schema-defn [name x & more]
  (if (is-keyword? x :-)
    (apply schema-defn-0 name (rest more))
    (apply schema-defn-0 name (cons x more))))

(defn- drop-docstring [name x & more]
  (if (api/string-node? x)
    (list* name more)
    (list* name (cons x more))))

(defn- as-do [& nodes]
  ;; multiple nodes in a 'do'
  (api/list-node (list* (api/token-node 'do) nodes)))

(defn defn-item [expr]
  (-> expr
      (rewrite-list (fn [children]
                      ;; rewrite node to defn, removing :static
                      (let [[name & r] (apply drop-docstring (rest children))]
                        ;; TODO: maybe register a finding if x is some other keyword than :static or :-
                        (apply schema-defn name (if (is-keyword? (first r) :static)
                                                  (rest r)
                                                  r)))))))

(defn defn-dom [expr]
  ;; basically the same as defn-item, but add arity with one arg less.
  (-> expr
      (rewrite-list (fn [children]
                      ;; rewrite node to defn, removing :static
                      (let [[name & r] (apply drop-docstring (rest children))
                            r (cond
                                (is-keyword? (first r) :static) (rest r)
                                (is-keyword? (first r) :-) (rest (rest r))
                                :else r)
                            [params & body] r]
                        ;; TODO: add finding when no args?
                        (schema-defn-n name
                                       [[params body]
                                        [(api/vector-node (rest (:children params)))
                                         ;; Note: even though the first param (attrs) need not be passed, it is still bound in body
                                         (list (apply schema-fn-0 (api/vector-node (list (first (:children params))))
                                                      body))]]))))))

(defn- with-state-as* [expr]
  (-> expr
      (rewrite-list (fn [children]
                      (let [[binding & body] (rest children)
                            as-fn (fn [params]
                                    (apply schema-fn-0 params body))]
                        (if (and (api/vector-node? binding)
                                 (> (count (:children binding)) 3)
                                 (is-keyword? (nth (:children binding) 2) :local))
                          (let [b (:children binding)]
                            ;; (with-state-as [b0 b1 :local value] & body) => (do value (fn [b0 b1] & body))
                            ;; TODO: warn if length(b) > 4 ?
                            (as-do (nth b 3)
                                   (as-fn (api/vector-node (list (nth b 0) (nth b 1))))))
                          ;; (with-state-as binding & body) => (fn [binding] & body)
                          (as-fn (api/vector-node (list binding)))))))))

(defn with-state-as [expr]
  (with-state-as* expr))

(def ^:private empty-1-arg-fn
  (api/list-node (list (api/token-node 'fn)
                       (api/vector-node (list (api/token-node '_))))))

(defn defn-subscription [expr]
  ;; (defn-subscription name deliver! :- Schema [args] & body)
  (-> expr
      (rewrite-list (fn [children]
                      (let [[name deliver x & r] (apply drop-docstring (rest children))
                            [[deliver _] params body] ;; ignoring deliver schema for now
                            (if (is-keyword? x :-)
                              [(list deliver (nth r 0)) (nth r 1) (rest (rest r))]
                              [(list deliver nil) x r])]
                        (schema-defn name params
                                     (api/list-node (list* (api/token-node 'let)
                                                           (api/vector-node (list deliver empty-1-arg-fn))
                                                           body))))))))
