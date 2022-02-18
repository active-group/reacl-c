(ns reacl-c.test-util.core)

(defmacro provided
  "Replaces the values bound to the given vars during the
  evaluation of `body`, and sets them back to the previous values
  afterwards. Example:

  ```
  (def x 42)
  (provided [x 11]
    (is (= (* x 2) 22)))
  (is (= x 42))
  ```
  "
  [bindings & body]
  (let [pairs (partition 2 bindings)
        olds (gensym "olds")]
    `(let [~olds [~@(map first pairs)]]
       (do ~@(map (fn [[s v]]
                    `(set! ~s ~v))
                  pairs)
           (try (do ~@body)
                (finally
                  (do ~@(map-indexed (fn [i [s v]]
                               `(set! ~s (get ~olds ~i)))
                             pairs))))))))
