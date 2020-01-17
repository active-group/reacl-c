(ns reacld.core)

(defmacro defn-dynamic [name state args & body]
  `(let [f# (fn [~state ~@args]
              ~@body)]
     (defn ~name [& args#]
       (apply reacld.core/dynamic f# args#))))

(defmacro def-dynamic [name state & body]
  `(let [f# (fn [~state]
              ~@body)]
     (def ~name
       (reacld.core/dynamic f#))))

(defmacro defn-interactive [name state set-state args & body]
  `(let [f# (fn [~state ~set-state ~@args]
              ~@body)]
     (defn ~name [& args#]
       (apply reacld.core/interactive f# args#))))

(defmacro def-interactive [name state set-state & body]
  `(let [f# (fn [~state ~set-state]
              ~@body)]
     (def ~name
       (reacld.core/interactive f#))))

(defmacro defn-effect [name args & body]
  `(let [eff# (fn ~args ~@body)]
     (defn ~name [& args#]
       (apply reacld.core/effect-action eff# args#))))

(defmacro defn-subscription [name deliver! args & body]
  ;; TODO: rename; 'external-source'? 'primitive'?
  `(let [f# (fn [~deliver! ~@args] ~@body)]
     (defn ~name [& args#]
       (apply reacld.core/subscribe f# args#))))

