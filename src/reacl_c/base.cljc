(ns ^:no-doc reacl-c.base)

(defprotocol E)

(defn element? [v]
  (or (string? v) (satisfies? E v)))

(defn lens? [v]
  (or (ifn? v) (keyword? v) (integer? v)))

(defprotocol Ref
  (-deref-ref [this]))

(defrecord Dynamic [f args] E) ;; aka WithState
(defrecord Focus [e lens] E)
(defrecord HandleAction [e f] E)
(defrecord LocalState [e initial] E)
(defrecord WithRef [f args] E)
(defrecord SetRef [e ref] E)
(defrecord DidMount [return] E)
(defrecord WillUnmount [return] E)
(defrecord DidUpdate [e f] E)
(defrecord WithAsyncActions [f args] E)
(defrecord MonitorState [e f] E)
(defrecord HandleMessage [e f] E)
(defrecord Named [e name] E)
(defrecord ErrorBoundary [e f] E)

(defrecord Keyed [e key] E)
(defrecord Fragment [children] E)

(defn fragment? [v]
  (instance? Fragment v))

(defrecord Returned [opt-state actions messages])

(defn return? [v] (instance? Returned v))

(defprotocol Application
  (-send-message! [this msg]))
