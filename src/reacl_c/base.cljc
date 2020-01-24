(ns ^:no-doc reacl-c.base)

(defprotocol E)

(defn element? [v]
  (or (string? v) (satisfies? E v)))

(defn lens? [v]
  (or (ifn? v) (keyword? v) (integer? v)))

(defrecord WithState [f args] E)
(defrecord Focus [e lens] E)
(defrecord HandleAction [e f args] E)
(defrecord LocalState [e initial] E)
;; TODO: rename did-mount, will-unmount and did-update ?
(defrecord DidMount [e f args] E)
(defrecord WillUnmount [e f args] E)
(defrecord DidUpdate [e f args] E)
(defrecord WithAsyncActions [f args] E)
(defrecord MonitorState [e f args] E)
(defrecord HandleMessage [e f args] E)
(defrecord Named [e name] E)
(defrecord ErrorBoundary [e f args] E)

(defrecord Keyed [e key] E)
(defrecord Fragment [children] E)

(defrecord Returned [opt-state actions messages])

(defprotocol Application
  (-send-message! [this msg]))
