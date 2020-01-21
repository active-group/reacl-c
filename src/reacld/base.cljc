(ns reacld.base)

(defprotocol E)

(defrecord WithState [f args] E)
(defrecord Focus [e lens] E)
(defrecord HandleAction [e f args] E)
(defrecord LocalState [e initial] E)
;; TODO: rename did-mount, will-unmount and did-update ?
(defrecord WhenMounted [e f args] E)
(defrecord WhenUnmounting [e f args] E)
(defrecord AfterUpdate [e f args] E)
(defrecord WithAsyncActions [f args] E)
(defrecord MonitorState [e f args] E)
(defrecord HandleMessage [e f args] E)

(defrecord Keyed [e key] E)
(defrecord Fragment [children] E)

(defprotocol Effect
  (-run-effect! [this]))

(defrecord Returned [opt-state actions messages])

(defprotocol Application
  (-send-message! [this msg]))
