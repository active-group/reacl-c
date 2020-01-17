(ns reacld.base)

(defprotocol E
  (-instantiate [this binding])) ;; aka render?

(defprotocol Effect
  (-run-effect! [this]))
