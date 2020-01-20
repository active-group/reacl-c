(ns reacld.browser
  (:require [reacld.impl.reacl :as impl]))

(defn run [dom e initial-state]
  (impl/run dom e initial-state))
