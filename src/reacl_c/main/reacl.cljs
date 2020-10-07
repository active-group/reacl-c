(ns reacl-c.main.reacl
  "Functions for using reacl-c within a Reacl application or library."
  (:require [reacl-c.main :as main]
            [reacl-c.core :as core]
            [active.clojure.lens :as lens]
            [reacl2.core :as reacl :include-macros true]
            [reacl-c.impl.reacl :as impl]))

#_(reacl/defclass ^:private runner this state [item]
  render
  (main/run- dom item onchange onaction)
  )

;; previously named reacl-render
(defn reacl
  "Returns a Reacl component running the given item with the given
  Reacl `state binding`.

  Messages sent to the returned component are passed to the given
  item. Actions emitted from the item are emitted from the returned
  component. Note that this includes effect actions. If you want
  effects to be executed instead, use [[reacl-c.main.execute-effects]]."
  [binding item]
  ;; TODO: can we make it independant from impl?
  #_(react/react-controlled )
  (impl/instantiate binding item))
