(ns clojox.native-functions
  (:require [clojox.callable :refer [ClojoxCallable]]))

(def clock
  (reify ClojoxCallable
    (call [_ _arguments]
      (/ (System/currentTimeMillis) 1000))
    (arity [_]
      0)
    (to-string [_]
      "<native fn>")))
