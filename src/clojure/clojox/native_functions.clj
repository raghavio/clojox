(ns clojox.native-functions
  (:require [clojox.protocols :as protocols]))

(def clock
  (reify protocols/ClojoxCallable
    (call [_ _arguments]
      (/ (System/currentTimeMillis) 1000.0))
    (arity [_]
      0)
    (to-string [_]
      "<native fn>")))
