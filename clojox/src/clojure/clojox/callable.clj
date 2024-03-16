(ns clojox.callable)

(defprotocol ClojoxCallable
  "A protocol to implement callable items like functions and classes."
  (call [this func-name-identifier arguments calling-env])
  (arity [this])
  (to-string [this]))
