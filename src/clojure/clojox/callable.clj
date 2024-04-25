(ns clojox.callable)

(defprotocol ClojoxCallable
  "A protocol to implement callable items like functions and classes."
  (call [this arguments])
  (arity [this])
  (to-string [this]))
