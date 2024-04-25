(ns clojox.function
  (:require [clojox.environment :as environment]
            [clojox.protocols :as protocols])
  (:import [jlox ReturnException]))

(defrecord Function [identifier params body closure]
  protocols/ClojoxCallable
  (call [this arguments]
    (let [params-vals (map vector params arguments)
          ;; Function's closure should contain itself for recursion to work.
          ;; Cannot do this during func declaration because then it will reference itself infinitely
          closure (environment/define closure identifier this)
          env (environment/create closure)
          env (reduce (fn [env [param arg]]
                        (environment/define env param arg)) env params-vals)]
      (try
        (first (protocols/evaluate body env))
        (catch ReturnException e
          (.value e)))))

  (arity [_]
    (count params))

  (to-string [_]
    (str "<fn " (.lexeme identifier) ">")))
