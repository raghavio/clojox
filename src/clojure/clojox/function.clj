(ns clojox.function
  (:require [clojox.environment :as environment]
            [clojox.protocols :as protocols])
  (:import [jlox ReturnException Token]))

(defrecord Function [identifier params body closure]
  protocols/ClojoxCallable
  (call [this args]
    ;; Function's closure should contain itself for recursion to work.
    ;; Cannot do this during func declaration because then it will reference itself infinitely
    (let [closure (environment/define closure identifier this)
          env (reduce (fn [env i]
                        (environment/define env (nth params i) (nth args i)))
                      closure
                      (range (count params)))]
      (try
        (protocols/evaluate body env)
        nil ;; Return nil
        (catch ReturnException e
          (.value e)))))

  (arity [_]
    (count params))

  (to-string [_]
    (str "<fn " (str (.lexeme ^Token identifier)) ">")))
