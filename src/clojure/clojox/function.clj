(ns clojox.function
  (:require [clojox.evaluate :refer [evaluate]]
            [clojox.environment :as environment]
            [clojox.callable :refer [ClojoxCallable]])
  (:import [jlox Return]))

(defrecord Function [fn-ast closure]
  ClojoxCallable
  (call [this arguments]
    (let [params-vals (map vector (:params fn-ast) arguments)
          fn-identifier (get-in this [:fn-ast :identifier])
          ;; Function's closure should contain itself for recursion to work.
          ;; Cannot do this during func declaration because then it will reference itself infinitely
          closure (environment/define closure fn-identifier this)
          env (environment/create closure)
          env (reduce (fn [env [param arg]]
                        (environment/define env param arg)) env params-vals)]
      (try
        (first (evaluate (:body fn-ast) env))
        (catch Return e
          (.value e)))))

  (arity [_]
    (count (:params fn-ast)))

  (to-string [_]
    (str "<fn " (.lexeme (:identifier fn-ast)) ">")))
