(ns clojox.function
  (:require [clojox.evaluate :refer [evaluate]]
            [clojox.environment :as environment]
            [clojox.callable :refer [ClojoxCallable]]))

(defrecord Function [fn-ast closure]
  ClojoxCallable
  (call [_ arguments]
    (let [params-vals (map vector (:params fn-ast) arguments)
          env (environment/create closure)
          env (reduce (fn [env [param arg]]
                        (environment/define env param arg)) env params-vals)
          return-value (try
                         (first (evaluate (:body fn-ast) env))
                         (catch Exception e
                           (when-let [return-value (-> e ex-data :return-value)]
                             return-value)))]
      return-value))

  (arity [_]
    (count (:params fn-ast)))

  (to-string [_]
    (str "<fn " (.lexeme (:identifier fn-ast)) ">")))
