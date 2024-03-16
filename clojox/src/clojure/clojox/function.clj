(ns clojox.function
  (:require [clojox.evaluate :refer [evaluate]]
            [clojox.environment :as environment]
            [clojox.callable :refer [ClojoxCallable]]))

(defrecord Function [fn-ast closure]
  ClojoxCallable
  ;; calling-env is used to return the
  (call [this func-name-identifier arguments calling-env]
    (let [params-vals (map vector (:params fn-ast) arguments)
          env (environment/create closure)
          env (reduce (fn [env [param arg]]
                        (environment/define env param arg)) env params-vals)
          [return-value env] (try
                               (evaluate (assoc (:body fn-ast) :calling-env calling-env) env)
                               (catch Exception e
                                 ;; if there is a return statement, return the value and parent env
                                 ;; of this function because we are exiting the block abruptly.
                                 (when-let [return-value (-> e ex-data :return-value)]
                                   [return-value (-> e ex-data :env)])))
          updated-record (assoc this :closure (dissoc env :calling-env))]
[return-value (environment/assign (:calling-env env) func-name-identifier  updated-record)]))

  (arity [this]
    (count (:params fn-ast)))

  (to-string [this]
    (str "<fn " (.lexeme (:identifier fn-ast)) ">")))
