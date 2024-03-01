(ns clojox.interpreter
  (:require [clojox.environment :as environment]
            [clojox.utils :as utils]
            [clojure.string :as str])
  (:import [jlox TokenType]))

(defn- stringify
  "Convert the intepreted output to string. Remove '.0' from number."
  [evaluated-expr]
  (cond
    (nil? evaluated-expr) "nil"
    (number? evaluated-expr) (let [s (str evaluated-expr)]
                               (if (str/ends-with? s ".0")
                                 (apply str (drop-last 2 s))
                                 s))
    :else (str evaluated-expr)))

(defn- handle-plus
  "If it's a number, it should do summation, if it's a string it should concat them."
  [left right]
  (cond
    (and (number? left) (number? right)) (+ left right)
    (and (string? left) (string? right)) (str left right)
    :else
    nil))

(defn- assert-number
  "Throws exception if operands aren't numbers."
  [operator operator-fn & operands]
  (when (not-every? number? operands)
    (throw (ex-info (str (if (second operands) "Operands" "Operand") " must be a number.")
                    {:token operator})))
  (apply operator-fn operands))

(defmulti evaluate (fn [ast _env] (:type ast)))

(defmethod evaluate :literal
  [{:keys [value]}, env]
  [value env])

(defmethod evaluate :grouping
  [{:keys [value]} env]
  (evaluate value env))

(defmethod evaluate :unary
  [{:keys [op right]} env]
  (let [[right* env] (evaluate right env)
        result (condp = (.type op)
                 TokenType/MINUS (- (double right*))
                 TokenType/BANG (not right*))]
    [result env]))

(defmethod evaluate :binary
  [{:keys [left op right]} env]
  (let [[left* env] (evaluate left env)
        [right* env] (evaluate right env)]
    [(case (.name (.type op)) ;; case doesn't work with java enums. Using the .name thing .
       "MINUS" (assert-number op - left* right*)
       "SLASH" (assert-number op / left* right*)
       "STAR" (assert-number op * left* right*)
       "PLUS" (handle-plus left* right*)
       "GREATER" (assert-number op > left* right*)
       "GREATER_EQUAL" (assert-number op >= left* right*)
       "LESS" (assert-number op < left* right*)
       "LESS_EQUAL" (assert-number op <= left* right*)
       "BANG_EQUAL" (not= left* right*)
       "EQUAL_EQUAL" (= left* right*))
     env]))

(defmethod evaluate :print
  [{:keys [expression]} env]
  (println (stringify (first (evaluate expression env))))
  [nil env])

(defmethod evaluate :var-stmt
  [{:keys [identifier initializer]} env]
  (let [evaluated-stmt (when initializer
                         (first (evaluate initializer env)))]
    [nil (environment/define env identifier evaluated-stmt)]))

(defmethod evaluate :variable
  [{:keys [identifier]} env]
  [(environment/lookup env identifier) env])

(defmethod evaluate :assign
  [{:keys [identifier value]} env]
  (let [[value* env] (evaluate value env)]
    [value* (environment/assign env identifier value*)]))

(defmethod evaluate :block
  [{:keys [statements]} env]
  (loop [statements statements
         env (environment/create env)]
    (if (empty? statements)
      [nil (:parent env)]
      (let [[_evaluated-expr env] (evaluate (first statements) env)]
        (recur (rest statements) env)))))

(defmethod evaluate :if
  [{:keys [condition then else]} env]
  (let [[result env] (evaluate condition env)]
    (cond
      result (evaluate then env)
      else (evaluate else env)))
  [nil env])

(defmethod evaluate :logical
  [{:keys [left op right]} env]
  (let [[left* env] (evaluate left env)]
    (if (= (.type op) TokenType/OR)
      (if left* ;; For OR, if left is true, return that else evaluate right expr.
        [left* env]
        (evaluate right env))
      (if left* ;; For AND, if left is true, evaluate right else return left.
        (evaluate right env)
        [left* env]))))

(defmethod evaluate :while
  [{:keys [condition body]} env]
  (loop [[condition* env] (evaluate condition env)]
    (if condition*
      (let [[_ env] (evaluate body env)]
        (recur (evaluate condition env)))
      [nil env])))

(defn interpret
  [statements]
  (loop [statements statements
         env (environment/create nil {})]
    (if (empty? statements)
      nil
      (let [[_evaluated-expr env] (try
                                    (evaluate (first statements) env)
                                    (catch RuntimeException e
                                      (when-let [data (ex-data e)]
                                        (utils/runtime-error (ex-message e) (.line (:token data))))))]
        (recur (rest statements) env)))))
