(ns clojox.interpreter
  (:require [clojox.utils :as utils]
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

(defmulti evaluate :type)

(defmethod evaluate :literal
  [{:keys [value]}]
  value)

(defmethod evaluate :grouping
  [{:keys [value]}]
  (evaluate value))

(defmethod evaluate :unary
  [{:keys [op right]}]
  (let [right* (evaluate right)]
    (condp = (.type op)
      TokenType/MINUS (- (double right*))
      TokenType/BANG (not right*))))

(defmethod evaluate :binary
  [{:keys [left op right]}]
  (let [left* (evaluate left)
        right* (evaluate right)]
    (case (.name (.type op)) ;; case doesn't work with java enums. Using the .name thing .
      "MINUS" (assert-number op - left* right*)
      "SLASH" (assert-number op / left* right*)
      "STAR" (assert-number op * left* right*)
      "PLUS" (handle-plus left* right*)
      "GREATER" (assert-number op > left* right*)
      "GREATER_EQUAL" (assert-number op >= left* right*)
      "LESS" (assert-number op < left* right*)
      "LESS_EQUAL" (assert-number op <= left* right*)
      "BANG_EQUAL" (not= left* right*)
      "EQUAL_EQUAL" (= left* right*))))

(defn interpret
  [expr]
  (try
    (let [evaluated-expr (evaluate expr)]
      (stringify evaluated-expr))
    (catch RuntimeException e
      (when-let [data (ex-data e)]
        (utils/runtime-error (ex-message e) (.line (:token data)))))))
