(ns clojox.interpreter
  (:require [clojox.environment :as environment]
            [clojox.utils :as utils]
            [clojure.string :as str]
            [clojox.protocols :as protocols :refer [evaluate]]

            [clojox.function :refer [->Function]]
            [clojox.native-functions :as native-functions]
            )
  (:import [jlox Token TokenType ReturnException]))


(defn- stringify
  "Convert the intepreted output to string. Remove '.0' from number."
  [evaluated-expr]
  (cond
    (nil? evaluated-expr) "nil"
    (number? evaluated-expr) (let [s (str evaluated-expr)]
                               (if (str/ends-with? s ".0")
                                 (apply str (drop-last 2 s))
                                 s))
    (satisfies? protocols/ClojoxCallable evaluated-expr) (protocols/to-string evaluated-expr)
    :else (str evaluated-expr)))

(defn- handle-plus
  "If it's a number, it should do summation, if it's a string it should concat them."
  [left right operator]
  (cond
    (and (number? left) (number? right)) (+ left right)
    (and (string? left) (string? right)) (str left right)
    :else (throw (ex-info "Operands must be two numbers or two strings." {:token operator}))))

(defn- assert-number
  "Throws exception if operands aren't numbers."
  [operator operator-fn & operands]
  (when (not-every? number? operands)
    (throw (ex-info (str (if (second operands) "Operands" "Operand")
                         " must be "
                         (if (second operands) "numbers" "a number")
                         ".")
                    {:token operator})))
  (apply operator-fn operands))

(defrecord Literal [value]
  protocols/Evaluate
  (evaluate [_ env]
    [value env]))

(defrecord Grouping [value]
  protocols/Evaluate
  (evaluate [_ env]
    (evaluate value env)))

(defrecord Unary [op right]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[right* env] (evaluate right env)
          result (condp = (.type ^Token op)
                   TokenType/MINUS (assert-number op - right*)
                   TokenType/BANG (not right*))]
      [result env])))

(defrecord Binary [left op right]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[left* env] (evaluate left env)
          [right* env] (evaluate right env)]
      [(case (.name (.type ^Token op))
         "MINUS" (assert-number op - left* right*)
         "SLASH" (assert-number op / left* right*)
         "STAR" (assert-number op * left* right*)
         "PLUS" (handle-plus left* right* op)
         "GREATER" (assert-number op > left* right*)
         "GREATER_EQUAL" (assert-number op >= left* right*)
         "LESS" (assert-number op < left* right*)
         "LESS_EQUAL" (assert-number op <= left* right*)
         "BANG_EQUAL" (not= left* right*)
         "EQUAL_EQUAL" (= left* right*))
       env])))

(defrecord Print [expression]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[value env] (evaluate expression env)]
      (println (stringify value))
      [nil env])))

(defrecord VarStmt [identifier initializer]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[evaluated-stmt env] (if initializer
                                 (evaluate initializer env)
                                 [nil env])]
      [nil (environment/define env identifier evaluated-stmt)])))

(defrecord Variable [identifier]
  protocols/Evaluate
  (evaluate [_ env]
    [(environment/lookup env identifier) env]))

(defrecord Assign [identifier value]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[value* env] (evaluate value env)
          updated-env (environment/assign env identifier value*)]
      [value* updated-env])))

(defrecord Block [statements]
  protocols/Evaluate
  (evaluate [_ env]
    (loop [statements statements
           env (environment/create env)]
      (if (empty? statements)
        [nil (:parent env)]
        (let [[_evaluated-expr env] (evaluate (first statements) env)]
          (recur (rest statements) env))))))

(defrecord If [condition then else]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[result env] (evaluate condition env)
          [_ env] (cond
                    result (evaluate then env)
                    else (evaluate else env)
                    :else [nil env])]
      [nil env])))

(defrecord Logical [left op right]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[left* env] (evaluate left env)]
      (if (= (.type ^Token op) TokenType/OR)
        (if left* ;; For OR, if left is true, return that else evaluate right expr.
          [left* env]
          (evaluate right env))
        (if left* ;; For AND, if left is true, evaluate right else return left.
          (evaluate right env)
          [left* env])))))

(defrecord While [condition body]
  protocols/Evaluate
  (evaluate [_ env]
    (loop [[condition* env] (evaluate condition env)]
      (if condition*
        (let [[_ env] (evaluate body env)]
          (recur (evaluate condition env)))
        [nil env]))))

(defrecord Call [callee paren arguments]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[callee* env] (evaluate callee env)
          args-val (->> arguments
                        (reduce (fn [[args-val env] arg-ast]
                                  (let [[arg-val env] (evaluate arg-ast env)]
                                    [(conj args-val arg-val) env])) [[] env])
                        first)
          args-count (count arguments)]
      (when-not (satisfies? protocols/ClojoxCallable callee*)
        (throw (ex-info "Can only call functions and classes." {:token paren})))

      (when (not= (protocols/arity callee*) args-count)
        (throw (ex-info
                (format "Expected %d arguments but got %d." (protocols/arity callee*)
                        args-count)
                {:token paren})))
      (let [return-value (protocols/call callee* args-val)]
        [return-value env]))))

(defrecord Fun [identifier params body]
  protocols/Evaluate
  (evaluate [_ env]
    [nil (environment/define env identifier (->Function identifier params body env))]))

(defrecord Return [_keyword expr]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[return-value _env] (when expr
                               (evaluate expr env))]
      (throw (ReturnException. return-value)))))

(defn interpret
  [statements]
  (try
    (loop [statements statements
           env (environment/create nil {"clock" (atom native-functions/clock)})]
      (if (empty? statements)
        nil
        (let [[_evaluated-expr env] (evaluate (first statements) env)]
          (recur (rest statements) env))))
    (catch clojure.lang.ExceptionInfo e
      (utils/error (ex-message e) (.line ^Token (:token (ex-data e))))
      70 ;; Exit code for runtime error.
      )))
