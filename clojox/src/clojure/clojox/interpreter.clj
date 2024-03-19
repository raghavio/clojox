(ns clojox.interpreter
  (:require [clojox.environment :as environment]
            [clojox.utils :as utils]
            [clojure.string :as str]
            [clojox.callable :as callable :refer [ClojoxCallable]]
            [clojox.function :refer [->Function]]
            [clojox.evaluate :refer [evaluate]])
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
    (satisfies? ClojoxCallable evaluated-expr) (callable/to-string evaluated-expr)
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
                 TokenType/MINUS (assert-number op - right*)
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
       "PLUS" (handle-plus left* right* op)
       "GREATER" (assert-number op > left* right*)
       "GREATER_EQUAL" (assert-number op >= left* right*)
       "LESS" (assert-number op < left* right*)
       "LESS_EQUAL" (assert-number op <= left* right*)
       "BANG_EQUAL" (not= left* right*)
       "EQUAL_EQUAL" (= left* right*))
     env]))

(defmethod evaluate :print
  [{:keys [expression]} env]
  (let [[value env] (evaluate expression env)]
    (println (stringify value))
    [nil env]))

(defmethod evaluate :var-stmt
  [{:keys [identifier initializer]} env]
  (let [[evaluated-stmt env] (if initializer
                               (evaluate initializer env)
                               [nil env])]
    [nil (environment/define env identifier evaluated-stmt)]))

(defmethod evaluate :variable
  [{:keys [identifier]} env]
  [(environment/lookup env identifier) env])

(defmethod evaluate :assign
  [{:keys [identifier value]} env]
  (let [[value* env] (evaluate value env)
        updated-env (environment/assign env identifier value*)
        updated-env (if false #_(contains? env :calling-env)
                      (assoc updated-env :calling-env (environment/assign (:calling-env env) identifier value*))
                      updated-env)]
    [value* updated-env]))

(defmethod evaluate :block
  [{:keys [statements calling-env]} env]
  (loop [statements statements
         env (if calling-env
               (assoc (environment/create env) :calling-env calling-env)
               (environment/create env))]
    (if (empty? statements)
      [nil (assoc (:parent env) :calling-env (:calling-env env))]
      (let [[_evaluated-expr env] (evaluate (first statements) env)]
        (recur (rest statements) env)))))

(defmethod evaluate :if
  [{:keys [condition then else]} env]
  (let [[result env] (evaluate condition env)
        [_ env] (cond
                  result (evaluate then env)
                  else (evaluate else env))]
    [nil env]))

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

(defmethod evaluate :call
  [{:keys [callee paren arguments]} env]
  (let [[callee* env] (evaluate callee env) ;; callee is a var that binds to a ClojoxCallable object
        args-val (->> arguments ;; The env from one eval should get passed to next.
                      (reduce (fn [[args-val env] arg-ast]
                                  (let [[arg-val env] (evaluate arg-ast env)]
                                    [(conj args-val arg-val) env])) [[] env])
                      first)
        func-name-identifier (:identifier callee)
        args-count (count arguments)]
    (when-not (satisfies? ClojoxCallable callee*)
      (throw (ex-info "Can only call functions and classes." {:token paren})))

    (when (not= (callable/arity callee*) args-count)
      (throw (ex-info
              (format "Expected %d arguments but got %d" (callable/arity callee*)
                      args-count)
              {:token paren})))
    (let [[x env_] (callable/call callee* func-name-identifier args-val env)]
      [x env_])))

(defmethod evaluate :function
  [{:keys [identifier] :as fn-ast} env]
  (let [fn-record (->Function fn-ast env)]
    [nil (environment/define env identifier fn-record)]))

(defmethod evaluate :return
  [{:keys [expr]} env]
  (let [[return-value env] (when expr
                         (evaluate expr env))]
    (throw
     (ex-info nil {:return-value return-value :env env  }))))

(defn interpret
  [statements]
  (try
    (loop [statements statements
           env (environment/create nil {})]
      (if (empty? statements)
        nil
        (let [[_evaluated-expr env] (evaluate (first statements) env)]
          (recur (rest statements) env))))
    (catch clojure.lang.ExceptionInfo e
      (utils/error (ex-message e) (.line (:token (ex-data e))))
      70 ;; Exit code for runtime error.
      )))
