(ns clojox.parser
  (:import [jlox TokenType Lox]))

(declare expression)

(defn- error
  [token message]
  (Lox/error token message)
  (RuntimeException.))

(defn- match?
  "Checks if the token matches any of the types"
  [token & types]
  (some #(= (.type token) %) types))

;; All the below functions return a vector of tokens and the ast.

(defn- grouping-ast
  [tokens]
  (let [[[token & remaining] group-expr] (expression tokens)]
    (when-not (match? token TokenType/RIGHT_PAREN)
      (error token "Except ')' after expression."))
    [remaining {:type :grouping :value group-expr}]))

(defn- literal-ast
  [[token & remaining-tokens]]
  (let [val-lookup {TokenType/FALSE false TokenType/TRUE true TokenType/NIL nil}
        value (get val-lookup (.type token) (.literal token))]
    [remaining-tokens {:type :literal :value value}]))

(defn- primary
  [[token & remaining :as tokens]]
  (cond
    (match? token TokenType/FALSE TokenType/TRUE TokenType/NIL TokenType/NUMBER TokenType/STRING) (literal-ast tokens)
    (match? token TokenType/LEFT_PAREN) (grouping-ast remaining)
    :else (error token "Except expression.")))

(defn- unary-ast
  [[token & remaining :as tokens]]
  (if (match? token TokenType/BANG TokenType/MINUS)
    (let [[remaining right] (unary-ast remaining)]
      [remaining {:type :unary :op token :right right}])
    (primary tokens)))

(defn- binary-ast
  [tokens parser-fn & operand-types]
  (loop [[[next-token & remaining :as tokens] left-expr] (parser-fn tokens)] ;; Process the first token as left side node.
    (if (apply match? next-token operand-types) ;; Recursively check if the next token(s) match the operand types 
      ;; If it does, process the remaining tokens for right side node.
      (let [[remaining right-expr] (parser-fn remaining)]
        (recur [remaining {:type :binary :left left-expr :op next-token :right right-expr}]))
      ;; If the next token's operand doesn't match, return remaining tokens & the left expr.
      [tokens left-expr])))

(defn- unary
  [tokens]
  (unary-ast tokens))

(defn- factor
  [tokens]
  (binary-ast tokens unary TokenType/SLASH TokenType/STAR))

(defn- term
  [tokens]
  (binary-ast tokens factor TokenType/MINUS TokenType/PLUS))

(defn- comparison
  [tokens]
  (binary-ast tokens term TokenType/GREATER TokenType/GREATER_EQUAL TokenType/LESS TokenType/LESS_EQUAL))

(defn- equality
  [tokens]
  (binary-ast tokens comparison TokenType/BANG_EQUAL TokenType/EQUAL_EQUAL))

(defn- expression
  [tokens]
  (equality tokens))

(defn parse
  "Returns the ast for the specified tokens."
  [tokens]
  (try
    (second (expression tokens))
    (catch RuntimeException _e
      nil)))
