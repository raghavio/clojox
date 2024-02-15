(ns clojox.parser
  (:import [jlox TokenType Lox]))

(declare expression)
(declare declaration)

(defn- error
  [token message]
  (Lox/runtimeError token message)
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
    (match? token TokenType/IDENTIFIER) [remaining {:type :variable :identifier token}]
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

(defn- var-stmt-ast
  ([identifier]
   (var-stmt-ast identifier nil))
  ([identifier initializer]
   {:type :var-stmt :identifier identifier :initializer initializer}))

(defn- var-stmt
  [[identifier second-token & remaining]]
  (if (match? identifier TokenType/IDENTIFIER)
    (cond
      (match? second-token TokenType/EQUAL) (let [[remaining initializer-expr] (expression remaining)]
                                              (if (match? (first remaining) TokenType/SEMICOLON)
                                                [(rest remaining) (var-stmt-ast identifier initializer-expr)]
                                                (error (first remaining) "Expect ';' after variable declaration.")))
      (not (match? second-token TokenType/SEMICOLON)) (error second-token "Expect ';' after variable declaration.")
      :else [remaining (var-stmt-ast identifier)])
    (error identifier "Expect variable name.")))

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

(defn- assignment
  [tokens]
  (let [[[second-token & rest-of-rest :as remaining] left-expr] (equality tokens)]
    (if (match? second-token TokenType/EQUAL)
      (let [[rest-of-rest* value-expr] (assignment rest-of-rest)]
        (if (= (:type left-expr) :variable)
          [rest-of-rest* {:type :assign :identifier (:identifier left-expr) :value value-expr}]
          (error (first tokens) "Invalid assignment target.")))
      [remaining left-expr])))

(defn- expression
  [tokens]
  (assignment tokens))


(defn- print-statement
  [tokens]
  (let [[[next-token & remaining] expr] (expression tokens)]
    (when-not (match? next-token TokenType/SEMICOLON)
      (error next-token "Expect ';' after value."))
    [remaining {:type :print :expression expr}]))

(defn- expression-statement
  [tokens]
  (let [[[next-token & remaining] expr] (expression tokens)]
    (when-not (match? next-token TokenType/SEMICOLON)
      (error next-token "Expect ';' after value."))
    [remaining expr]))

(defn- block-statements
  [tokens]
  (loop [[first-token & remaining :as tokens] tokens
         statements []]
    (cond
      (match? first-token TokenType/EOF) (do (error first-token "Except '}' after block")
                                             [tokens statements])
      (match? first-token TokenType/RIGHT_BRACE) [remaining statements]
      :else (let [[remaining expr] (declaration tokens)]
              (recur remaining (conj statements expr))))))

(defn- statement
  [[token & remaining :as tokens]]
  (case (.name (.type token))
    "PRINT" (print-statement remaining)
    "LEFT_BRACE" (let [[tokens statements] (block-statements remaining)]
                   [tokens {:type :block :statements statements}])
    (expression-statement tokens)))

(defn- declaration
  [[token & remaining :as tokens]]
  (if (match? token TokenType/VAR)
    (var-stmt remaining)
    (statement tokens)))

(defn parse
  "Returns a vector of ASTs containing statements and expressions."
  [tokens]
  (loop [tokens tokens
         statements []]
    (if (match? (first tokens) TokenType/EOF)
      statements
      (let [[remaining stmt] (try (declaration tokens)
                                  (catch RuntimeException _e
                                    nil))]
        (recur remaining (conj statements stmt))))))
