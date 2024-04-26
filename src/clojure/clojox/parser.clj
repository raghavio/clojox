(ns clojox.parser
  (:require [clojox.interpreter :refer [->Grouping ->Literal ->Unary ->Binary ->Print
                                        ->VarStmt ->Variable ->Assign ->Block ->If
                                        ->Logical ->While ->Call ->Fun ->Return]])
  (:import [jlox Token TokenType Lox]
           [clojox.interpreter Variable]))

(declare statement declaration expression)

(defn- error
  [[errored-token & tokens] message]
  (Lox/runtimeError errored-token message)
  (ex-info "parse-error" {:tokens tokens}))

(defn- throw-error
  [tokens message]
  (throw (error tokens message)))

(defn- match?
  "Checks if the token matches any of the types"
  [token & types]
  (some #(= (.type ^Token token) %) types))

(defn- consume
  "Consumes the first token if it matches the type, else raises an exception with the error message."
  [tokens type error-message]
  (if (match? (first tokens) type)
    (rest tokens)
    (throw-error tokens error-message)))

(defn- synchronize
  [[token & remaining :as tokens]]
  (cond (match? token TokenType/EOF) tokens
        (match? token TokenType/SEMICOLON) remaining
        (match? token TokenType/CLASS TokenType/FUN TokenType/VAR TokenType/FOR
                TokenType/IF TokenType/WHILE TokenType/PRINT TokenType/RETURN) tokens
        :else (recur remaining)))

;; All the below functions return a vector of tokens and the ast.

(defn- grouping-ast
  [tokens]
  (let [[[token & remaining :as tokens] group-expr] (expression tokens)]
    (when-not (match? token TokenType/RIGHT_PAREN)
      (throw-error tokens "Expect ')' after expression."))
    [remaining (->Grouping group-expr)]))

(defn- literal-ast
  [[token & remaining-tokens]]
  (let [val-lookup {TokenType/FALSE false TokenType/TRUE true TokenType/NIL nil}
        value (get val-lookup (.type ^Token token) (.literal ^Token token))]
    [remaining-tokens (->Literal value)]))

(defn- primary
  [[token & remaining :as tokens]]
  (cond
    (match? token TokenType/FALSE TokenType/TRUE TokenType/NIL TokenType/NUMBER TokenType/STRING) (literal-ast tokens)
    (match? token TokenType/LEFT_PAREN) (grouping-ast remaining)
    (match? token TokenType/IDENTIFIER) [remaining (->Variable token)]
    :else (throw-error tokens "Expect expression.")))

(defn- left-right-ast
  [ast-type tokens parser-fn & operand-types] ;; This gets used for logical operators as well
  (loop [[[next-token & remaining :as tokens] left-expr] (parser-fn tokens)] ;; Process the first token as left side node.
    (if (apply match? next-token operand-types) ;; Recursively check if the next token(s) match the operand types
        ;; If it does, process the remaining tokens for right side node.
      (let [[remaining right-expr] (parser-fn remaining)]
        (recur [remaining (ast-type left-expr next-token right-expr)]))
        ;; If the next token's operand doesn't match, return remaining tokens & the left expr.
      [tokens left-expr])))

(def binary-ast (partial left-right-ast ->Binary))

(def logical-ast (partial left-right-ast ->Logical))

(defn- var-stmt-ast
  ([identifier]
   (var-stmt-ast identifier nil))
  ([identifier initializer]
   (->VarStmt identifier initializer)))

(defn- var-stmt
  [[identifier second-token & remaining :as tokens]]
  (if (match? identifier TokenType/IDENTIFIER)
    (cond
      (match? second-token TokenType/EQUAL) (let [[remaining initializer-expr] (expression remaining)]
                                              (if (match? (first remaining) TokenType/SEMICOLON)
                                                [(rest remaining) (var-stmt-ast identifier initializer-expr)]
                                                (throw-error remaining "Expect ';' after variable declaration.")))
      (not (match? second-token TokenType/SEMICOLON)) (throw-error [second-token remaining] "Expect ';' after variable declaration.")
      :else [remaining (var-stmt-ast identifier)])
    (throw-error tokens "Expect variable name.")))

(defn- fn-arguments
  "Returns the remaining tokens and the arguments vector"
  [tokens]
  (if (match? (first tokens) TokenType/RIGHT_PAREN)
    [tokens []]
    (loop [[tokens argument] (expression tokens)
           arguments [argument]]
      (if (match? (first tokens) TokenType/COMMA)
        (do
          (when (>= (count arguments) 255)
            (error (rest tokens) "Can't have more than 255 arguments."))
          (let [[tokens argument] (expression (rest tokens))]
            (recur [tokens argument] (conj arguments argument))))
        [tokens arguments]))))

(defn- call
  [tokens]
  (loop [[[token & remaining :as tokens] expr] (primary tokens)]
    (if (match? token TokenType/LEFT_PAREN)
      (let [[remaining arguments] (fn-arguments remaining)
            expr (->Call expr (first remaining) arguments)
            remaining (consume remaining TokenType/RIGHT_PAREN "Expect ')' after arguments.")]
        (recur [remaining expr]))
      [tokens expr])))

(defn- unary
  [[token & remaining :as tokens]]
  (if (match? token TokenType/BANG TokenType/MINUS)
    (let [[remaining right] (unary remaining)]
      [remaining (->Unary token right)])
    (call tokens)))

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

(defn- logical-and
  [tokens]
  (logical-ast tokens equality TokenType/AND))

(defn- logical-or
  [tokens]
  (logical-ast tokens logical-and TokenType/OR))

(defn- assignment
  [tokens]
  (let [[[second-token & rest-of-rest :as remaining] left-expr] (logical-or tokens)]
    (if (match? second-token TokenType/EQUAL)
      (let [[rest-of-rest* value-expr] (assignment rest-of-rest)]
        (if (instance? Variable left-expr)
          [rest-of-rest* (->Assign (:identifier left-expr) value-expr)]
          (throw-error remaining "Invalid assignment target.")))
      [remaining left-expr])))

(defn- expression
  [tokens]
  (assignment tokens))

(defn- print-statement
  [tokens]
  (let [[[next-token & remaining :as tokens] expr] (expression tokens)]
    (when-not (match? next-token TokenType/SEMICOLON)
      (throw-error tokens "Expect ';' after value."))
    [remaining (->Print expr)]))

(defn- expression-statement
  ([tokens]
   (expression-statement tokens "Expect ';' after expression."))
  ([tokens error-message]
   (let [[[next-token & remaining :as tokens] expr] (expression tokens)]
     (when-not (match? next-token TokenType/SEMICOLON)
       (throw-error tokens error-message))
     [remaining expr])))

(defn- block-statements
  [tokens]
  (loop [[first-token & remaining :as tokens] tokens
         statements []]
    (cond
      (match? first-token TokenType/EOF) (do (throw-error first-token "Expect '}' after block")
                                             [tokens statements])
      (match? first-token TokenType/RIGHT_BRACE) [remaining statements]
      :else (let [[remaining expr] (declaration tokens)]
              (recur remaining (conj statements expr))))))

(defn- if-statement
  [[left-paren & remaining :as tokens]]
  (if (match? left-paren TokenType/LEFT_PAREN)
    (let [[[right-paren & remaining :as tokens] condition] (expression remaining)]
      (if (match? right-paren TokenType/RIGHT_PAREN)
        (let [[remaining then-branch] (statement remaining)
              [remaining else-branch] (if (match? (first remaining) TokenType/ELSE)
                                        (statement (rest remaining))
                                        [remaining nil])]
          [remaining (->If condition then-branch else-branch)])
        (throw-error tokens "Expect ')' after if condition.")))
    (throw-error tokens "Expect '(' after 'if'.")))

(defn- while-statement
  [[left-paren & remaining :as tokens]]
  (if (match? left-paren TokenType/LEFT_PAREN)
    (let [[[right-paren & remaining :as tokens] condition] (expression remaining)]
      (if (match? right-paren TokenType/RIGHT_PAREN)
        (let [[remaining body] (statement remaining)]
          [remaining (->While condition body)])
        (throw-error tokens "Expect ')' after condition.")))
    (throw-error tokens "Expect '(' after 'while'.")))

(defn- for-statement
  [[left-paren & remaining :as tokens]]
  (if (match? left-paren TokenType/LEFT_PAREN)
    (let [[remaining initializer] (cond
                                    (match? (first remaining) TokenType/SEMICOLON) [(rest remaining) nil]
                                    (match? (first remaining) TokenType/VAR) (var-stmt (rest remaining))
                                    :else (expression-statement remaining))
          [remaining condition] (if (match? (first remaining) TokenType/SEMICOLON)
                                  [(rest remaining) (->Literal true)] ;; Default condition true for infinite loop.
                                  (expression-statement remaining "Expect ')' after loop condition."))
          [remaining increment] (if (match? (first remaining) TokenType/RIGHT_PAREN)
                                  [remaining (->Literal nil)]
                                  (expression remaining))
          remaining (consume remaining TokenType/RIGHT_PAREN "Expect ')' after for clauses.")
          [remaining body] (statement remaining)
          body (if increment
                 (->Block [body increment])
                 body)
          body (->While condition body)
          body (if initializer
                 (->Block [initializer body])
                 body)]
      [remaining body])
    (throw-error tokens "Expect '(' after 'for'.)")))

(defn function
  [kind [fn-identifier & _ :as tokens]]
  (let [tokens (consume tokens TokenType/IDENTIFIER (str "Expect " kind " name."))
        tokens (consume tokens TokenType/LEFT_PAREN (str "Expect '(' after  " kind " name."))
        get-params-fn (fn [tokens params]
                        (do
                          (when (>= (count params) 255)
                            (error tokens "Can't have more than 255 parameters."))
                          [(consume tokens TokenType/IDENTIFIER "Expect parameter name.")
                           (conj params (first tokens))]))
        [tokens params] (if (match? (first tokens) TokenType/RIGHT_PAREN)
                          [tokens []]
                          (loop [[tokens params] (get-params-fn tokens [])]
                            (if (match? (first tokens) TokenType/COMMA)
                              (recur (get-params-fn (rest tokens) params))
                              [tokens params])))
        tokens (consume tokens TokenType/RIGHT_PAREN "Expect ')' after parameters.")
        tokens (consume tokens TokenType/LEFT_BRACE (str "Expect '{' before " kind " body."))
        [tokens body-expr] (block-statements tokens)]
    [tokens (->Fun fn-identifier params (->Block body-expr))]))

(defn func-declare
  "This is a custom feature to support mutual recursion.
  Just like in Clojure, I implemented `declare` for forward declaration."
  [[identifier & remaining :as tokens]]
  (if (match? identifier TokenType/IDENTIFIER)
    (if (match? (first remaining) TokenType/SEMICOLON)
      [(rest remaining) (->Fun identifier nil nil)]
      (throw-error remaining "Expect ';' after variable declaration."))
    (throw-error tokens "Expect variable name.")))

(defn return-statement
  [[return-token & tokens]]
  (let [[tokens expr] (if (match? (first tokens) TokenType/SEMICOLON)
                        [tokens nil]
                        (expression tokens))
        tokens (consume tokens TokenType/SEMICOLON
                        "Expect ';' after return value.")]
    [tokens (->Return return-token expr)]))

(defn- statement
  [[token & remaining :as tokens]]
  (case (.name (.type ^Token token))
    "PRINT" (print-statement remaining)
    "LEFT_BRACE" (let [[tokens statements] (block-statements remaining)]
                   [tokens (->Block statements)])
    "IF" (if-statement remaining)
    "WHILE" (while-statement remaining)
    "FOR" (for-statement remaining)
    "RETURN" (return-statement tokens) ;; this function expects the return token.
    (expression-statement tokens)))

(defn- declaration
  [[token & remaining :as tokens]]
  (cond (match? token TokenType/VAR) (var-stmt remaining)
        (match? token TokenType/FUN) (function "function" remaining)
        (match? token TokenType/DECLARE) (func-declare remaining)
        :else (statement tokens)))

(defn parse
  "Returns a vector of ASTs containing statements and expressions."
  [tokens]
  (loop [tokens tokens
         statements []]
    (if (match? (first tokens) TokenType/EOF)
      statements
      (let [[remaining stmt] (try (declaration tokens)
                                  (catch clojure.lang.ExceptionInfo e
                                    [(synchronize (:tokens (ex-data e))) nil]))]
        (recur remaining (conj statements stmt))))))
