(ns clojox.core-test
  (:require [clojure.test :refer [deftest is]]) 
  (:import [jlox Scanner]
           [jlox Token]
           [jlox TokenType]))

(deftest test-scan-tokens
  (let [scanner (Scanner. "!=;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/BANG_EQUAL "!=" nil 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens2
  (let [scanner (Scanner. "=;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/EQUAL "=" nil 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens-ignore-comments
  (let [scanner (Scanner. "=; // yoo all of this should get ignored!\n>=;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/EQUAL "=" nil 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/GREATER_EQUAL ">=" nil 2)
            (Token. TokenType/SEMICOLON ";" nil 2)
            (Token. TokenType/EOF "" nil 2)]))))

(deftest test-scan-tokens-strings
  (let [scanner (Scanner. "\"Yooo!\";")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/STRING "\"Yooo!\"" "Yooo!" 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens-numbers
  (let [scanner (Scanner. "2342;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/NUMBER "2342" 2342.0 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens-fractional-numbers
  (let [scanner (Scanner. "2342.43;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/NUMBER "2342.43" 2342.43 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens-invalid-fractional-numbers
  (let [scanner (Scanner. "2342.;")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/NUMBER "2342" 2342.0 1)
            (Token. TokenType/DOT "." nil 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))

(deftest test-scan-tokens-identifiers
  (let [scanner (Scanner. "var test = \"Hello world\";")]
    (is (= (.scanTokens scanner)
           [(Token. TokenType/VAR "var" nil 1)
            (Token. TokenType/IDENTIFIER "test" nil 1)
            (Token. TokenType/EQUAL "=" nil 1)
            (Token. TokenType/STRING "\"Hello world\"" "Hello world" 1)
            (Token. TokenType/SEMICOLON ";" nil 1)
            (Token. TokenType/EOF "" nil 1)]))))
