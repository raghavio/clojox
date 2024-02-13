(ns clojox.interpreter-test
  (:require [clojox.interpreter :refer [interpret]]
            [clojox.parser :as parser]
            [clojure.test :refer [deftest is testing]])
  (:import [jlox Scanner]))

(defn- ast
  [expr-str]
  (let [scanner (Scanner. expr-str)
        tokens (.scanTokens scanner)]
    (parser/parse tokens)))

(deftest test-interpret-arithmetic
  (testing "interpret 2 + 3"
    (is (= (interpret (ast "2 + 3")) "5")))
  (testing "interpret 2 - 3"
    (is (= (interpret (ast "2 - 3")) "-1")))
  (testing "interpret 2 * 3"
    (is (= (interpret (ast "2 * 3")) "6")))
  (testing "interpret 6 / 3"
    (is (= (interpret (ast "6 / 3")) "2")))
  (testing "interpret 3/5 - (5 * 6)"
    (is (= (interpret (ast "3/5 - (5 * 6)")) "-29.4"))))

(deftest test-interpret-string-concat
  (testing "interpret \"Hello\" + \" World\""
    (is (= (interpret (ast "\"Hello\" + \" World\"")) "Hello World"))))

(deftest test-interpret-logical
  (testing "interpret !true"
    (is (= (interpret (ast "!true")) "false")))
  (testing "interpret true == false"
    (is (= (interpret (ast "true == false")) "false"))))

(deftest test-interpret-comparison-operations
  (testing "interpret 1 > 2"
    (is (= (interpret (ast "1 > 2")) "false")))
  (testing "interpret 1 < 2"
    (is (= (interpret (ast "1 < 2")) "true")))
  (testing "interpret 2 >= 2"
    (is (= (interpret (ast "2 >= 2")) "true")))
  (testing "interpret 1 <= 2"
    (is (= (interpret (ast "1 <= 2")) "true"))))