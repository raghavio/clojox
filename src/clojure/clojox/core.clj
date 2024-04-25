(ns clojox.core
  (:gen-class)
  (:require [clojox.interpreter :refer [interpret]]
            [clojox.parser :refer [parse]])
  (:import [jlox Lox Scanner]
           [java.io FileNotFoundException]))

(defn run
  [source]
  (let [scanner (Scanner. source)
        tokens (.scanTokens scanner)
        ast (parse tokens)
        error-code (when (Lox/hadError) 65)
        error-code (if-not error-code
                     (interpret ast)
                     error-code)]
    error-code))

(defn run-file
  [file]
  (try (let [error-code (run (slurp file))]
         (when error-code
           (System/exit error-code)))
       (catch FileNotFoundException e
         (println (ex-message e))
         (System/exit 65))))

(defn -main
  [& args]
  (cond
    (> (count args) 1) (do (println "Usage: jlox [script]")
                           (System/exit 64))
    (= (count args) 1) (run-file (first args))
    :else (Lox/runREPL)))

(defn reset-error-flag!
  []
  (set! (. Lox hadError) false))
