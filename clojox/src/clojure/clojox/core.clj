(ns clojox.core
  (:gen-class)
  (:require [clojox.interpreter :refer [interpret]]
            [clojox.parser :refer [parse]])
  (:import [jlox Lox Scanner]))

(defn run
  [source]
  (let [scanner (Scanner. source)
        tokens (.scanTokens scanner)
        ast (parse tokens)]
    (interpret ast)))

(defn run-file
  [file]
  (run (slurp file)))

(defn -main
  [& args]
  (cond
    (> (count args) 1) (do (println "Usage: jlox [script]")
                           (System/exit 64))
    (= (count args) 1) (run-file (first args))
    :else (Lox/runREPL)))