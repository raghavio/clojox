(ns clojox.core
  (:gen-class)
  (:require [clojox.parser :refer [parser]]) 
  (:import [jlox Lox Scanner]))

(defn -main
  [& args]
  (cond
    (> (count args) 1) (do (println "Usage: jlox [script]")
                           (System/exit 64))
    (= (count args) 1) (Lox/runFile (first args))
    :else (Lox/runREPL)))

(defn run
  [source]
  (let [scanner (Scanner. source)
        tokens (.scanTokens scanner)]
    (parser tokens)))