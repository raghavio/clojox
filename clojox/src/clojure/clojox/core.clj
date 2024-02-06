(ns clojox.core
  (:gen-class)
  (:import [jlox Lox]))

(defn -main
  [& args]
  (cond
    (> (count args) 1) (do (println "Usage: jlox [script]")
                           (System/exit 64))
    (= (count args) 1) (Lox/runFile (first args))
    :else (Lox/runREPL)))
