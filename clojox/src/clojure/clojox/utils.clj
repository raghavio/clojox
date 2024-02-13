(ns clojox.utils
  (:import [jlox Lox]))

(defn runtime-error
  [message line]
  (set! (. Lox hadRuntimeError) true)
  (binding [*out* *err*]
    (println (str message " [line " line "]"))))