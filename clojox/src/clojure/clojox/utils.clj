(ns clojox.utils
  (:import [jlox Lox]))

(defn error
  [message line]
  (binding [*out* *err*]
    (println (str message " [line " line "]"))))
