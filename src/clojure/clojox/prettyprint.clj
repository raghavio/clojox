(ns clojox.prettyprint)

(defmulti pretty-print-ast :type)

(defmethod pretty-print-ast :binary
  [{:keys [left op right]}]
  [(.lexeme op) (pretty-print-ast left) (pretty-print-ast right)])

(defmethod pretty-print-ast :grouping
  [{:keys [value]}]
  ["grouping" (pretty-print-ast value)])

(defmethod pretty-print-ast :unary
  [{:keys [op right]}]
  [(.lexeme op) (pretty-print-ast right)])

(defmethod pretty-print-ast :literal
  [{:keys [value]}]
  value)