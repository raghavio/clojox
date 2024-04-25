(ns clojox.evaluate)

(defmulti evaluate (fn [ast _env] (:type ast)))
