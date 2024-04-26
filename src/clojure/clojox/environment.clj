(ns clojox.environment
  (:import [jlox Token]))

(defn throw-undefined
  [identifier]
  (throw (ex-info (str "Undefined variable '" (.lexeme ^Token identifier) "'.")
                  {:token identifier})))

(defn lookup
  [env identifier]
  (if (contains? env (.lexeme ^Token identifier)) ;; value can be nil.
    @(get env (.lexeme ^Token identifier))
    (throw-undefined identifier)))

(defn assign
  [env identifier val]
  (if (contains? env (.lexeme ^Token identifier))
    (do (reset! (get env (.lexeme ^Token identifier)) val)
        env)
    (throw-undefined identifier)))

(defn define
  [env identifier val]
  (assoc env (.lexeme ^Token identifier) (atom val)))
