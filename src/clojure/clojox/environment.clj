(ns clojox.environment
  (:import [jlox Token]))

(defn create
  ([parent]
   (create parent {}))
  ([parent bindings]
   {:bindings bindings :parent parent}))

(defn lookup
  [env identifier]
  (if env
    (if (contains? (:bindings env) (.lexeme ^Token identifier)) ;; value can be nil.
      @(get (:bindings env) (.lexeme ^Token identifier))
      (recur (:parent env) identifier))
    (throw (ex-info (str "Undefined variable '" (.lexeme ^Token identifier) "'.")
                    {:token identifier}))))

(defn assign
  [env identifier val]
  (if (contains? (:bindings env) (.lexeme ^Token identifier))
    (do (reset! (get-in env [:bindings (.lexeme ^Token identifier)])  val) env)
    (if (:parent env)
      (if-let [parent-env (assign (:parent env) identifier val)]
        (assoc env :parent parent-env)
        nil)
      (throw (ex-info (str "Undefined variable '" (.lexeme ^Token identifier) "'.")
                      {:token identifier})))))

(defn define
  [env identifier val]
  (if (contains? (:bindings env) (.lexeme ^Token identifier))
    (assign env identifier val)
    (assoc-in env [:bindings (.lexeme ^Token identifier)] (atom val))))
