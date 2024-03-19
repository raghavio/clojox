(ns clojox.environment)

(defn create
  ([parent]
   (create parent {}))
  ([parent bindings]
   {:bindings bindings :parent parent}))

(defn lookup
  [env identifier]
  (if env
    (if (contains? (:bindings env) (.lexeme identifier))
      (get-in env [:bindings (.lexeme identifier)])
      (recur (:parent env) identifier))
    (throw (ex-info (str "Undefined variable '" (.lexeme identifier) "'.") {:token identifier}))))

(defn define
  [env identifier val]
  (assoc-in env [:bindings (.lexeme identifier)] val))

(defn assign
  [env identifier val]
  (if (contains? (:bindings env) (.lexeme identifier))
    (assoc-in env [:bindings (.lexeme identifier)] val)
    (if (:parent env)
      (if-let [parent-env (assign (:parent env) identifier val)]
        (assoc env :parent parent-env)
        nil)
      (throw (ex-info (str "Undefined variable '" (.lexeme identifier) "'.") {:token identifier})))))
