(ns clojox.environment)

(defn create
  ([parent]
   (create parent {}))
  ([parent bindings]
   {:bindings bindings :parent parent}))

(defn lookup
  [env identifier]
  (if env
    (if (contains? (:bindings env) (.lexeme identifier)) ;; value can be nil.
      @(get (:bindings env) (.lexeme identifier))
      (recur (:parent env) identifier))
    (throw (ex-info (str "Undefined variable '" (.lexeme identifier) "'.") {:token identifier}))))

(defn define
  [env identifier val]
  (assoc-in env [:bindings (.lexeme identifier)] (atom val)))

(defn assign
  [env identifier val]
  (if (contains? (:bindings env) (.lexeme identifier))
    (do (reset! (get-in env [:bindings (.lexeme identifier)])  val) env)
    (if (:parent env)
      (if-let [parent-env (assign (:parent env) identifier val)]
        (assoc env :parent parent-env)
        nil)
      (throw (ex-info (str "Undefined variable '" (.lexeme identifier) "'.") {:token identifier})))))
