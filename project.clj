(defproject clojox "0.1.0-SNAPSHOT"
  :description "Clojure implementation of the Lox programming language from the book Crafting Interpreters."
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.clojure-goes-fast/clj-async-profiler "1.2.0"]]
  :main ^:skip-aot clojox.core
  :target-path "target/%s"
  :source-paths ["src/clojure"]
  :java-source-paths ["src/jlox"]
  :javac-options ["--enable-preview" "--source" "21"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ["--enable-preview" "-Djdk.attach.allowAttachSelf"])
