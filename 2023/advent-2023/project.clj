(defproject advent-2023 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [criterium "0.4.6"]
                 [euclidean "0.2.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [astar-search "0.2"]]
  :main ^:skip-aot advent-2023.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
