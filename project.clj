(defproject grouper "0.1.0-SNAPSHOT"
  :description "grouper"
  :url "https://github.com/YukiThornton/grouper"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [integrant "0.8.0"]]
  :main ^:skip-aot grouper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
