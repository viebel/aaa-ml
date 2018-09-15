(defproject audyx-big-data "0.0.1"
  :main ^:skip-aot gorilla-test.core
  :target-path "target/%s"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [honeysql "0.9.3"]
                 [viebel/gadjett "0.5.2"]
                 [clj-time "0.14.4"]
                 [org.postgresql/postgresql "42.2.5"]
                 [org.clojure/java.jdbc "0.7.8"]]
  :plugins [[lein-gorilla "0.4.0"]])