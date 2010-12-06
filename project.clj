(defproject mcp-core "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :repl-init-script (str (System/getProperty "user.home") "/.clojure.d/swank-init/user.clj"))
