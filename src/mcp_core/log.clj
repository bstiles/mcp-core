(ns mcp-core.log
  (:import (java.util.logging Level Logger LogManager)
           (java.util.prefs Preferences))
  (:require [clojure.java.io :as io]))

(def *logging-dir* (io/file (System/getProperty "user.home") "MCP" "Logs"))

(def *configuration* (format "
handlers= java.util.logging.ConsoleHandler, java.util.logging.FileHandler
.level = ALL
java.util.logging.FileHandler.pattern = %s
java.util.logging.FileHandler.append = true
java.util.logging.FileHandler.limit = 500000
java.util.logging.FileHandler.count = 1000
java.util.logging.FileHandler.formatter = java.util.logging.SimpleFormatter
java.util.logging.FileHandler.level = ALL
java.util.logging.ConsoleHandler.level = WARNING
java.util.logging.ConsoleHandler.formatter = java.util.logging.SimpleFormatter
" (str *logging-dir* "/mcp-%u.%g.log")))

(defn configure
  []
  (when-not (or (System/getProperty "java.util.logging.config.class")
                (System/getProperty "java.util.logging.config.file"))
    (when-not (.exists *logging-dir*)
      (when-not (.mkdirs *logging-dir*)
        (throw (RuntimeException. (str "Could not create log directory: " *logging-dir*)))))
    (.readConfiguration (LogManager/getLogManager)
                        (io/input-stream (.getBytes *configuration*)))))
(configure)

(def *levels* {:debug Level/FINE
               :info Level/INFO
               :warning Level/WARNING
               :severe Level/SEVERE})

(defonce *logging-agent* (agent 0
                                :error-handler (fn [_ e]
                                                 (.printStackTrace e)
                                                 (.throwing (Logger/getLogger "") e))))

(defmacro log
  [level & args]
  (when-not (*levels* level)
    (throw (IllegalArgumentException. (str "Unknown level: " level))))
  `(send-off *logging-agent*
             (let [calling-ns-name# (name (ns-name *ns*))]
               (fn [message-count#]
                 (io! (.log (Logger/getLogger calling-ns-name#) (*levels* ~level) (str ~@args)))
                 (inc message-count#)))))
