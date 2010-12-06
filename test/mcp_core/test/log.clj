(ns mcp-core.test.log
  (:use [mcp-core.log] :reload)
  (:use [clojure.test]))


(time (dotimes [_ (* 1000 1000)] (*levels* :info)))
(time (dotimes [_ (* 1000)] (log :info "foo")))

(deftest log-ns
  (let [orig-ns *ns*]
    (try
      (in-ns 'test1)
      (is (log :info "tasting"))
      (finally (in-ns (ns-name orig-ns))))))

