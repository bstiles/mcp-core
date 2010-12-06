(ns mcp-core.test.capture
  (:use [mcp-core.capture] :reload)
  (:use [clojure.test])
  (:require [clojure.java.io :as io])
  (:require [clojure.contrib.io :as cio]))

(defn- clean-dir
  [dir]
  (when (.exists dir)
    (when-not (.exists (io/file dir "SAFE_TO_DELETE"))
      (throw (IllegalStateException. (format "%s is not safe to delete." dir))))
    (cio/delete-file-recursively dir)))

(use-fixtures :each (fn [f]
                      (let [orig-value (System/getProperty "mcp-core.capture.dir")
                            dir (io/file (System/getProperty "java.io.tmpdir") "mcp-core.test.capture")]
                        (clean-dir dir)
                        (when-not (.mkdirs dir)
                          (throw (RuntimeException. (format "Couldn't make %s in fixture." dir))))
                        (spit (io/file dir "SAFE_TO_DELETE") "")
                        (System/setProperty "mcp-core.capture.dir" (str dir))
                        (f)
                        (if orig-value
                          (System/setProperty "mcp-core.capture.dir" orig-value)
                          (System/clearProperty "mcp-core.capture.dir"))
                        (clean-dir dir))))

(defn- find-empty-leaf-dirs
  [root]
  (filter (fn [el] (and (.isDirectory el) (empty? (.listFiles el))))
          (file-seq (io/file root))))

(deftest assumptions-1
  (is (empty? (find-empty-leaf-dirs (base-capture-dir)))))

(deftest assumptions-2
  (is (empty? (find-empty-leaf-dirs (base-capture-dir))))
  (get-capture-dir)
  (is (not (empty? (find-empty-leaf-dirs (base-capture-dir)))))
  (clean-dir (base-capture-dir)))

(deftest anonymous
  (is (not (.exists (io/file (base-capture-dir) "anonymous"))))
  (is (.exists (get-capture-dir)))
  (is (= 1 (count (find-empty-leaf-dirs (base-capture-dir)))))
  (is (= "anonymous" (.getName (first (find-empty-leaf-dirs (base-capture-dir))))))
  (is (.exists (io/file (base-capture-dir) "anonymous"))))

(deftest anonymous-twice
  (is (not (.exists (io/file (base-capture-dir) "anonymous"))))
  (is (.exists (get-capture-dir)))
  (is (= 1 (count (find-empty-leaf-dirs (base-capture-dir)))))
  (is (.exists (get-capture-dir)))
  (is (= 1 (count (find-empty-leaf-dirs (base-capture-dir)))))
  (is (.exists (get-capture-dir :fail-if-exists true)))
  (is (= 2 (count (find-empty-leaf-dirs (base-capture-dir))))))

(deftest named
  (is (not (.exists (io/file (base-capture-dir) "cat"))))
  (is (.exists (get-capture-dir :category "cat" :dir-name "named")))
  (is (= 1 (count (find-empty-leaf-dirs (base-capture-dir)))))
  (is (= "named" (.getName (first (find-empty-leaf-dirs (base-capture-dir))))))
  (is (.exists (io/file (base-capture-dir) "cat"))))

(deftest unique-failure
  (is (.exists (get-capture-dir :dir-name "d")))
  (is (thrown? IllegalStateException (get-capture-dir :dir-name ["d"] :fail-if-exists true))))

(deftest unique-failure-2
  (is (.exists (get-capture-dir :dir-name ["d" "f"] :fail-if-exists true)))
  (is (.exists (get-capture-dir :dir-name ["d" "f"] :fail-if-exists true)))
  (is (= 2 (count (find-empty-leaf-dirs (base-capture-dir)))))
  (is (thrown? IllegalStateException (get-capture-dir :dir-name ["d" "f"] :fail-if-exists true))))

(deftest files
  (is (= 2 (count (get-capture-files ["stdout" "stderr"]))))
  (is (= 2 (count (filter (fn [f] (not (or (.isDirectory f)
                                           (= "SAFE_TO_DELETE" (.getName f)))))
                          (file-seq (io/file (base-capture-dir))))))))

(deftest unique-files-failure
  (is (every? (memfn exists) (get-capture-files ["f"] :dir-name "d")))
  (is (thrown? RuntimeException (get-capture-files [["f"]] :dir-name ["d"] :fail-if-exists true))))

