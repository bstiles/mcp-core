(ns mcp-core.test.sh
  (:import (java.io ByteArrayOutputStream PrintStream))
  (:use [mcp-core.sh] :reload)
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

(deftest $>?-1
  (is (= {:exit 1 :err "" :out ""} ($>? false))))

(deftest $>?-2
  (is (= {:out "foo\n" :err "" :exit 0} ($>? echo foo))))

(deftest echo
  (is (= "\n" ($> echo))))

(deftest echo-1
  (is (= "1\n" ($> echo 1))))

(deftest echo-2
  (let [x 2]
    (is (= "2\n" ($> echo ~x)))))

(deftest echo-3
  (let [x 3]
    (is (= "3\n" ($> echo ~(* 3 1))))))

(deftest echo-4
  (let [x 3]
    (is (= "3" ($> echo -n ~(* 3 1))))))

(deftest echo-5
  (let [x ['foo 'bar]]
    (is (= "foo bar\n" ($> echo ~@x)))))

(deftest echo-6
  (let [x ['foo 'fred 'bar]]
    (is (= "foo fred\n" ($> echo ~@(take 2 x))))))

(deftest piped-to-system-out
  (let [orig-out System/out
        new-out (ByteArrayOutputStream.)]
    (try
      (System/setOut (PrintStream. new-out))
      (is (= 0 ($? {:out System/out} echo "foo")))
      (is (= "foo\n" (String. (.toByteArray new-out))))
      (finally
       (System/setOut orig-out)))))

(deftest piped-to-stream
  (let [out (ByteArrayOutputStream.)]
    (is (= 0 ($? {:out out} echo "foo")))
    (is (= "foo\n" (String. (.toByteArray out))))))

(deftest tee-to-two-streams
  (let [out [(ByteArrayOutputStream.) (ByteArrayOutputStream.)]]
    (is (= 0 ($? {:out out} echo "foo")))
    (is (= "foo\n" (String. (.toByteArray (out 0)))))
    (is (= "foo\n" (String. (.toByteArray (out 1)))))))

(deftest string-through-stdin
  (let [in "foo"
        out [(ByteArrayOutputStream.) (ByteArrayOutputStream.)]]
    (is (= 0 ($? {:out out :in in} cat)))
    (is (= in (String. (.toByteArray (out 0)))))
    (is (= in (String. (.toByteArray (out 1)))))))

(deftest large-input-stream
  (let [in (byte-array (take (* 100 1000) (cycle (map byte [1 2 3 4 5]))))
        out [(ByteArrayOutputStream.) (ByteArrayOutputStream.)]]
    (is (= 0 ($? {:out out :in in} cat)))
    (is (= (seq in) (seq (.toByteArray (out 0)))))
    (is (= (seq in) (seq (.toByteArray (out 1)))))))

(deftest various-tokens
  (is (= "-Dfoo=bar\n" ($> echo -Dfoo=bar))))

(deftest various-tokens-2
  (let [opts ["-Dfoo=bar" "-Xcheck:jni"]]
    (is (= "-Dfoo=bar -Xcheck:jni 6 org.foo\n" ($> echo ~@opts ~(* 2 3) org.foo)))))


(deftest dir-pushing
  (let [start (.getCanonicalPath (io/file (System/getProperty "user.dir")))
        one (.getCanonicalPath (io/file "/tmp"))
        two (.getCanonicalPath (io/file "/Users"))]
    (is (= start (working-dir)))
    ($popd)
    (is (= start (working-dir)))
    ($pushd one)
    (is (= one (working-dir)))
    ($popd)
    (is (= start (working-dir)))
    ($pushd one)
    ($pushd two)
    (is (= two (working-dir)))
    ($popd)
    (is (= one (working-dir)))
    ($popd)
    (is (= start (working-dir)))
    ($popd)
    (is (= start (working-dir)))))


(deftest env-pushing
  (is (= {} (env)))
  ($push-env {:a :b})
  (is (= {:a :b} (env)))
  ($pop-env)
  (is (= {} (env)))
  ($push-env {:a :b})
  ($push-env {:a :c})
  (is (= {:a :c} (env)))
  ($pop-env)
  (is (= {:a :b} (env)))
  ($pop-env)
  (is (= {} (env)))
  ($pop-env)
  (is (= {} (env))))
