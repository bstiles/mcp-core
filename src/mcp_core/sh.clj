(ns mcp-core.sh
  "Provides the ability to execute and manage external processes."
  (:import (java.io BufferedInputStream BufferedOutputStream ByteArrayInputStream 
                    ByteArrayOutputStream LineNumberReader)
           (java.nio ByteBuffer)
           (java.nio.channels Channels)
           (java.util Date))
  (:use clojure.core
        [mcp-core.log :only [log]])
  (:require [clojure.contrib.io :as io]
            [clojure.string :as string]
            [mcp-core.capture :as capture]))

(defrecord ProcessInfo [pid start-time end-time process exit args work-dir]
  Object
  (toString [this]
            (str (apply array-map
                        `(:pid ~(:pid this)
                               :start-time ~(:start-time this)
                               ~@(if (future-done? (:exit this))
                                   [:end-time @(:end-time this)
                                    :exit @(:exit this)]
                                   [:end-time (format "Executing as of %s" (Date.))
                                    :exit "--"])
                               :work-dir ~(:work-dir this)
                               :args ~(:args this))))))

(defmethod clojure.core/print-method ProcessInfo
  [process-info writer]
  (.write writer (.toString process-info)))

(defn make-process-info
  "Convenience constructor for ProcessInfo."
  [& {:keys [pid start-time end-time process exit args work-dir]}]
  (ProcessInfo. pid start-time end-time process exit args work-dir))

(defonce
  ^{:doc "PID => ProcessInfo

  Table of recently spawned and still running process."}
  *processes* (atom {}))

(defn clear-dead-processes
  "Removes from the table of processes any that have terminated."
  []
  (swap! *processes* (fn [orig]
                       (let [dead (into {} (filter (fn [[_ {exit :exit}]]
                                                     (future-done? exit))
                                                   orig))]
                         (doseq [x (vals dead)]
                           (log :debug (format "Clearing %s" (pr-str x))))
                         (into {} (filter (comp not dead key) orig))))))

(defn $ps
  "Lists all running processes."
  []
  (doseq [p (filter (fn [[_ {exit :exit}]] (not (future-done? exit))) @*processes*)]
    (println p)))

(defn $kill
  "Terminates the specified process(es)."
  [pid & pids]
  (let [pids (into #{pid} pids)]
    (doseq [[_ proc-info] (filter (fn [[_ {pid :pid exit :exit}]]
                                    (and (pids pid)
                                         (not (future-done? exit))))
                                  @*processes*)]
      (.destroy (:process proc-info)))))

(def
  ^{:doc "Buffer size used for piping bytes from processes' std[in|out|err]."}
  *buffer-size* 8096)

(def
  ^{:doc "Path of the directory to use as the working directory,
  overriding (System/getProperty \"user.dir\")"}
  *working-dir* nil)
(set-validator! #'*working-dir*
                (fn [dir]
                  (if-not (or (nil? dir)
                                (.exists (java.io.File. dir)))
                    (throw (java.io.FileNotFoundException. dir))
                    true)))

(def *pushd-stack* '())

(defn working-dir
  []
  (or *working-dir* (System/getProperty "user.dir")))

(defn cd
  "Changes the working directory to the path specified (must exist),
  or $HOME if none is specified."
  ([] (cd (System/getProperty "user.home")))
  ([dir]
     (alter-var-root #'*working-dir*
                     (fn [_] (-> (condp = (class dir)
                                     clojure.lang.Named (java.io.File. (name dir))
                                     java.io.File dir
                                     java.lang.String (java.io.File. dir))
                                 .getCanonicalPath)))))

(defn $pushd
  "Changes the working directory to the path specified (must exist)
  and stores prior working directory on a stack that can be restored
  with $popd."
  [dir]
  (alter-var-root #'*pushd-stack* conj *working-dir*)
  (cd dir))

(defn $popd
  "Changes the working directory to the one most recently pushed
  onto the pushd stack."
  []
  (alter-var-root #'*pushd-stack* (fn [old] (cd (peek old)) (drop 1 old))))

(defn splice-args
  "INTERNAL"
  [args]
  (mapcat (fn [arg] (if (sequential? arg)
                      (map str arg)
                      (vector (str arg))))
          args))

(defn- convert-to-strings
  [args]
  (map (fn [arg] (cond
                  (keyword? arg) (name arg)
                  (and (= clojure.lang.Cons (class arg))
                       (= "unquote" (name (first arg)))) `(str ~(second arg))
                  (and (= clojure.lang.Cons (class arg))
                       (= "unquote-splicing" (name (first arg)))) `~(second arg)
                  :else (str arg)))
          args))

(defn- command-name
  [args]
  (.getName (io/file (first args))))

(defn sh-exec
  "Executes a process via Runtime.exec and captures the input/output.
  The first argument must be a map of options.  All subsequent arguments
  must be strings to be used as the command to be executed.

  Options:
    :out      One or a sequence of output streams to which the stdout
              of the spawned process will be copied.
    :err      One or a sequence of output streams to which the stderr
              of the spawned process will be copied.
    :in       An input stream from which the stdin of the spawned
              process will be copied.

  NOTE: This is Bash-specific because of the hack to obtain the child
  process' PID."
  [options & args]
  (log :debug (print-str "Calling:" (pr-str args)))
  (let [options (merge {:out nil
                        :err nil
                        :in nil}
                       options)
        start-time (Date.)
        end-time (atom nil)
        work-dir (io/file (working-dir))
        process (.. (ProcessBuilder. (into-array String (concat ["bash" "-s"] args)))
                    (directory work-dir)
                    start)
        out (:out options)
        err (:err options)]
    
    (doto (io/writer (.getOutputStream process))
      (.write "echo $$ 1>&2\n")
      (.write "exec \"$@\"\n")
      .flush)

    (let [pid (try
                (Integer/parseInt (.readLine (LineNumberReader. (io/reader (.getErrorStream process)))))
                (catch NumberFormatException e nil))
          child-stdout (Channels/newChannel (BufferedInputStream. (.getInputStream process)))
          child-stderr (Channels/newChannel (BufferedInputStream. (.getErrorStream process)))
          [stdout stderr stdin command] (map io/output-stream
                                             (capture/get-capture-files ["stdout" "stderr" "stdin" "command"]
                                                                        :category "proc"
                                                                        :dir-name (str (command-name args) "-" pid)))
          outs (concat [stdout] (when out (if (sequential? out) out [out])))
          errs (concat [stderr] (when err (if (sequential? err) err [err])))
          transfer-threads (doall (for [[source dests streams] [[child-stdout
                                                                 (map #(Channels/newChannel %) outs)
                                                                 outs]
                                                                [child-stderr
                                                                 (map #(Channels/newChannel %) errs)
                                                                 errs]]]
                                    ;; Copy output from child process to destinations
                                    (future
                                      (io!
                                       (let [buffer (ByteBuffer/allocate *buffer-size*)]
                                         (.clear buffer)
                                         (loop [bytes-read (.. source (read buffer))]
                                           (when (or (not (neg? bytes-read)) (not= 0 (.position buffer)))
                                             (doseq [dest dests]
                                               (.write dest (.flip buffer)))
                                             (.compact buffer)
                                             (recur (.. source (read buffer)))))
                                         (doseq [buffered streams]
                                           (.flush buffered)
                                           (when-not (#{System/out System/err} buffered)
                                             (.close buffered))))))))]

      ;; Record the command and working directory
      (spit command (str (prn-str args) work-dir \newline))

      ;; Copy input to child process
      (when-let [in (:in options)]
        (let [child-stdin-stream (BufferedOutputStream. (.getOutputStream process))
              child-stdin (Channels/newChannel child-stdin-stream)
              stdin-channel (Channels/newChannel stdin)
              in-source (Channels/newChannel (if (string? in)
                                               (ByteArrayInputStream. (.getBytes in))
                                               (io/input-stream in)))]
          (future
            (io!
             (let [buffer (ByteBuffer/allocate *buffer-size*)]
               (.clear buffer)
               (loop [bytes-read (.. in-source (read buffer))]
                 (when (or (not (neg? bytes-read)) (not= 0 (.position buffer)))
                   (let [flipped (.flip buffer)]
                     (.write child-stdin flipped)
                     (.write stdin-channel flipped))
                   (.compact buffer)
                   (recur (.. in-source (read buffer)))))
               (doto child-stdin-stream
                 .flush
                 .close)
               (doto stdin
                 .flush
                 .close))))))

      (let [result (make-process-info :pid pid
                                      :start-time start-time
                                      :end-time end-time
                                      :process process
                                      :exit (future (doseq [t transfer-threads] @t)
                                                    (let [exit-code (.waitFor process)]
                                                      (swap! end-time (fn [_] (Date.)))
                                                      (log :info (print-str "PID:" pid "exited with" exit-code))
                                                      exit-code))
                                      :args args
                                      :work-dir work-dir)]
        (log :info (print-str "PID:" pid "started via" (pr-str args)))
        (swap! *processes* (fn [processes] (assoc processes pid result)))
        result))))

(defmacro $$
  "Executes an external command, returning a ProcessInfo describing
  the command.  Arguments are turned into an array of strings to be
  used as the command line.  Keywords are converted to their names,
  ~ and ~@ forms are expanded, and everything else but maps are
  converted to strings using STR.  A map can be used to pass options
  to sh-exec and will not become part of the command line.
  See also sh-exec.

  Examples:
    ($$ echo foo)

      => \"echo\" \"foo\"\"

    (let [opts [\"-Dfoo=bar\" \"-Xcheck:jni\"]]
      ($$ java ~@opts ~(* 2 3) org.foo))

      => \"java\" \"-Dfoo=bar\" \"-Xcheck:jni\" \"6\" \"org.foo\"

    ($$ {:out System/out} echo foo)

      => \"echo\" \"foo\"

      and redirects echo's output to System/out.
   "
  [& args]
  `(let [options# (apply merge {} [~@(filter map? args)])]
     (apply sh-exec options# (splice-args [~@(convert-to-strings (filter (comp not map?) args))]))))

(defmacro $?
  "Executes an external command, returning its exit code.
  See also $$."
  [& args]
  `(deref (:exit ($$ ~@args))))

(defmacro $>?
  "Executes an external command, returning a map of its standard
  output (:out) and its exit code (:exit).  See also $$."
  [& args]
  `(let [out# (ByteArrayOutputStream.)
         err# (ByteArrayOutputStream.)
         exit# ($? {:out out# :err err#} ~@args)]
     {:exit exit#
      :out (String. (.toByteArray out#))
      :err (String. (.toByteArray err#))}))

(defmacro $>
  "Executes an external command, returning its standard output
  as a string.  See also $$."
  [& args]
  `(let [out# (ByteArrayOutputStream.)]
     ($? {:out out#} ~@args)
     (String. (.toByteArray out#))))

(defmacro $
  "Executes an external command, printing its standard output
  and returning its exit code.  See also $$."
  [& args]
  `(let [result# ($>? ~@args)]
     (io!
      (print (:out result#))
      (print (:err result#)))
     (:exit result#)))

#_(defmacro $->
  ([form] `($ ~~(if (seq? form) (list ~@form) form)))
)

(defmacro abs-path
  "Create a file path"
  [& args]
  `(.getAbsolutePath (java.io.File. (apply str java.io.File/separator
                          (interpose java.io.File/separator
                                     [~@(convert-to-strings (filter (comp not map?) args))])))))

(defmacro rel-path
  "Create a relative file path."
  [& args]
  `(java.io.File. (working-dir)
                  (apply str (interpose java.io.File/separator
                                        [~@(convert-to-strings (filter (comp not map?) args))]))))

(defmacro $cd
  [& args]
  `(let [all# [~@(convert-to-strings args)]
         head# (first all#)]
    (cond
     (#{".." "."} head#) (cd (rel-path ~@args))
     (= "/" head#) (cd (abs-path ~@(next args)))
     :else (apply cd [~@(convert-to-strings args)]))))

(defn $exit
  []
  (Thread/sleep 1000)
  (shutdown-agents))

(comment
 ;; Example
  (let [text "bar"]
    ($> echo ~text))
  ($> ls "fo ba" ~*pwd* ~(string/trim-newline ($> echo 1)))
  ($-> ls cat (> / Users bstiles tmp))

  )
