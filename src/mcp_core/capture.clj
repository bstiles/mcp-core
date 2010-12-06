(ns mcp-core.capture
  (:import (java.io IOException)
           (java.util Calendar Date))
  (:require [clojure.contrib.io :as io]))

(defn base-capture-dir
  []
  (if-let [override-dir (System/getProperty "mcp-core.capture.dir")]
    (io/file override-dir)
    (io/file (System/getProperty "user.home") "MCP" "Working")))

(def *calendar* (Calendar/getInstance))

(defn- get-year-month-day
  []
  (let [c (Calendar/getInstance)]
    [(.get c Calendar/YEAR) (+ 1(.get c Calendar/MONTH)) (.get c Calendar/DAY_OF_MONTH)]))

(defn- get-timestamp
  []
  (let [d (Date.)]
   (format "%02d%02d.%02d" (.getHours d) (.getMinutes d) (.getSeconds d))))

(defn- unique-names
  ([] (unique-names nil))
  ([prefix]
     (let [counter (atom 1)]
       (fn [] (let [suffix (swap! counter inc)]
                (if prefix
                  (str prefix (when (< 2 suffix) suffix))
                  (str suffix)))))))

(defn- finite-names
  [name-coll]
  (let [names (atom name-coll)]
    (fn [] (let [name (first @names)]
             (swap! names next)
             name))))

(defn- create-dir-or-die
  [dir]
  (when-not (.exists dir)
    (when-not (.mkdirs dir)
      (throw (IOException. (format "Couldn't create %s!" dir)))))
  (when-not (.isDirectory dir)
    (throw (IllegalStateException. (format "%s is not a directory!" dir)))))

(defn get-capture-dir
  [& {:keys [category dir-name fail-if-exists]
      :or {category "anonymous"
           dir-name "anonymous"
           fail-if-exists nil}
      :as args}]
  (let [unique-fn (cond (fn? dir-name) dir-name
                        (coll? dir-name) (finite-names dir-name)
                        :else (unique-names (str dir-name)))]
    (loop [dir (io/file (base-capture-dir)
                        category
                        (apply str (interpose "-" (take 2 (get-year-month-day))))
                        (str (nth (get-year-month-day) 2))
                        (unique-fn))]
      (if (and (not fail-if-exists) (.exists dir))
        ;; Already exists
        dir
        (if-not (.exists dir)
          ;; Doesn't exist
          (if-not (.mkdirs dir)
            (throw (IOException. (str "Could not create " dir)))
            dir)
          ;; Exists but we are required not to re-use an existing dir
          (if-let [next-unique (unique-fn)]
            (recur (io/file (.getParent dir) next-unique))
            (throw (IllegalStateException. (format "Couldn't create a unique dir (%s)!" args)))))))

    ))

(defn get-capture-files
  [files & {:keys [fail-if-exists] :or {fail-if-exists true} :as args}]
  (let [get-capture-dir-args (apply concat (merge args {:fail-if-exists nil}))
        dir (apply get-capture-dir get-capture-dir-args)]
    (doall (for [file-name files]
             (let [unique-fn (cond (fn? file-name) file-name
                                   (coll? file-name) (finite-names file-name)
                                   :else (unique-names (str file-name)))]
               (loop [file (io/file dir (unique-fn))]
                 (if (and (not fail-if-exists) (.exists file))
                   ;; Already exists
                   file
                   (if-not (.exists file)
                     ;; Doesn't exist
                     (if-not (.createNewFile file)
                       (throw (IOException. (str "Could not create " file)))
                       file)
                     ;; Exists but we are required not to re-use an existing file
                     (if-let [next-unique (unique-fn)]
                       (recur (io/file (.getParent file) next-unique))
                       (throw (IllegalStateException. (format "Couldn't create a unique file %s (%s)!" file-name args))))))))))))
