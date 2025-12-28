(ns xcommon.io
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [xcommon.string :refer [remove-last-n]])
  (:import [java.io File]
           [java.util.jar JarFile JarEntry]
           [java.net URL URLDecoder URI]
           [java.util.regex Pattern]))

(defn load-edn-from-resource
  [filename]
  (let [res (io/resource filename)]
    (if res
      (-> res slurp edn/read-string)
      (let [f (io/file "resources" filename)]
        (if (.exists f)
          (-> f slurp edn/read-string)
          (throw (ex-info (str "Could not find resource: " filename)
                          {:filename filename})))))))

(defn load-edn
  [filename]
  (-> filename
      (slurp)
      (edn/read-string)))

(defn slurp-bytes ^bytes [src]
  (cond
    (bytes? src)
    src

    (instance? java.io.InputStream src)
    (.readAllBytes ^java.io.InputStream src)

    :else
    (with-open [is (io/input-stream src)]
      (.readAllBytes is))))

(defn extension
  "Get file extension without dot, e.g. \"txt\" from \"/tmp/a.txt\"."
  [path]
  (let [name (if (instance? File path)
               (.getName ^File path)
               (str path))
        idx (.lastIndexOf name ".")]
    (when (pos? idx)
      (.substring name (inc idx)))))

(defn list-all [dir]
  (->> dir
       io/resource
       .getPath
       clojure.java.io/file
       file-seq
       (filter #(.isFile ^File %))
       (map (fn [file] [(keyword (remove-last-n (.getName ^File file) 4)) (.getPath ^File file)]))
       (remove nil?)
       (into {})))

(defn get-filename-from-resource [resource]
  (-> (io/resource resource)
      .getFile
      ;; (java.net.URLDecoder/decode "UTF-8")
      ))

(defn get-files [path ext]
  (filter #(= ext (extension %)) (map #(.getPath ^File %)
                                      (file-seq (io/file (io/resource path))))))

(defn write-file
  "Write content to a file, creating parent directories if needed."
  [path content]
  (io/make-parents path)
  (spit path content))

;; (defn write-json
;;   "Writes the data as JSON to `filename`."
;;   [filename data]
;;   (io/make-parents filename)
;;   (with-open [wrt (io/writer filename)]
;;     (json/write-json wrt data)))

#_(defn camel-case->kebab-case [string]
    (reduce
     (fn [result char]
       (if (= (str char) (str/upper-case char))
         (str result "-" (str/lower-case char))
         (str result char)))
     ""
     string))

(let [camelcase-pattern (re-pattern (str/join "|"
                                              ["(?<=[A-Z])(?=[A-Z][a-z])"
                                               "(?<=[^A-Z])(?=[A-Z])"
                                               "(?<=[A-Za-z])(?=[^A-Za-z])"]))]
  (defn camel-case->kebab-case
    [s]
    (-> s
        (str/replace camelcase-pattern "-")
        (str/lower-case))))

;; (defn json->edn
;;   ([src-file]
;;    (json/read-json (io/file src-file) :key-fn #(keyword (camel-case->kebab-case %))))
;;   ([src-file dest-file]
;;    (->> (json/read-json (io/file src-file) :key-fn #(keyword (camel-case->kebab-case %)))
;;         (spit (io/file dest-file)))))

(comment
  (:time-mode (load-edn-from-resource "sample/gantt/pa.edn"))

  ;; (->> (json/read-json (io/file "resources/sample/roadmap/1.json") :key-fn #(keyword (camel-case->kebab-case %)))
  ;;      :version
  ;;      ;(spit (io/file "resources/sample/roadmap/1.edn"))
  ;;      ;; :style
  ;;      ;; :milestones
  ;;      ;; :timeband-style
  ;;      )

  (camel-case->kebab-case "SuperHero")

  (:swimlanes-v-2 (load-edn-from-resource "sample/roadmap/1.edn")))
