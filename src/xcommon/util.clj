(ns xcommon.util
  (:refer-clojure :exclude [name parents])
  (:require
   [clojure.java [io :as io]])
  (:import [java.nio.file Files Path]
           [java.io File]))

(defn is-chinese? [s]
  (> (int (first s)) 0x7f))

(defn get-files [path ext]
  (letfn [(extension [p]
            (let [name (if (instance? File p)
                         (.getName ^File p)
                         (str p))
                  idx (.lastIndexOf name ".")]
              (when (pos? idx)
                (.substring name (inc idx)))))]
    (filter #(= ext (extension %))
            (map #(.getPath ^File %)
                 (file-seq (io/file path))))))

(defn depth-search [pred? coll]
  (filter pred? (tree-seq coll? seq coll)))

(comment
  (depth-search number? [1 6 7 [:2 3 8] [8 "2" 5 [7]]])

  (tree-seq coll? seq [1 6 7 [2 3 8] [8 2 5 [7]]]))

(defn convert [m]
  (zipmap (map #(str (symbol %)) (keys m)) (vals m)))

(defn copy-uri-to-file [uri file]
  (with-open [in (clojure.java.io/input-stream uri)
              out (clojure.java.io/output-stream file)]
    (clojure.java.io/copy in out)))

;;primarily those that help with interop.
;;aux functions...possibly move these to a util namespace.
;;=======================================================
;;use reflection to rip out all the constants into a map for us...

(defn class-name
  "Rips the class name from a class."
  [^java.lang.Class cls]
  (.getName cls))

(defn class-member
  "Aux function to construct an idiomatic clojure symbol for class 
   member access ala cls/memb."
  [cls memb]
  (symbol (str (class-name cls) "/" memb)))

(defn camel->under
  "Coerces a CamelCase string to a CAMEL_CASE style java constant fieldname."
  [s]
  (->> (str s)
       (re-seq #"[A-Z]*[^A-Z]+")
       (map clojure.string/upper-case)
       (clojure.string/join "_")))

(defmacro get-enums
  "Given a java class and an enum datatype, such as 
   XSLFPictureData and PictureType, returns a seq of 
   [k v] for each enumerated type, where enum 
   appears as PictureType.k -> v. "
  [cls root]
  (let [tgt (str (camel->under (str root)) "_")]
    `(get-constants ~cls
                    (fn [s#] (clojure.string/replace (str s#) ~tgt "")))))

(defn get-or-err
  "Returns exception if k is not in map m."
  [m k]
  (or (get m k)
      (throw (Exception.
              (str [:unnknown-key k])))))

(defn file->bytes
  "Slurps all byes from a file immediately, returning a byte array."
  [p]
  (-> (io/file p)
      ^Path (.toPath)
      (Files/readAllBytes)))

(comment
  (copy-uri-to-file "https://ederbeauty.com/wp-content/uploads/2020/12/eder-beauty-body-treatment-414x414.jpg" "xyz.jpg"))
