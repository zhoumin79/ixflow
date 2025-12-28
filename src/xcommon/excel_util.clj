(ns xcommon.excel-util
  (:require
   [clojure.java.io :as io]
   [zero-one.fxl.core :as fxl])
  (:import [java.text SimpleDateFormat]
           (org.apache.poi.ss.usermodel   Cell
                                          DateUtil)))

(defn read-xlsx [xlsx sheet group-num]
  (->> ^java.io.File xlsx
       io/file
       .getPath
       fxl/read-xlsx!
       (filter #(= (get-in % [:coord :sheet]) (str sheet)))
       (filter #(>= (get-in % [:coord :row]) 0))
       (map :value)
       (map str)
       (remove nil?)
       (partition group-num)))

(defn smart-read-cell [^Cell cell]
  (if (DateUtil/isCellDateFormatted cell)
    (.getDateCellValue cell)
    (.getNumericCellValue cell)))

(defn get-date-str [double]
  (let [sdf (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (-> (.format sdf (DateUtil/getJavaDate double))
        (subs 0 10))))

(comment
  (read-xlsx (io/resource "plan.xlsx") "plan" 4)

         (->> (read-xlsx (io/resource "gantt.xlsx") "Sheet2" 3)
              rest
              flatten
              (map (fn [v] (if (double? (parse-double v)) (get-date-str (parse-double v)) v)))
              (partition 3)
              (map #(zipmap [:task :start :end] %))
              (map (fn [task] (cond
                                (and (empty? (:start task)) (not-empty (:end task))) (assoc task :start (:end task))
                                (and (empty? (:end task)) (not-empty (:start task))) (assoc task :end (:start task))
                                :else task))))

         (.getPath (io/file (io/resource "plan.xlsx")))


         (->> (io/resource "sample/roadmap/pool-timeline.xlsx")
              io/file
              .getPath
              fxl/read-xlsx!
              (filter #(= (get-in % [:coord :sheet]) (str "Timeline Data")))
              (filter #(>= (get-in % [:coord :row]) 0))
              (map :value)
              (map str)
              (remove nil?)
              (partition 9)
              (map rest)
              (map drop-last)
              last
              )

(parse-double "abcasdf11")
         )

