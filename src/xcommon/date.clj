(ns xcommon.date
  (:require
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.temporal.chrono-unit :as chrono-unit])
  (:import [java.time LocalDate Month]
           [java.util Collections]
           [java.text SimpleDateFormat]
           [java.time.format DateTimeFormatter]
           [java.time DayOfWeek]
           [java.time.temporal TemporalAdjusters IsoFields]
           [java.util TimeZone]))

(defn get-monday [date]
  (let [date-str (if (keyword? date) (name date) date)
        monday (ld/with (ld/parse date-str) (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY))]
    (ld/to-string monday)))

(defn get-sunday [date]
  (let [date-str (if (keyword? date) (name date) date)
        sunday (ld/with (ld/parse date-str) (TemporalAdjusters/nextOrSame DayOfWeek/SUNDAY))]
    (ld/to-string sunday)))

(defn get-timeline-period [str-date mode]
  (let [parse-int #(Integer/parseInt %)
        date-str (if (keyword? str-date) (name str-date) str-date)
        date (ld/parse date-str)
        this-year (parse-int (subs date-str 0 4))
        this-month (parse-int (subs date-str 5 7))
        make-date (fn [year month day] (ld/of year month day))]
    (cond
      (= mode :week)
      (let [;this-week (.get ^LocalDate date IsoFields/WEEK_OF_WEEK_BASED_YEAR)
            timeline-start-period (get-monday date-str)
            timeline-end-period (get-sunday date-str)]
        [timeline-start-period timeline-end-period])

      (= mode :month)
      (let [timeline-start-period (make-date this-year this-month 1)
            days-in-month (ld/length-of-month date)
            timeline-end-period (make-date this-year this-month days-in-month)]
        [(ld/to-string timeline-start-period) (ld/to-string timeline-end-period)])

      (= mode :quarter)
      (let [this-quarter (.get ^LocalDate date IsoFields/QUARTER_OF_YEAR)
            this-month (case this-quarter
                         1 1
                         2 4
                         3 7
                         4 10)
            timeline-start-period (make-date this-year (inc (* 3 (quot (dec this-month) 3))) 1)
            timeline-end-period (ld/minus-days (make-date (+ this-year (quot (* 3 this-quarter) 12)) (inc (mod (* 3 this-quarter) 12)) 1) 1)]
        [(ld/to-string timeline-start-period) (ld/to-string timeline-end-period)])

      (= mode :half-year)
      (let [this-half (if (> this-month 6) 2 1)
            half-start (if (= this-half 2) 7 1)
            half-end (if (= this-half 2) 12 6)
            ;; days-in-half (ld/length-of-year this-year)
            timeline-start-period (make-date this-year half-start 1)
            timeline-end-period (make-date this-year half-end 30)]
        [timeline-start-period timeline-end-period])

      (= mode :year)
      (let [timeline-start-period (make-date this-year 1 1)
            days-in-year (ld/with-day-of-year date (ld/length-of-year date))
            timeline-end-period days-in-year #_(make-date this-year 12 days-in-year)]
        [(ld/to-string timeline-start-period) (ld/to-string timeline-end-period)]))))

(comment
  (get-timeline-period "2024-06-30" :week)
  (ld/with-day-of-year (ld/parse "2025-02-16") (ld/length-of-year (ld/parse "2025-02-16")))
  (ld/length-of-month (ld/parse "2025-02-16"))

  (.get ^LocalDate (ld/parse "2024-01-11") IsoFields/WEEK_OF_WEEK_BASED_YEAR)

  (.get ^LocalDate (ld/parse "2024-10-04") IsoFields/QUARTER_OF_YEAR)

  (get-sunday "2024-12-30"))

(defn generate-dates [start-date end-date interval]
  (let [start-str (if (keyword? start-date) (name start-date) start-date)
        end-str (if (keyword? end-date) (name end-date) end-date)
        start (ld/parse start-str)
        end (ld/parse end-str)]
    (case interval
      "year"
      ;; 参考roadmapper实现：基于月份偏移量生成年份序列
      ;; 计算从开始日期到结束日期需要多少个年份项
      (let [start-year (ld/get-year start)
            end-year (ld/get-year end)
            years-between (- end-year start-year)
            ;; 生成每个年份的代表日期（使用该年的1月1日）
            year-dates (for [i (range (inc years-between))]
                         (ld/of (+ start-year i) 1 1))]
        year-dates)

      "quarter"
      ;; 生成季度序列
      (let [start-year (ld/get-year start)
            start-month (ld/get-month-value start)
            start-quarter (inc (quot (dec start-month) 3))
            end-year (ld/get-year end)
            end-month (ld/get-month-value end)
            end-quarter (inc (quot (dec end-month) 3))
            ;; 计算总季度数
            total-quarters (+ (* (- end-year start-year) 4)
                              (- end-quarter start-quarter)
                              1)
            ;; 生成每个季度的开始日期
            quarter-dates (for [i (range total-quarters)
                                :let [current-year (+ start-year (quot (+ start-quarter i -1) 4))
                                      current-quarter (inc (mod (+ start-quarter i -1) 4))
                                      quarter-start-month (case current-quarter
                                                            1 1
                                                            2 4
                                                            3 7
                                                            4 10)]]
                            (ld/of current-year quarter-start-month 1))]
        quarter-dates)

      "day"
      (concat (take-while #(<= (ld/compare-to % end) 0)
                          (iterate #(ld/plus-days % 1) start))
              [end])

      (or "week" "week-n")
      (let [week-start (if (= "week" interval)
                         (ld/parse (get-monday (ld/to-string start)))
                         start)]
        (take-while #(<= (ld/compare-to % end) 0)
                    (iterate #(ld/plus-weeks % 1) week-start)))

      "month"
      (take-while #(<= (ld/compare-to % end) 0)
                  (iterate #(ld/plus-months % 1) start)))))

(defn generate-str-dates [start-date end-date interval]
  (map (fn [date]
         (let [str-date (ld/to-string date)]
           (case interval
             "year" (subs str-date 0 4)
             "day" (subs str-date 5)
             (or "week" "week-n") (subs str-date 5)
             "month" (str (subs str-date 5 7) "月")))) (generate-dates start-date end-date interval)))

(defn between [start end interval]
  (inc (abs (chrono-unit/between (case interval
                                   "year" chrono-unit/years
                                   "day" chrono-unit/days
                                   "week" chrono-unit/weeks
                                   "month" chrono-unit/months) (ld/parse (if (keyword? start) (name start) start)) (ld/parse (if (keyword? end) (name end) end))))))

(defn num2pct
  "Format Percentage"
  [n]
  (when n (format "%.4f%%" (* 100 (parse-double (str n))))))

(defn get-min-max-date [dates]
  (let [min-date (Collections/min dates)
        max-date (Collections/max dates)]
    {:min min-date :max max-date}))

;; (num2date 43173.0)
(comment (defonce sdf (SimpleDateFormat. "HH:mm:ss"))
         (.setTimeZone sdf (TimeZone/getTimeZone "UTC"))

         (defn num2date
           "Format Excel Date"
           [n]
           (when n (.format (.plusDays (LocalDate/of 1899 Month/DECEMBER 30) (parse-double (str n)))
                            (DateTimeFormatter/ofPattern "yyyy-mm-dd"))))

         (defn num2time
           "Format Excel Time"
           [n]
           (when n (.format sdf (* (parse-double (str n)) 24 60 60 1000)))))
