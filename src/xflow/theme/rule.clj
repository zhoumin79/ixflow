(ns xflow.theme.rule
  "规则管理和分类模块"
  (:require [clojure.string :as str]
            [xcommon.io :refer [load-edn-from-resource]]))

(defn load-rules
  "从资源文件加载规则配置"
  []
  (load-edn-from-resource "presets/rules.edn"))

(defn classify-rule-by-name [rule-name]
  (cond
    (keyword? rule-name) rule-name
    (string? rule-name) (keyword rule-name)
    :else :default))

(defn classify-condition-by-type [condition-vector]
  (let [[key val] condition-vector]
    (cond
      (= key :pool-idx) :pool
      (= key :lane-idx) :lane
      (= key :completed) :state
      (= key :type) :type ;; New: Support classification by node type
      :else :general)))

(defn classify-swimlane-rules [swimlane-rules]
  (reduce-kv (fn [m k v]
               (assoc m k v))
             {}
             swimlane-rules))

(defn get-rules-by-category [all-rules category]
  (get all-rules category {}))

(defn get-layout-rules [all-rules]
  (get all-rules :layout {}))

(defn get-pool-related-rules [swimlane-rules]
  (select-keys swimlane-rules [:pool-header :pool-task]))

(defn get-lane-related-rules [swimlane-rules]
  (select-keys swimlane-rules [:lane-header :lane-task]))

(defn get-shape-related-rules [swimlane-rules]
  (select-keys swimlane-rules [:task :pool-task :lane-task]))

(defn get-milestone-related-rules [swimlane-rules]
  (select-keys swimlane-rules [:milestone]))

(defn reorganize-rules-by-priority [swimlane-rules priority-order]
  swimlane-rules)

(defn reorganize-swimlane-rules-by-priority [swimlane-rules rule-type]
  swimlane-rules)

(defn get-rule-categories [swimlane-rules]
  (keys swimlane-rules))

(defn validate-rule-structure [rule-config]
  (and (map? rule-config)
       (every? map? (vals rule-config))))

(defn merge-rule-configs [base-config & override-configs]
  (apply merge-with merge base-config override-configs))

(defn resolve-rule-value [rule-value context indices]
  (if (vector? rule-value)
    (let [[category key] rule-value]
      ;; TODO: Implement actual lookup logic if needed, currently just returns keyword
      rule-value)
    rule-value))

(defn match-condition? [condition indices node]
  (let [[key val] condition]
    (cond
      (= key :any) true
      (= key :mod-pool-idx) (if-let [idx (:pool-idx indices)]
                              (= (mod idx 2) val)
                              false)
      (= key :mod-lane-idx) (if-let [idx (:lane-idx indices)]
                              (= (mod idx 2) val)
                              false)
      (= key :type) (= (:type node) val) ;; Match node type
      (= key :completed) (= (:completed indices) val) ;; Legacy support
      :else (= (get indices key) val))))

(defn get-matching-rule
  "Finds the best matching rule for a node given a category of rules (e.g. :layout rules).
   indices can now include the node itself for property matching."
  [rules-category indices & [node]]
  (let [context (merge indices node)
        ;; Sort keys to ensure deterministic application order.
        ;; :default should usually come first if we want others to override it.
        sorted-rules (sort-by key rules-category)]
    (reduce (fn [acc [rule-name rule-def]]
              (let [matches (filter (fn [[condition config]]
                                      (match-condition? condition context node))
                                    rule-def)]
                (if (seq matches)
                  (merge acc (second (first matches))) ;; Simple first match wins strategy for now
                  acc)))
            {}
            sorted-rules)))
