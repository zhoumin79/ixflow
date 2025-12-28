(ns xflow.theme.rule
  "规则管理和分类模块"
  (:require [clojure.string :as str]
            [xcommon.io :refer [load-edn-from-resource]]))

(defn load-rules
  "从资源文件加载规则配置"
  []
  (-> "presets/rules.edn"
      load-edn-from-resource
      :rules))

(defn classify-rule-by-name
  "根据规则名称对规则进行分类
  参数:
  - rule-name: 规则名称（关键字或字符串）
  返回分类关键字: :pool, :lane, :shape, :milestone, 或 :other"
  [rule-name]
  (let [name-str (if (keyword? rule-name)
                   (name rule-name)
                   (str rule-name))]
    (cond
      ;; 优先匹配更具体的模式
      (str/includes? name-str "shape") :shape
      (str/includes? name-str "milestone") :milestone
      (str/includes? name-str "pool") :pool
      (str/includes? name-str "lane") :lane
      :else :other)))

(defn classify-condition-by-type
  "根据条件向量的类型对规则进行分类
  参数:
  - condition-vector: 条件向量，如 [:pool-idx 1] 或 [:lane-idx :any]
  返回分类关键字: :pool, :lane, :shape, :milestone, 或 :other"
  [condition-vector]
  (when (vector? condition-vector)
    (let [first-element (first condition-vector)]
      (cond
        (= first-element :pool-idx) :pool
        (= first-element :lane-idx) :lane
        ;; 如果条件向量同时包含pool和lane，优先按pool分类
        (and (some #{:pool-idx} condition-vector)
             (some #{:lane-idx} condition-vector)) :pool
        ;; 其他情况归类为other
        :else :other))))

(defn classify-swimlane-rules
  "对泳道规则进行分类
  参数:
  - swimlane-rules: 泳道规则映射，键为条件向量，如 [:pool-idx 1] 或 [:lane-idx :any]
  返回分类后的规则映射: {:pool {...}, :lane {...}, :shape {...}, :milestone {...}, :other {...}}"
  [swimlane-rules]
  (when swimlane-rules
    (reduce (fn [acc [condition-vector rule-config]]
              (let [category (classify-condition-by-type condition-vector)]
                (assoc-in acc [category condition-vector] rule-config)))
            {}
            swimlane-rules)))

(defn get-rules-by-category
  "获取指定分类的规则
  参数:
  - swimlane-rules: 泳道规则映射
  - category: 分类关键字 (:pool, :lane, :shape, :milestone, :other)
  返回该分类的规则映射"
  [swimlane-rules category]
  (when swimlane-rules
    (let [classified (classify-swimlane-rules swimlane-rules)]
      (get classified category {}))))

(defn get-pool-related-rules
  "获取池相关的规则"
  [swimlane-rules]
  (get-rules-by-category swimlane-rules :pool))

(defn get-lane-related-rules
  "获取泳道相关的规则"
  [swimlane-rules]
  (get-rules-by-category swimlane-rules :lane))

(defn get-shape-related-rules
  "获取形状相关的规则"
  [swimlane-rules]
  (get-rules-by-category swimlane-rules :shape))

(defn get-milestone-related-rules
  "获取里程碑相关的规则"
  [swimlane-rules]
  (get-rules-by-category swimlane-rules :milestone))

(defn reorganize-rules-by-priority
  "根据优先级重新组织规则
  参数:
  - swimlane-rules: 原始的泳道规则映射
  - priority-order: 优先级顺序向量，如 [:pool :lane :shape :milestone :other]
  返回重新组织优先级后的规则映射（使用ArrayMap保持顺序）"
  [swimlane-rules priority-order]
  (if-not (and swimlane-rules priority-order)
    swimlane-rules
    (let [classified (classify-swimlane-rules swimlane-rules)
          ordered-rules (mapcat (fn [category]
                                  (seq (get classified category {})))
                                priority-order)]
      ;; 使用apply array-map保持插入顺序
      (apply array-map (mapcat identity ordered-rules)))))

(defn reorganize-swimlane-rules-by-priority
  "根据主题指定的规则类型重新组织泳道规则的优先级
  参数:
  - swimlane-rules: 原始的泳道规则映射
  - rule-type: 规则类型，如 :pool-task 或 :lane-task
  返回重新组织优先级后的规则映射（使用ArrayMap保持顺序）"
  [swimlane-rules rule-type]
  (if-not (and swimlane-rules rule-type)
    swimlane-rules
    (case rule-type
      ;; pool-task优先：池相关规则优先于泳道相关规则
      :pool-task
      (reorganize-rules-by-priority swimlane-rules [:pool :lane :shape :milestone :other])

      ;; lane-task优先：泳道相关规则优先于池相关规则
      :lane-task
      (reorganize-rules-by-priority swimlane-rules [:lane :pool :shape :milestone :other])

      ;; 默认情况：保持原有顺序
      swimlane-rules)))

(defn get-rule-categories
  "获取所有规则分类
  参数:
  - swimlane-rules: 泳道规则映射
  返回分类列表: [:pool :lane :shape :milestone :other]"
  [swimlane-rules]
  (when swimlane-rules
    (let [classified (classify-swimlane-rules swimlane-rules)]
      (keys classified))))

(defn validate-rule-structure
  "验证规则结构的有效性
  参数:
  - rule-config: 规则配置
  返回验证结果: {:valid? boolean :errors [...]}"
  [rule-config]
  (let [errors []]
    ;; 这里可以添加更多的验证逻辑
    {:valid? (empty? errors)
     :errors errors}))

(defn merge-rule-configs
  "合并多个规则配置
  参数:
  - base-config: 基础规则配置
  - override-configs: 覆盖规则配置序列
  返回合并后的规则配置"
  [base-config & override-configs]
  (reduce (fn [acc config]
            (merge-with merge acc config))
          base-config
          override-configs))

(defn resolve-rule-value
  "解析规则值
   例如: [:pool-colors :mod-pool-idx]
   context: theme context
   indices: {:pool-idx 0 :lane-idx 1}"
  [rule-value context indices]
  (if (vector? rule-value)
    (let [[ref-key mod-key] rule-value]
      (if (and ref-key mod-key)
        (let [base-colors (get-in context [:colors (keyword (name ref-key))])

              ;; Resolve specific colors if available (e.g. lane colors based on pool color)
              colors (if (and (= (keyword (name ref-key)) :lane-colors)
                              (:pool-idx indices)
                              (get-in context [:colors :pool-lane-map]))
                       (let [pool-colors (get-in context [:colors :pool-colors])
                             pool-idx (:pool-idx indices)
                             pool-color (when (and pool-colors (seq pool-colors))
                                          (nth pool-colors (mod pool-idx (count pool-colors))))
                             specific-lane-colors (when pool-color
                                                    (get-in context [:colors :pool-lane-map pool-color]))]
                         (or specific-lane-colors base-colors))
                       base-colors)

              idx (cond
                    (= mod-key :mod-pool-idx) (:pool-idx indices)
                    (= mod-key :mod-lane-idx) (:lane-idx indices)
                    :else 0)
              count (count colors)]
          (if (and colors (pos? count))
            (nth colors (mod idx count))
            nil))
        rule-value))
    rule-value))

(defn match-condition?
  "判断条件是否匹配
  condition: [:pool-idx :any] 或 [:pool-idx 1]
  indices: {:pool-idx 0 :lane-idx 1}"
  [condition indices]
  (if (vector? condition)
    (let [[type value] condition]
      (case type
        :pool-idx (or (= value :any) (= value (:pool-idx indices)))
        :lane-idx (or (= value :any) (= value (:lane-idx indices)))
        :completed (or (= value :any) (= value (:completed indices))) ; Handle task completion status if needed
        true))
    true))

(defn get-matching-rule
  "获取匹配的规则配置
  rules: 规则集合 {:pool-header {[:pool-idx :any] ...}}
  category: 规则分类 :pool-header
  indices: 索引集合 {:pool-idx 0 ...}"
  [rules category indices]
  (let [category-rules (get rules category)]
    (when category-rules
      (reduce (fn [matched [condition rule-config]]
                (if (match-condition? condition indices)
                  (merge matched rule-config)
                  matched))
              {}
              category-rules))))
