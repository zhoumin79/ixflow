(ns xcommon.string
  (:require [clojure.string :as str]))

(defn remove-last-n [s n]
  (subs s 0 (- (count s) n)))

(defn s->v
  "string to vector"
  [data]
  (if (vector? data) data (vec (remove empty? (str/split data #" ")))))

(defn str->nodes [s]
  (let [cstr (s->v s)
        cnt  (count cstr)]
    (->> cstr
         (map-indexed (fn [i x]
                        (if (<= i (dec cnt))
                          (str (inc i) "," x "," (inc (inc i)))
                          "")))
         vec)))

(defn number-nodes? [data]
  ;;异常需捕获
  (let [v-data (s->v data)
        ids (map first v-data)]
    (every? #(number? (Integer/parseInt (str %))) ids)))

(defn nodes->v-ms
  "将带编号及连接号的节点字符串或节点向量转换成由映射表组成的向量集合,便于工作流方便调用"
  [strs]
  (let [data (s->v strs)
        edges    (map #(clojure.string/split % #",") data) ; 将字符串向量分割成边向量
        ;; ids (map first edges)
        ;; texts (map second edges)
        ;; to-ids (map #(drop 2 %) edges)
        ]
    (mapv
     (fn [e]
       {:id   (first e)
        :text (second e)
        :to   (str/join "," (drop 2 e))}) edges)))

;;下列用做提取一段以空格为首的多行文本

(defn extract-nodes [s]
  (let [lines (clojure.string/split-lines s)]
    (loop [result [] nodes lines]
      (if (seq nodes)
        (let [node             (first nodes)
              next-level-nodes (filter #(clojure.string/starts-with? % (str node " ")) (rest nodes))
              sub-nodes        (if (empty? next-level-nodes) nil (extract-nodes (apply str (map #(clojure.string/replace % #"^\s+" "") next-level-nodes))))]
          (recur (conj result {:name     (clojure.string/trim (clojure.string/replace node #"^\s+" ""))
                               :children sub-nodes})
                 (concat next-level-nodes (drop-while #(not= % node) (rest nodes)))))
        result))))

