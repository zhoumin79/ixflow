(ns ixflow.layout.routing.pipeline
  (:require [ixflow.layout.routing.port :as port]
            [ixflow.layout.routing.grid :as grid]
            [clojure.string :as str]))

(defn calculate-route
  "完整的路由计算管线
   source-node, target-node: {:bbox {:x :y :w :h} ...}
   obstacles: Set of grid coords or compatible structure
   bounds: {:x :y :w :h} routing area"
  [source-node target-node obstacles bounds]
  (let [;; 1. 端口选择
        ports (port/select-best-ports source-node target-node {})
        start-point (:source-point ports)
        end-point (:target-point ports)

        ;; 2. 正交寻径 (A*)
        ;; 注意：obstacles 需要是 grid set，这里暂时简化假设传入的 obstacles 已经是处理好的
        ;; 实际应用中可能需要在这里把 bounds 内的其他 node 转为 obstacles
        raw-path (grid/find-path start-point end-point obstacles bounds)

        ;; 3. 路径简化
        simplified-path (if raw-path
                          (grid/simplify-path raw-path)
                          [start-point end-point]) ;; Fallback to direct line if no path found

        ;; 4. 生成元数据
        segments (vec (partition 2 1 simplified-path))
        mid-idx (int (/ (count segments) 2))
        mid-segment (get segments mid-idx)
        label-anchor (if mid-segment
                       (let [[[x1 y1] [x2 y2]] mid-segment]
                         [(/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)])
                       [(/ (+ (first start-point) (first end-point)) 2.0)
                        (/ (+ (second start-point) (second end-point)) 2.0)])]

    (merge ports
           {:points simplified-path
            :segments segments
            :polyline (str/join " " (map (fn [[x y]] (str x "," y)) simplified-path))
            :label-anchor label-anchor})))
