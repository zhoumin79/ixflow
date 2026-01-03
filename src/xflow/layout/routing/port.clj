(ns ixflow.layout.routing.port
  "端口选择模块：负责确定连线的起始和终止端口。
   考虑因素：节点相对位置、拥塞程度、强制约束。"
  (:require [clojure.set :as set]))

(def directions #{:top :bottom :left :right})

(defn- center-point [{:keys [x y w h]}]
  [(+ x (/ w 2.0)) (+ y (/ h 2.0))])

(defn- relative-direction
  "计算目标相对于源的大致方向"
  [src-bbox tgt-bbox]
  (let [[sx sy] (center-point src-bbox)
        [tx ty] (center-point tgt-bbox)
        dx (- tx sx)
        dy (- ty sy)]
    (if (> (Math/abs dx) (Math/abs dy))
      (if (pos? dx) :right :left)
      (if (pos? dy) :bottom :top))))

(defn get-candidate-ports
  "根据相对位置推荐优先端口"
  [src-bbox tgt-bbox]
  (let [main-dir (relative-direction src-bbox tgt-bbox)]
    (case main-dir
      :right {:src :right :tgt :left}
      :left {:src :left :tgt :right}
      :bottom {:src :bottom :tgt :top}
      :top {:src :top :tgt :bottom})))

(defn select-best-ports
  "选择最佳端口。
   future-work: 加入拥塞控制 (congestion) 和 端口槽位 (slots) 分配。"
  [source-node target-node _context]
  (let [src-bbox (:bbox source-node)
        tgt-bbox (:bbox target-node)
        {:keys [src tgt]} (get-candidate-ports src-bbox tgt-bbox)]
    {:source-port src
     :target-port tgt
     ;; 预留具体坐标计算，通常在 layout 阶段根据 port 方向计算具体 point
     :source-point (case src
                     :right [(+ (:x src-bbox) (:w src-bbox)) (+ (:y src-bbox) (/ (:h src-bbox) 2))]
                     :left [(:x src-bbox) (+ (:y src-bbox) (/ (:h src-bbox) 2))]
                     :top [(+ (:x src-bbox) (/ (:w src-bbox) 2)) (:y src-bbox)]
                     :bottom [(+ (:x src-bbox) (/ (:w src-bbox) 2)) (+ (:y src-bbox) (:h src-bbox))])
     :target-point (case tgt
                     :right [(+ (:x tgt-bbox) (:w tgt-bbox)) (+ (:y tgt-bbox) (/ (:h tgt-bbox) 2))]
                     :left [(:x tgt-bbox) (+ (:y tgt-bbox) (/ (:h tgt-bbox) 2))]
                     :top [(+ (:x tgt-bbox) (/ (:w tgt-bbox) 2)) (:y tgt-bbox)]
                     :bottom [(+ (:x tgt-bbox) (/ (:w tgt-bbox) 2)) (+ (:y tgt-bbox) (:h tgt-bbox))])}))
