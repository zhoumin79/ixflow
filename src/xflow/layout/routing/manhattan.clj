(ns xflow.layout.routing.manhattan
  (:require [clojure.set :as set]))

(defn- intersects-node? [segment node]
  (let [{:keys [x1 y1 x2 y2]} segment
        {:keys [x y w h]} node
        pad 10
        nx (- x pad) ny (- y pad)
        nw (+ w (* 2 pad)) nh (+ h (* 2 pad))]
    ;; Check if vertical segment intersects node box
    ;; Segment is vertical: x1 == x2
    (if (= x1 x2)
      (and (>= x1 nx) (<= x1 (+ nx nw))
           (or (and (>= y1 ny) (<= y1 (+ ny nh)))
               (and (>= y2 ny) (<= y2 (+ ny nh)))
               (and (<= y1 ny) (>= y2 (+ ny nh)))))
      ;; Horizontal segment
      (and (>= y1 ny) (<= y1 (+ ny nh))
           (or (and (>= x1 nx) (<= x1 (+ nx nw)))
               (and (>= x2 nx) (<= x2 (+ nx nw)))
               (and (<= x1 nx) (>= x2 (+ nx nw))))))))

(defn- find-safe-mid [p1 p2 nodes mode]
  ;; Finds a mid-x (or mid-y) that doesn't intersect any node
  ;; p1, p2 are endpoints of the edge
  ;; We are looking for a vertical segment at mid-x (in horizontal mode)
  ;; or horizontal segment at mid-y (in vertical mode)

  (let [lr? (= mode "horizontal")

        ;; Initial guess: Average
        start-mid (if lr? (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2))

        ;; Define the segment we are trying to place
        ;; If LR: Vertical segment from min-y to max-y at x=mid
        seg-min (if lr? (min (:y p1) (:y p2)) (min (:x p1) (:x p2)))
        seg-max (if lr? (max (:y p1) (:y p2)) (max (:x p1) (:x p2)))

        ;; Function to check collision for a given mid value
        check (fn [mid]
                (let [segment (if lr?
                                {:x1 mid :x2 mid :y1 seg-min :y2 seg-max}
                                {:x1 seg-min :x2 seg-max :y1 mid :y2 mid})]
                  (some #(intersects-node? segment %) nodes)))]

    (if (not (check start-mid))
      start-mid
      ;; Collision! Search outwards
      (loop [offset 20]
        (if (> offset 200) ;; Give up
          start-mid
          (let [try-pos (+ start-mid offset)
                try-neg (- start-mid offset)]
            (cond
              (not (check try-pos)) try-pos
              (not (check try-neg)) try-neg
              :else (recur (+ offset 20)))))))))

(defn- find-safe-channel [p1 p2 nodes mode]
  ;; Finds a safe channel for side-loop
  ;; For TB (Left-Left): Vertical channel to the left of p1.x and p2.x
  ;; For LR (Top-Top): Horizontal channel above p1.y and p2.y
  (let [vertical? (not= mode "horizontal")

        start-val (if vertical?
                    (- (min (:x p1) (:x p2)) 30)
                    (- (min (:y p1) (:y p2)) 30))

        seg-min (if vertical? (min (:y p1) (:y p2)) (min (:x p1) (:x p2)))
        seg-max (if vertical? (max (:y p1) (:y p2)) (max (:x p1) (:x p2)))

        check (fn [val]
                (let [segment (if vertical?
                                {:x1 val :x2 val :y1 seg-min :y2 seg-max} ;; Vertical line
                                {:x1 seg-min :x2 seg-max :y1 val :y2 val})] ;; Horizontal line
                  (some #(intersects-node? segment %) nodes)))]

    (loop [offset 0]
      (if (> offset 200)
        start-val
        (let [try-pos (- start-val offset)]
          (if (not (check try-pos))
            try-pos
            (recur (+ offset 20))))))))

(defn- select-ports [n1 n2 mode]
  (let [vertical? (not= mode "horizontal")
        margin 20]
    (if vertical?
      ;; Vertical Layout (TB)
      (let [s-bottom (+ (:y n1) (:h n1))
            t-top (:y n2)
            ;; If target is comfortably below source, use standard Bottom->Top
            enough-space? (> t-top (+ s-bottom margin))]
        (if enough-space?
          {:p1 {:x (+ (:x n1) (/ (:w n1) 2)) :y s-bottom}
           :p2 {:x (+ (:x n2) (/ (:w n2) 2)) :y t-top}
           :strategy :standard}
          ;; Back-edge or side-by-side: Loop around Left side
          {:p1 {:x (:x n1) :y (+ (:y n1) (/ (:h n1) 2))} ;; Left
           :p2 {:x (:x n2) :y (+ (:y n2) (/ (:h n2) 2))} ;; Left
           :strategy :side-loop}))
      ;; Horizontal Layout (LR)
      (let [s-right (+ (:x n1) (:w n1))
            t-left (:x n2)
            ;; If target is comfortably to right of source, use standard Right->Left
            enough-space? (> t-left (+ s-right margin))]
        (if enough-space?
          {:p1 {:x s-right :y (+ (:y n1) (/ (:h n1) 2))}
           :p2 {:x t-left :y (+ (:y n2) (/ (:h n2) 2))}
           :strategy :standard}
           ;; Back-edge: Loop around Top side
          {:p1 {:x (+ (:x n1) (/ (:w n1) 2)) :y (:y n1)} ;; Top
           :p2 {:x (+ (:x n2) (/ (:w n2) 2)) :y (:y n2)} ;; Top
           :strategy :side-loop})))))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Determine mode:
        ;; 1. If Layout is Swimlane, prioritize swimlane-mode (default horizontal)
        ;; 2. Otherwise use direction (tb=vertical, lr=horizontal)
        layout-type (:layout options)
        direction (:direction options)
        swimlane-mode (:swimlane-mode options "horizontal")

        mode (cond
               (= layout-type "swimlane")
               (if (= swimlane-mode "vertical") "vertical" "horizontal")

               (= direction "tb") "vertical"
               (= direction "lr") "horizontal"
               :else "vertical")

        ;; Group edges by from node to handle branching overlaps
        edges-by-from (group-by :from (:edges layout))]

    (update layout :edges
            (fn [_]
              (into []
                    (mapcat (fn [[_ siblings]]
                              (let [edge-geoms
                                    (map (fn [e]
                                           (let [n1 (get nodes-map (:from e))
                                                 n2 (get nodes-map (:to e))]
                                             (if (and n1 n2)
                                               (let [{:keys [p1 p2 strategy]} (select-ports n1 n2 mode)]
                                                 {:e e :p1 p1 :p2 p2 :strategy strategy :valid true})
                                               {:e e :valid false})))
                                         siblings)

                                    valid-geoms (filter :valid edge-geoms)

                                    ;; --- 辅助函数：坐标转浮点数 (避免 SVG 渲染问题) ---
                                    to-double (fn [p]
                                                (when (or (nil? (:x p)) (nil? (:y p)))
                                                  (println "ERROR: Nil coord in point:" p))
                                                {:x (double (:x p)) :y (double (:y p))})

                                    ;; --- 辅助函数：清理点 (转浮点 + 去重) ---
                                    ;; 修复问题：如果相邻点相同（如直线连线），会导致 SVG 圆角计算出现 NaN
                                    clean-points (fn [pts]
                                                   (into [] (dedupe (map to-double pts))))

                                    ;; --- 路径点路由 (处理 Dummy Nodes 产生的长连线) ---
                                    waypoint-results
                                    (keep (fn [geom]
                                            (let [{:keys [e p1 p2]} geom]
                                              (when (seq (:points e))
                                                (let [waypoints (:points e)
                                                      ;; 完整路径: 起点 -> 中间点(Dummy Nodes) -> 终点
                                                      full-path (concat [p1] waypoints [p2])

                                                      ;; 正交连接所有点
                                                      final-points
                                                      (loop [pts full-path
                                                             result []]
                                                        (if (< (count pts) 2)
                                                          result
                                                          (let [curr (first pts)
                                                                next (second pts)
                                                                ;; 正交连接逻辑 (Orthogonal Connection)
                                                                ;; TB模式 (Vertical): (x1,y1) -> (x1,mid) -> (x2,mid) -> (x2,y2)
                                                                ;; LR模式 (Horizontal): (x1,y1) -> (mid,y1) -> (mid,y2) -> (x2,y2)
                                                                segment-pts
                                                                (if (= mode "horizontal")
                                                                  (let [mid (/ (+ (:x curr) (:x next)) 2)]
                                                                    [curr
                                                                     {:x mid :y (:y curr)}
                                                                     {:x mid :y (:y next)}
                                                                     next])
                                                                  (let [mid (/ (+ (:y curr) (:y next)) 2)]
                                                                    [curr
                                                                     {:x (:x curr) :y mid}
                                                                     {:x (:x next) :y mid}
                                                                     next]))]
                                                            (recur (rest pts)
                                                                   (into result (if (empty? result)
                                                                                  segment-pts
                                                                                  (rest segment-pts)))))))] ;; 避免重复起点
                                                  (assoc e :points (clean-points final-points))))))
                                          valid-geoms)

                                    ;; 过滤掉已通过 Waypoint 路由的边
                                    remaining-geoms (remove (fn [g] (seq (:points (:e g)))) valid-geoms)

                                    ;; --- 标准路由 (无中间点) ---
                                    standard-geoms (filter #(= (:strategy %) :standard) remaining-geoms)
                                    side-loop-geoms (filter #(= (:strategy %) :side-loop) remaining-geoms)

                                    ;; --- 标准曼哈顿路由逻辑 (Standard Manhattan Routing) ---
                                    standard-results
                                    (if (seq standard-geoms)
                                      (let [mid-xs (map #(/ (+ (:x (:p1 %)) (:x (:p2 %))) 2) standard-geoms)
                                            mid-ys (map #(/ (+ (:y (:p1 %)) (:y (:p2 %))) 2) standard-geoms)
                                            avg-mid-x (/ (reduce + mid-xs) (count mid-xs))
                                            avg-mid-y (/ (reduce + mid-ys) (count mid-ys))

                                            ;; 计算连线束范围 (Bundle Extent)
                                            bundle-min (if (= mode "horizontal")
                                                         (apply min (map #(min (:y (:p1 %)) (:y (:p2 %))) standard-geoms))
                                                         (apply min (map #(min (:x (:p1 %)) (:x (:p2 %))) standard-geoms)))
                                            bundle-max (if (= mode "horizontal")
                                                         (apply max (map #(max (:y (:p1 %)) (:y (:p2 %))) standard-geoms))
                                                         (apply max (map #(max (:x (:p1 %)) (:x (:p2 %))) standard-geoms)))

                                            ;; 寻找无障碍中间线 (Safe Mid Line)
                                            safe-mid (if (= mode "horizontal")
                                                       (find-safe-mid {:x avg-mid-x :y bundle-min} {:x avg-mid-x :y bundle-max} nodes mode)
                                                       (find-safe-mid {:x bundle-min :y avg-mid-y} {:x bundle-max :y avg-mid-y} nodes mode))]

                                        (map (fn [geom]
                                               (let [{:keys [e p1 p2]} geom]
                                                 (assoc e :points
                                                        (clean-points
                                                         (if (= mode "horizontal")
                                                           [p1
                                                            {:x safe-mid :y (:y p1)}
                                                            {:x safe-mid :y (:y p2)}
                                                            p2]
                                                           [p1
                                                            {:x (:x p1) :y safe-mid}
                                                            {:x (:x p2) :y safe-mid}
                                                            p2])))))
                                             standard-geoms))
                                      [])

                                    ;; --- 侧边回环路由 (Side Loop Routing) - 用于回退边 ---
                                    side-loop-results
                                    (map (fn [geom]
                                           (let [{:keys [e p1 p2]} geom
                                                 channel (find-safe-channel p1 p2 nodes mode)]
                                             (assoc e :points
                                                    (clean-points
                                                     (if (not= mode "horizontal") ;; TB -> Left loop
                                                       [p1
                                                        {:x channel :y (:y p1)}
                                                        {:x channel :y (:y p2)}
                                                        p2]
                                                            ;; LR -> Top loop
                                                       [p1
                                                        {:x (:x p1) :y channel}
                                                        {:x (:x p2) :y channel}
                                                        p2])))))
                                         side-loop-geoms)]

                                (concat waypoint-results standard-results side-loop-results)))
                            edges-by-from))))))

