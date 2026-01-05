(ns xflow.layout.routing.ortho
  (:require [xflow.layout.config :as config]
            [xflow.geometry :as geo]))

(defn- get-node-bounds [node padding]
  (let [x (or (:x node) 0)
        y (or (:y node) 0)
        w (or (:w node) 0)
        h (or (:h node) 0)]
    {:x1 (- x padding)
     :y1 (- y padding)
     :x2 (+ x w padding)
     :y2 (+ y h padding)}))

;; Replaced by xflow.geometry/distribute-points
(def distribute-points geo/distribute-points)

(defn- is-vertical? [direction]
  (let [d (keyword direction)]
    (or (= d :tb) (= d :bt) (= d :vertical))))

;; --- 智能防重叠 (Smart Overlap Prevention) ---

(defn- assign-segment-tracks [segments interval-key-start interval-key-end key-fn]
  "区间着色算法 / 轨道分配 (Track Assignment)。
   用于解决线段重叠问题。
   算法思想：
   将一组共线的线段分配到不同的'轨道'上，确保同一轨道内的线段不重叠。
   类似于贪心着色算法：
   1. 将线段按起始位置排序。
   2. 遍历线段，尝试将其放入现有的轨道中（该轨道的末端 <= 线段起点）。
   3. 如果没有合适轨道，则创建新轨道。
   
   segments: 线段列表
   interval-key-start: 获取线段起点的函数
   interval-key-end: 获取线段终点的函数
   key-fn: 获取结果Map的Key的函数 (通常是 edge-id)
   返回: {key -> track-index}"
  (let [sorted (sort-by interval-key-start segments)
        tracks (atom [])] ;; vector of end-positions
    (reduce (fn [results seg]
              (let [start (interval-key-start seg)
                    end (interval-key-end seg)
                    ;; 查找可用的轨道
                    track-idx (first (keep-indexed
                                      (fn [idx track-end]
                                        (if (<= track-end start) idx nil))
                                      @tracks))
                    final-idx (if track-idx
                                (do
                                  (swap! tracks assoc track-idx end)
                                  track-idx)
                                (do
                                  (swap! tracks conj end)
                                  (dec (count @tracks))))]
                (assoc results (key-fn seg) final-idx)))
            {}
            sorted)))

(defn- nudge-segments-smart [segments axis gap]
  "智能微调 (Smart Nudge)。
   将重叠的线段分散开。
   axis: :x 或 :y (当前正在微调的线段方向，例如垂直布局中微调垂直线段，axis=:y)
   gap: 轨道间距
   
   算法步骤：
   1. 按另一轴坐标分组 (例如垂直线段按 x 坐标分组)。
   2. 对每组共线线段，调用 assign-segment-tracks 分配轨道。
   3. 根据轨道索引计算偏移量 (offset)，使线段整体居中对齐。"
  (let [relevant-segs (filter #(= (:axis %) axis) segments)
        ;; Group by the coordinate on the OTHER axis
        other-axis (if (= axis :x) :y :x)
        grouped (group-by #(get-in % [:p1 other-axis]) relevant-segs)]

    (reduce-kv
     (fn [nudges fixed-coord segs]
       (if (<= (count segs) 1)
         nudges
         (let [;; Sort segments by their interval on the main axis
               sorted-segs (sort-by #(min (get-in % [:p1 axis]) (get-in % [:p2 axis])) segs)
                ;; Assign tracks/offsets
               track-assignments (assign-segment-tracks
                                  sorted-segs
                                  #(min (get-in % [:p1 axis]) (get-in % [:p2 axis]))
                                  #(max (get-in % [:p1 axis]) (get-in % [:p2 axis]))
                                  :id)
                ;; Calculate nudge amount based on track index
               max-track (apply max (vals track-assignments))
               total-width (* max-track gap)
               start-offset (- (/ total-width 2.0))]

           (reduce (fn [acc [edge-id track-idx]]
                     (let [offset (+ start-offset (* track-idx gap))]
                       (assoc acc edge-id offset)))
                   nudges
                   track-assignments))))
     {}
     grouped)))

(defn- extract-segments [routed-edges]
  "提取所有直线段。
   将每条边的路径分解为一系列水平或垂直的线段，用于碰撞检测和微调。"
  (mapcat (fn [edge]
            (let [points (:points edge)
                  id (:id edge)]
              (map (fn [p1 p2]
                     (let [is-vertical (= (:x p1) (:x p2))
                           axis (if is-vertical :y :x)]
                       {:id id :p1 p1 :p2 p2 :axis axis}))
                   points
                   (rest points))))
          routed-edges))

;; --- 端口分配 (Port Assignment) ---

(defn- assign-ports [nodes edges options]
  "端口分配 (Port Assignment)。
   确定每条边在节点上的具体连接点。
   简单的策略：
   根据输入/输出边的数量，将它们均匀分布在节点的对应边上。
   TB布局: 输入在Top，输出在Bottom。
   LR布局: 输入在Left，输出在Right。"
  (let [nodes-map (into {} (map (juxt :id identity) nodes))
        direction (:direction options :tb)
        vertical? (is-vertical? direction)]

    (when-let [n (some #(when (= (:id %) "Gateway") %) nodes)]
      (println "DEBUG: assign-ports Gateway:" (:x n) (:y n) (:w n) (:h n)))

    (reduce
     (fn [acc node]
       (let [node-id (:id node)
             my-inputs (filter #(= (:to %) node-id) edges)
             my-outputs (filter #(= (:from %) node-id) edges)

              ;; Sort edges to minimize crossings
             sort-fn (fn [e is-input?]
                       (let [other-id (if is-input? (:from e) (:to e))
                             other-node (get nodes-map other-id)]
                         (if vertical?
                           (or (:x other-node) 0)
                           (or (:y other-node) 0))))

             sorted-in (sort-by #(sort-fn % true) my-inputs)
             sorted-out (sort-by #(sort-fn % false) my-outputs)

              ;; Distribute ports
              ;; TB: Inputs Top, Outputs Bottom
             in-ports (distribute-points
                       (if vertical? (or (:x node) 0) (or (:y node) 0))
                       (if vertical? (or (:w node) 0) (or (:h node) 0))
                       (if vertical? (or (:y node) 0) (or (:x node) 0))
                       (count sorted-in)
                       (if vertical? :x :y))

             out-ports (distribute-points
                        (if vertical? (or (:x node) 0) (or (:y node) 0))
                        (if vertical? (or (:w node) 0) (or (:h node) 0))
                        (if vertical? (+ (or (:y node) 0) (or (:h node) 0)) (+ (or (:x node) 0) (or (:w node) 0)))
                        (count sorted-out)
                        (if vertical? :x :y))

              ;; Create map: edge-id -> port
             in-map (zipmap (map :id sorted-in) in-ports)
             out-map (zipmap (map :id sorted-out) out-ports)]

         (as-> acc m
           (reduce (fn [m2 [eid port]] (assoc-in m2 [eid :target] port)) m in-map)
           (reduce (fn [m2 [eid port]] (assoc-in m2 [eid :source] port)) m out-map))))
     {}
     nodes)))

;; --- 主路由逻辑 (Main Routing Logic) ---

(defn- check-node-intersection [p1 p2 node padding]
  "检查线段 (p1, p2) 是否与节点 node 及其 padding 区域相交。"
  (let [b (get-node-bounds node padding)
        ;; Segment bounds
        sx1 (min (:x p1) (:x p2))
        sx2 (max (:x p1) (:x p2))
        sy1 (min (:y p1) (:y p2))
        sy2 (max (:y p1) (:y p2))

        ;; Intersection check
        intersects? (and (< sx1 (:x2 b)) (> sx2 (:x1 b))
                         (< sy1 (:y2 b)) (> sy2 (:y1 b)))]
    intersects?))

(defn- find-safe-mid [p1 p2 nodes vertical? padding]
  "寻找安全的中点。
   在生成 Z 形路径时，中间的横向（或纵向）线段可能会穿过其他节点。
   该函数尝试寻找一个 Y (或 X) 坐标，使得中间线段不穿过任何节点。
   尝试的候选位置包括：中点、节点的上下边界。"
  (let [mid-val (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))
        candidates (concat [mid-val]
                           (map (fn [n]
                                  (if vertical?
                                    (+ (or (:y n) 0) (or (:h n) 0) padding) ;; Below node
                                    (+ (or (:x n) 0) (or (:w n) 0) padding))) ;; Right of node
                                nodes)
                           (map (fn [n]
                                  (if vertical?
                                    (- (or (:y n) 0) padding) ;; Above node
                                    (- (or (:x n) 0) padding))) ;; Left of node
                                nodes))

        ;; Filter candidates that are reasonable (between start and end roughly, or slightly outside)
        valid-candidates (take 10 (distinct candidates))] ;; Limit to 10 tries for perf

    (or (first (filter (fn [val]
                         (let [;; Construct 3 segments based on this split value
                               m1 (if vertical? {:x (:x p1) :y val} {:x val :y (:y p1)})
                               m2 (if vertical? {:x (:x p2) :y val} {:x val :y (:y p2)})

                               s1 [p1 m1]
                               s2 [m1 m2]
                               s3 [m2 p2]

                               ;; Check if ANY segment intersects ANY node
                               collision? (some (fn [node]
                                                  (or (check-node-intersection (first s1) (second s1) node padding)
                                                      (check-node-intersection (first s2) (second s2) node padding)
                                                      (check-node-intersection (first s3) (second s3) node padding)))
                                                nodes)]
                           (not collision?)))
                       valid-candidates))
        mid-val))) ;; Fallback to default mid

(defn- check-path-intersection [points nodes padding]
  (some (fn [node]
          (some (fn [i]
                  (let [p1 (nth points i)
                        p2 (nth points (inc i))]
                    (check-node-intersection p1 p2 node padding)))
                (range (dec (count points)))))
        nodes))

(defn- generate-z-path [p1 p2 mid vertical?]
  (if vertical?
    [p1 {:x (:x p1) :y mid} {:x (:x p2) :y mid} p2]
    [p1 {:x mid :y (:y p1)} {:x mid :y (:y p2)} p2]))

(defn- find-safe-z-path [p1 p2 nodes vertical? padding]
  "寻找安全的 Z 形路径。
   Z形路径由三段组成：Start -> Mid1 -> Mid2 -> End。
   尝试多个中间位置，直到找到无碰撞的路径。"
  (let [mid-val (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))
        candidates (concat [mid-val]
                           (map (fn [n]
                                  (if vertical?
                                    (+ (or (:y n) 0) (or (:h n) 0) padding) ;; Below node
                                    (+ (or (:x n) 0) (or (:w n) 0) padding))) ;; Right of node
                                nodes)
                           (map (fn [n]
                                  (if vertical?
                                    (- (or (:y n) 0) padding) ;; Above node
                                    (- (or (:x n) 0) padding))) ;; Left of node
                                nodes))

        valid-candidates (take 15 (distinct candidates))]

    (first (keep (fn [val]
                   (let [path (generate-z-path p1 p2 val vertical?)] \n (if (not (check-path-intersection path nodes padding))
                                                                          path
                                                                          nil)))
                 valid-candidates))))

(defn- generate-detour-path [p1 p2 detour-val vertical? padding]
  ;; Detour path (C-shape / U-shape logic)
  ;; For TB (vertical):
  ;; p1 -> (x1, y1+pad) -> (detour, y1+pad) -> (detour, y2-pad) -> (x2, y2-pad) -> p2
  ;; This assumes p1 is Top/Bottom.
  ;; If we just want a generic detour:
  ;; We move orthogonal to direction first?

  (let [eps padding]
    (if vertical?
      ;; Vertical Layout (TB)
      (let [y1-shift (if (< (:y p1) (:y p2)) (+ (:y p1) eps) (- (:y p1) eps))
            y2-shift (if (< (:y p1) (:y p2)) (- (:y p2) eps) (+ (:y p2) eps))
            ;; But we should verify if y1-shift and y2-shift make sense relative to p1/p2 direction
            ;; Let's just use fixed small offsets from p1/p2
            y1-m (+ (:y p1) (if (< (:y p2) (:y p1)) (- eps) eps))
            y2-m (- (:y p2) (if (< (:y p2) (:y p1)) (- eps) eps))]

        [p1
         {:x (:x p1) :y y1-m}
         {:x detour-val :y y1-m}
         {:x detour-val :y y2-m}
         {:x (:x p2) :y y2-m}
         p2])

      ;; Horizontal Layout (LR)
      (let [x1-m (+ (:x p1) (if (< (:x p2) (:x p1)) (- eps) eps))
            x2-m (- (:x p2) (if (< (:x p2) (:x p1)) (- eps) eps))]
        [p1
         {:x x1-m :y (:y p1)}
         {:x x1-m :y detour-val}
         {:x x2-m :y detour-val}
         {:x x2-m :y (:y p2)}
         p2]))))

(defn- find-safe-detour-path [p1 p2 nodes vertical? padding]
  "寻找安全的绕行路径 (Detour Path)。
   当直接的 Z 形路径受阻时，尝试“绕大圈” (C形或U形路径)。
   尝试从所有节点的左右（或上下）外侧绕行。"
  (let [;; Try detour values on the cross-axis
        ;; For TB, cross-axis is X.
        base-val (if vertical? (:x p1) (:y p1))

        ;; Candidates: Right/Left (or Down/Up) of nodes, plus some fixed offsets
        candidates (concat
                    [(+ base-val 40) (- base-val 40) (+ base-val 80) (- base-val 80)]
                    (map (fn [n]
                           (if vertical?
                             (+ (or (:x n) 0) (or (:w n) 0) padding) ;; Right of node
                             (+ (or (:y n) 0) (or (:h n) 0) padding))) ;; Below node
                         nodes)
                    (map (fn [n]
                           (if vertical?
                             (- (or (:x n) 0) padding) ;; Left of node
                             (- (or (:y n) 0) padding))) ;; Above node
                         nodes))

        valid-candidates (take 20 (distinct candidates))]

    (first (keep (fn [val]
                   (let [path (generate-detour-path p1 p2 val vertical? padding)]
                     (if (not (check-path-intersection path nodes padding))
                       path
                       nil)))
                 valid-candidates))))

(defn- route-segment-smart [p1 p2 direction nodes]
  "智能路由单条线段。
   策略优先级：
   1. 尝试无碰撞的 Z 形路径 (Smart Z-Path)。
   2. 尝试无碰撞的绕行路径 (Detour Path)。
   3. 降级：使用可能碰撞的 Z 形路径 (Fallback)。"
  (let [vertical? (is-vertical? direction)
        padding 15

        ;; 1. Try simple Z-path
        z-path (find-safe-z-path p1 p2 nodes vertical? padding)

        ;; 2. If failed, try Detour path
        final-path (or z-path
                       (find-safe-detour-path p1 p2 nodes vertical? padding)
                       ;; 3. Fallback to direct Z-path (even if collides)
                       (let [mid (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))]
                         (generate-z-path p1 p2 mid vertical?)))]
    final-path))

(defn- apply-nudges [edges nudges segment-axis]
  (let [;; If segment is vertical (:y), we shift along :x.
        ;; If segment is horizontal (:x), we shift along :y.
        shift-axis (if (= segment-axis :y) :x :y)]
    (map (fn [edge]
           (if-let [offset (get nudges (:id edge))]
             (let [points (:points edge)]
               (if (> (count points) 2)
                 (let [start (first points)
                       end (last points)
                       mid-points (subvec (vec points) 1 (dec (count points)))
                       ;; Only shift points that are part of the segment being nudged?
                       ;; Wait, apply-nudges shifts the WHOLE middle section?
                       ;; The previous logic was:
                       ;; shifted-mids (mapv ... mid-points)
                       ;; It shifted ALL middle points.
                       ;; If we have a Z-shape: p1(V) -> m1(H) -> m2(V) -> p2
                       ;; If we nudge vertical segments (V), we shift X.
                       ;; m1 and m2 are endpoints of the vertical segments?
                       ;; No.
                       ;; p1->m1 is vertical? (x constant)
                       ;; m1->m2 is horizontal? (y constant)
                       ;; m2->p2 is vertical? (x constant)

                       ;; If we nudge vertical segments, we want to change X of m1 and m2?
                       ;; Wait.
                       ;; p1->m1: vertical. X is constant.
                       ;; m2->p2: vertical. X is constant.
                       ;; If we shift X of m1...
                       ;; p1 is fixed (port). So p1->m1 becomes diagonal?
                       ;; We can't just shift all middle points.

                       ;; Original logic:
                       ;; (assoc p shift-axis (+ (get p shift-axis) offset))
                       ;; It shifted ALL middle points.
                       ;; In a simple Z-path (3 segments), points are [p1 m1 m2 p2].
                       ;; Mid points are [m1 m2].
                       ;; If we shift X of m1 and m2:
                       ;; p1->m1 becomes diagonal (if p1.x != new m1.x)
                       ;; m1->m2 stays horizontal (both shifted same amount)
                       ;; m2->p2 becomes diagonal.

                       ;; This seems to imply that "nudging" assumes we can move the whole middle section.
                       ;; But ports are fixed.
                       ;; So this effectively creates diagonals at the ends.
                       ;; Is this intended?
                       ;; "avoid overlapping segments"
                       ;; If we have Orthogonal Routing, we want to keep it orthogonal.
                       ;; If we shift X, we must insert new points or move p1/p2?
                       ;; We cannot move p1/p2 (ports).
                       ;; So we must add "doglegs"?

                       ;; The current implementation seems simplistic: it shifts middle points.
                       ;; This DOES break orthogonality if p1/p2 are not shifted.
                       ;; But maybe that's acceptable for "smart nudging" or it relies on simplification later?
                       ;; No, simplification is done before.

                       ;; Let's stick to the existing logic's intent but apply it correctly.
                       ;; The existing logic shifted all middle points.
                       ;; Let's keep that behavior for now but make it axis-aware.

                       shifted-mids (mapv (fn [p]
                                            (assoc p shift-axis (+ (get p shift-axis) offset)))
                                          mid-points)]
                   (assoc edge :points (into [start] (conj shifted-mids end))))
                 edge))
             edge))
         edges)))

(defn route-edges [layout options]
  "正交路由主入口 (Orthogonal Routing Entry)。
   步骤：
   1. 端口分配 (Port Assignment): 确定起止点。
   2. 初始路由 (Initial Routing): 使用障碍物避让算法为每条边生成路径。
   3. 智能微调 (Nudging): 检测重叠的线段，并将它们分开，避免连线粘连。"
  (let [nodes (:nodes layout)
        edges (:edges layout)
        config (config/resolve-config options)
        direction (:direction options :tb)
        _ (println "DEBUG: route-edges direction=" direction " type=" (type direction) " is-vertical=" (is-vertical? direction))

        ;; 1. Assign ports
        port-assignments (assign-ports nodes edges options)

        ;; 2. Initial routing with Obstacle Avoidance
        routed-edges
        (map (fn [edge]
               (let [source-port (get-in port-assignments [(:id edge) :source])
                     target-port (get-in port-assignments [(:id edge) :target])]
                 (if (and source-port target-port)
                   (let [;; Exclude source and target nodes from obstacle checks to avoid self-collision at ports
                         relevant-nodes (filter #(and (not= (:id %) (:from edge))
                                                      (not= (:id %) (:to edge)))
                                                nodes)
                         points (route-segment-smart source-port target-port direction relevant-nodes)
                         simplified (geo/simplify-points points)]
                     (assoc edge :points simplified))
                   edge))) ;; Keep original if ports not found
             edges)

        ;; 3. Nudging (Optimization)
        ;; Apply nudging for both Vertical (:y) and Horizontal (:x) segments

        ;; First: Nudge Vertical segments (shift X)
        all-segments-v (extract-segments routed-edges)
        nudges-v (nudge-segments-smart all-segments-v :y 10)
        edges-v (apply-nudges routed-edges nudges-v :y)

        ;; Second: Nudge Horizontal segments (shift Y)
        all-segments-h (extract-segments edges-v)
        nudges-h (nudge-segments-smart all-segments-h :x 10)
        final-edges (apply-nudges edges-v nudges-h :x)]

    (assoc layout :edges final-edges)))
