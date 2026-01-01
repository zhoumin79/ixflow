(ns xflow.layout.sugiyama
  (:require [clojure.set :as set]
            [xflow.layout.layering.network-simplex :as network-simplex]
            [xflow.layout.coordinate :as coordinate]))

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

;; --- 分层 (Layering) ---
;; 使用 Network Simplex 算法进行分层，这是目前最先进的分层算法之一。
;; 它的目标是减少边的长度总和，从而使图形更加紧凑。
(defn assign-ranks [nodes edges]
  (network-simplex/assign-ranks nodes edges))

;; --- 交叉最小化 (Crossing Minimization) ---
;; 使用重心法 (Barycenter Method) 结合扫描线 (Sweep-Line) 算法。
;; 通过迭代调整每一层的节点顺序，尽量减少边与边的交叉。

(defn- build-adj-matrix [nodes edges]
  (let [node-ids (set (map :id nodes))
        valid-edges (filter (fn [e] (and (node-ids (:from e)) (node-ids (:to e)))) edges)]
    {:parents (reduce (fn [m e] (update m (:to e) (fnil conj []) (:from e))) {} valid-edges)
     :children (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} valid-edges)}))

(defn- barycenter [node neighbors order-map]
  (if (empty? neighbors)
    (get order-map (:id node) 0) ;; Keep current position if no neighbors
    (/ (reduce + (map #(get order-map % 0) neighbors))
       (count neighbors))))

(defn- sort-layer [layer neighbors-map order-map]
  (let [nodes-with-val
        (map (fn [node]
               (let [neighbors (get neighbors-map (:id node) [])
                     val (barycenter node neighbors order-map)]
                 {:node node :val val}))
             layer)]
    (map :node (sort-by :val nodes-with-val))))

(defn order-nodes [nodes edges]
  (let [ranked-nodes (sort-by :rank nodes)
        ranks (group-by :rank ranked-nodes)
        max-rank (apply max (keys ranks))
        {:keys [parents children]} (build-adj-matrix nodes edges)
        iterations 8] ;; Standard number of iterations

    ;; 初始随机顺序 (Initial random order)
    (loop [iter 0
           current-ranks ranks
           ;; Build initial order-map: {node-id -> index}
           order-map (into {} (mapcat (fn [[r layer]]
                                        (map-indexed (fn [i n] [(:id n) i]) layer))
                                      ranks))]
      (if (>= iter iterations)
        ;; 完成: 分配最终的 order 属性
        (flatten
         (map (fn [[r layer]]
                (map-indexed (fn [i n] (assoc n :order i)) layer))
              current-ranks))

        ;; 扫描 (Sweep)
        (let [;; 下行扫描 (Down sweep): Rank 1 -> Max
              ;; 根据上一层(parents)的重心调整当前层
              [down-ranks down-order-map]
              (reduce
               (fn [[rs om] r]
                 (let [layer (get rs r)
                       ;; Sort based on parents (rank r-1)
                       sorted-layer (sort-layer layer parents om)
                       ;; Update order map for this layer
                       new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                   [(assoc rs r sorted-layer) new-om]))
               [current-ranks order-map]
               (range 1 (inc max-rank)))

              ;; 上行扫描 (Up sweep): Rank Max-1 -> 0
              ;; 根据下一层(children)的重心调整当前层
              [up-ranks up-order-map]
              (reduce
               (fn [[rs om] r]
                 (let [layer (get rs r)
                       ;; Sort based on children (rank r+1)
                       sorted-layer (sort-layer layer children om)
                       new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                   [(assoc rs r sorted-layer) new-om]))
               [down-ranks down-order-map]
               (range (dec max-rank) -1 -1))]

          (recur (inc iter) up-ranks up-order-map))))))

;; --- 坐标分配 (Coordinate Assignment) ---
;; 处理长边 (Long Edges) 的虚拟节点插入。
;; 真正的坐标分配逻辑委托给 coordinate 命名空间，使用 Brandes-Köpf 或类似的高级对齐算法。

(defn insert-dummy-nodes [nodes edges]
  (let [nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Helper to check span
        span (fn [e]
               (let [u (get nodes-map (:from e))
                     v (get nodes-map (:to e))]
                 (if (and u v) (- (:rank v) (:rank u)) 0)))

        long-edges (filter #(> (span %) 1) edges)
        other-edges (filter #(<= (span %) 1) edges)]

    (loop [to-process long-edges
           final-nodes nodes
           final-edges other-edges
           dummy-info {}]
      (if (empty? to-process)
        {:nodes final-nodes
         :edges final-edges
         :dummies dummy-info}
        (let [edge (first to-process)
              u (get nodes-map (:from edge))
              v (get nodes-map (:to edge))
              u-rank (:rank u)
              v-rank (:rank v)
              ;; Create dummy nodes for each intermediate rank
              dummies (map (fn [i]
                             {:id (str "dummy_" (:id edge) "_" i)
                              :rank (+ u-rank i 1)
                              :dummy? true
                              :w 0 :h 0 ;; Dummy nodes have 0 size
                              :parent-edge edge})
                           (range (dec (- v-rank u-rank))))

              ;; Create segments connecting u -> d1 -> d2 -> ... -> v
              segments (loop [ds dummies
                              prev (:id u)
                              segs []]
                         (if (empty? ds)
                           (conj segs {:from prev :to (:id v)})
                           (let [curr (first ds)]
                             (recur (rest ds)
                                    (:id curr)
                                    (conj segs {:from prev :to (:id curr)})))))]

          (recur (rest to-process)
                 (concat final-nodes dummies)
                 (concat final-edges segments)
                 (reduce (fn [acc d] (assoc acc (:id d) (:parent-edge d)))
                         dummy-info
                         dummies)))))))

(defn remove-dummy-nodes [nodes edges dummy-nodes-map]
  (let [;; Filter out dummy nodes
        real-nodes (remove :dummy? nodes)

        ;; Map of dummy-id -> coordinates
        dummy-coords (into {} (map (fn [n] [(:id n) {:x (:x n) :y (:y n)}]) (filter :dummy? nodes)))]
    {:nodes real-nodes
     :dummy-coords dummy-coords}))

;; Helper to apply waypoints to original edges
(defn apply-edge-points [original-edges dummy-nodes-with-coords]
  (let [edge-dummies (group-by #(:id (:parent-edge %)) (filter :dummy? dummy-nodes-with-coords))]
    (map (fn [edge]
           (if-let [dummies (get edge-dummies (:id edge))]
             ;; Sort dummies by rank to ensure correct order
             (let [sorted-dummies (sort-by :rank dummies)
                   points (mapv (fn [d] {:x (:x d) :y (:y d)}) sorted-dummies)]
               (assoc edge :points points))
             edge))
         original-edges)))

(defn- swap-node-dims [nodes]
  (map (fn [n] (assoc n :w (:h n) :h (:w n))) nodes))

(defn- swap-coords [nodes]
  (map (fn [n] (assoc n :x (:y n) :y (:x n))) nodes))

(defn- swap-edge-points [edges]
  (map (fn [e]
         (if (:points e)
           (update e :points (fn [pts] (mapv (fn [p] {:x (:y p) :y (:x p)}) pts)))
           e))
       edges))

(defn layout-graph [nodes edges options]
  (let [direction (get options :direction :TB) ;; 默认为自上而下 (Top-Bottom)
        is-horizontal? (or (= direction :LR) (= direction :RL)) ;; 目前支持 LR, 将来可支持 RL

        ;; 1. 预处理: 如果是水平布局，交换节点的宽高
        ;; Pre-processing: Swap width/height for horizontal layout
        nodes-for-layout (if is-horizontal?
                           (swap-node-dims nodes)
                           nodes)

        ;; 2. 执行标准 Sugiyama 布局 (默认 Top-Bottom)
        ;; Execute standard Sugiyama layout
        ;; 2.1 分层 (Layering)
        ranked-nodes (assign-ranks nodes-for-layout edges)

        ;; 2.2 插入虚拟节点 (Insert Dummy Nodes)
        {:keys [nodes dummies] :as graph-with-dummies} (insert-dummy-nodes ranked-nodes edges)

        ;; 2.3 节点排序 (Node Ordering)
        ordered-nodes (order-nodes (:nodes graph-with-dummies) (:edges graph-with-dummies))

        ;; 2.4 坐标分配 (Coordinate Assignment)
        coord-result (coordinate/assign-coordinates ordered-nodes (:edges graph-with-dummies) options)
        nodes-with-coords (:nodes coord-result)

        ;; 2.5 边路由点计算 (Edge Routing Points)
        edges-with-points (apply-edge-points edges nodes-with-coords)

        ;; 2.6 清理虚拟节点
        final-nodes (remove :dummy? nodes-with-coords)

        ;; 获取布局尺寸
        layout-w (:width coord-result)
        layout-h (:height coord-result)]

    ;; 3. 后处理: 如果是水平布局，交换坐标 X/Y 和 布局宽/高
    ;; Post-processing: Swap X/Y and Width/Height for horizontal layout
    (if is-horizontal?
      {:nodes (swap-coords final-nodes)
       :edges (swap-edge-points edges-with-points)
       :width layout-h
       :height layout-w}
      {:nodes final-nodes
       :edges edges-with-points
       :width layout-w
       :height layout-h})))
