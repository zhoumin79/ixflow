(ns xflow.layout.sugiyama
  (:require [clojure.set :as set]
            [xflow.layout.layering.network-simplex :as network-simplex]
            [xflow.layout.coordinate :as coordinate]
            [xflow.geometry :as geo]))

;; --- Helpers ---

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

(defn- build-adj-matrix [nodes edges]
  "构建邻接矩阵 (Adjacency Matrix)。
   返回一个包含父节点映射和子节点映射的 Map。
   :parents {child-id [parent-id ...]}
   :children {parent-id [child-id ...]}"
  (let [node-ids (set (map :id nodes))
        valid-edges (filter (fn [e] (and (node-ids (:from e)) (node-ids (:to e)))) edges)]
    {:parents (reduce (fn [m e] (update m (:to e) (fnil conj []) (:from e))) {} valid-edges)
     :children (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} valid-edges)}))

(defn- barycenter [node neighbors order-map]
  "计算重心 (Barycenter)。
   根据邻居节点在上一层（或下一层）的顺序 (order) 计算当前节点的理想位置。
   如果节点没有邻居，则保持当前顺序不变。"
  (if (empty? neighbors)
    (get order-map (:id node) 0) ;; Keep current position if no neighbors
    (/ (reduce + (map #(get order-map % 0) neighbors))
       (count neighbors))))

(defn- sort-layer [layer neighbors-map order-map]
  "根据重心理论对层内节点进行排序。
   这是减少交叉的核心步骤：节点的顺序应该尽可能接近其邻居的平均位置。"
  (let [nodes-with-val
        (map (fn [node]
               (let [neighbors (get neighbors-map (:id node) [])
                     val (barycenter node neighbors order-map)]
                 {:node node :val val}))
             layer)]
    (map :node (sort-by :val nodes-with-val))))

;; --- Phase 1: Layering (分层) ---

(defn- remove-cycles [nodes edges]
  "去环 (Cycle Removal)。
   Sugiyama 算法要求输入图是有向无环图 (DAG)。
   该函数使用 DFS 遍历检测环，并将回边 (Back Edges) 反转，使图变为 DAG。
   算法步骤：
   1. 计算入度并排序，确保确定性的遍历顺序。
   2. 使用递归 DFS 维护 visited 和 on-stack 状态。
   3. 如果遇到 on-stack 的节点，说明发现环，将边标记为 reversed? 并反转方向。
   4. 否则正常处理。"
  (let [adj (reduce (fn [m e] (update m (:from e) (fnil conj []) e)) {} edges)

        ;; Sort nodes to ensure deterministic start order
        in-degrees (reduce (fn [acc e] (update acc (:to e) (fnil inc 0)))
                           {} edges)
        sorted-nodes (sort-by (fn [n] (get in-degrees (:id n) 0)) nodes)

        ;; Recursive DFS function that carries state
        ;; state: {:visited #{} :on-stack #{} :edges []}
        visit-fn (fn visit [state u-id]
                   (let [state-1 (-> state
                                     (update :visited conj u-id)
                                     (update :on-stack conj u-id))

                         ;; Process neighbors
                         neighbors (get adj u-id)

                         final-state (reduce
                                      (fn [curr-state e]
                                        (let [v-id (:to e)]
                                          (cond
                                            ;; Back edge: v is on recursion stack
                                            (contains? (:on-stack curr-state) v-id)
                                            (update curr-state :edges conj (assoc e :from v-id :to u-id :reversed? true))

                                            ;; Not visited: recurse
                                            (not (contains? (:visited curr-state) v-id))
                                            (let [after-visit (visit curr-state v-id)]
                                              ;; Add the edge (it's a tree edge)
                                              (update after-visit :edges conj e))

                                            ;; Already visited (Forward/Cross edge)
                                            :else
                                            (update curr-state :edges conj e))))
                                      state-1
                                      neighbors)]

                     ;; Backtrack: remove u from stack
                     (update final-state :on-stack disj u-id)))

        ;; Iterate over all nodes to ensure disconnected components are visited
        final-state (reduce (fn [state n]
                              (if (contains? (:visited state) (:id n))
                                state
                                (visit-fn state (:id n))))
                            {:visited #{} :on-stack #{} :edges []}
                            sorted-nodes)]

    (:edges final-state)))

(defn assign-ranks [nodes edges]
  "分配层级 (Rank Assignment)。
   使用网络单纯形算法 (Network Simplex) 为每个节点分配 :rank 值。
   这是分层算法中最优的一种，能保证总边长最短。"
  (let [acyclic-edges (remove-cycles nodes edges)]
    (network-simplex/assign-ranks nodes acyclic-edges)))

;; --- Phase 2: Crossing Minimization (交叉最小化) ---

(defn- crossing-minimization-sweep [ranks max-rank parents children iterations]
  "交叉最小化扫描 (Crossing Minimization Sweep)。
   这是一个启发式算法，通过多轮上下扫描来调整每层节点的顺序，以减少边的交叉。
   
   Down Sweep (从上往下):
     根据上一层 (parents) 的顺序，调整当前层节点的顺序 (重心法)。
   
   Up Sweep (从下往上):
     根据下一层 (children) 的顺序，调整当前层节点的顺序。
   
   iterations: 迭代次数，通常 8-12 次即可达到较好效果。"
  (loop [iter 0
         current-ranks ranks
         ;; Build initial order-map: {node-id -> index}
         order-map (into {} (mapcat (fn [[r layer]]
                                      (map-indexed (fn [i n] [(:id n) i]) layer))
                                    ranks))]
    (if (>= iter iterations)
      ;; Finish: Assign final order attribute
      (flatten
       (map (fn [[r layer]]
              (map-indexed (fn [i n] (assoc n :order i)) layer))
            current-ranks))

      ;; Sweep
      (let [;; Down sweep: Rank 1 -> Max
            ;; Adjust current layer based on previous layer (parents)
            [down-ranks down-order-map]
            (reduce
             (fn [[rs om] r]
               (let [layer (get rs r)
                     sorted-layer (sort-layer layer parents om)
                     new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                 [(assoc rs r sorted-layer) new-om]))
             [current-ranks order-map]
             (range 1 (inc max-rank)))

            ;; Up sweep: Rank Max-1 -> 0
            ;; Adjust current layer based on next layer (children)
            [up-ranks up-order-map]
            (reduce
             (fn [[rs om] r]
               (let [layer (get rs r)
                     sorted-layer (sort-layer layer children om)
                     new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                 [(assoc rs r sorted-layer) new-om]))
             [down-ranks down-order-map]
             (range (dec max-rank) -1 -1))]

        (recur (inc iter) up-ranks up-order-map)))))

(defn order-nodes [nodes edges]
  "节点排序 (Node Ordering)。
   在确定了每个节点的层级 (:rank) 后，确定其在层内的顺序 (:order)，
   目标是最小化边交叉数。"
  (let [ranked-nodes (sort-by :rank nodes)
        ranks (group-by :rank ranked-nodes)
        max-rank (apply max (keys ranks))
        {:keys [parents children]} (build-adj-matrix nodes edges)
        iterations 8]
    (crossing-minimization-sweep ranks max-rank parents children iterations)))

;; --- Phase 3: Dummy Nodes (Coordinate Assignment Prep) ---

(defn insert-dummy-nodes [nodes edges]
  "插入虚拟节点 (Dummy Nodes)。
   如果一条边跨越了多层 (rank差 > 1)，需要在中间的每一层插入一个虚拟节点。
   这样做的目的是将长边分解为一系列单位长度的短边，简化后续的坐标计算和路由。
   虚拟节点的大小通常为 0。"
  (let [nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Helper to calculate rank span
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

(defn apply-edge-points [original-edges nodes-with-coords]
  "将计算出的坐标应用到原始边上。
   路径由起点中心、所有虚拟节点位置、终点中心组成。"
  (let [node-map (into {} (map (juxt :id identity) nodes-with-coords))
        edge-dummies (group-by #(:id (:parent-edge %)) (filter :dummy? nodes-with-coords))]

    (map (fn [edge]
           (let [u (get node-map (or (:from edge) (:source edge)))
                 v (get node-map (or (:to edge) (:target edge)))
                 start-pt (geo/node-center u)
                 end-pt (geo/node-center v)

                 dummies (get edge-dummies (:id edge) [])
                 sorted-dummies (sort-by :rank dummies)
                 dummy-pts (mapv (fn [d] {:x (:x d) :y (:y d)}) sorted-dummies)

                 ;; Combine: Start -> Dummies -> End
                 full-path (vec (concat [start-pt] dummy-pts [end-pt]))]

             (if (and u v)
               (assoc edge :points full-path)
               edge)))
         original-edges)))

(defn straighten-dummy-nodes [edges nodes options]
  "拉直虚拟节点链 (Straighten Dummy Chains)。
   由于重心法可能会导致长边的虚拟节点呈锯齿状排列，这个步骤尝试将它们拉直，
   使长边尽可能垂直（或水平），提升美观度。"
  (let [node-map (into {} (map (juxt :id identity) nodes))
        layout-dir (:direction options "lr")
        horizontal? (or (= layout-dir "lr") (= layout-dir "rl"))

        get-coord (fn [n]
                    (let [center (geo/node-center n)]
                      (if horizontal? (:y center) (:x center))))

        ;; Helper to find the chain of dummy nodes for an edge
        find-dummy-chain (fn [edge]
                           (let [start-node (get node-map (:from edge))
                                 end-node (get node-map (:to edge))
                                 ;; Find dummy nodes that belong to this edge (dummy nodes store parent-edge)
                                 chain (filter #(and (:dummy? %) (= (:id (:parent-edge %)) (:id edge))) nodes)
                                 sorted-chain (sort-by :rank chain)]
                             {:start start-node
                              :end end-node
                              :dummies sorted-chain}))

        ;; Function to adjust a single chain
        adjust-chain (fn [nodes-map {:keys [start end dummies]}]
                       (if (empty? dummies)
                         nodes-map
                         (let [start-center (get-coord start)
                               end-center (get-coord end)

                               ;; If start and end are close (e.g. same swimlane), align perfectly to start
                               target-pos (if (< (Math/abs (- start-center end-center)) 50)
                                            start-center
                                            ;; If different, stick to start's level for now to prefer L-shape over Z-shape
                                            start-center)

                               ;; Update dummy nodes in the map
                               updated-dummies (map (fn [d]
                                                      ;; Dummy nodes have 0 size, so their x/y is their center
                                                      (if horizontal?
                                                        (assoc d :y target-pos)
                                                        (assoc d :x target-pos)))
                                                    dummies)]
                           (reduce (fn [m n] (assoc m (:id n) n)) nodes-map updated-dummies))))]

    (vals (reduce adjust-chain
                  (into {} (map (juxt :id identity) nodes))
                  (map find-dummy-chain edges)))))

(defn layout-graph [nodes original-edges options]
  "Sugiyama 布局算法主流程 (Main Pipeline)。
   这是分层图布局的标准实现，适用于流程图、依赖图等。
   
   主要步骤：
   1. 分层 (Layering): 确定每个节点的 Y 坐标 (Rank)。
   2. 插入虚拟节点 (Dummy Insertion): 处理跨层长边。
   3. 排序 (Ordering): 确定每层节点的顺序，减少交叉。
   4. 坐标分配 (Coordinate Assignment): 确定 X 坐标，平衡节点间距和边的直线性。
   5. 路由 (Routing): 根据虚拟节点位置生成边的路径点。"
  (let [direction (get options :direction :TB)
        is-horizontal? (or (= direction :LR) (= direction :RL))

        nodes-for-layout (if is-horizontal? (geo/swap-xy nodes) nodes)

        ;; 1. Layering
        ranked-nodes (assign-ranks nodes-for-layout original-edges)

        ;; 2. Dummy Nodes (splits edges into segments)
        ;; We rename the result to avoid shadowing 'original-edges'
        graph-with-dummies (insert-dummy-nodes ranked-nodes original-edges)
        nodes-with-dummies (:nodes graph-with-dummies)
        edge-segments (:edges graph-with-dummies)

        ;; 3. Ordering (uses segments to minimize crossings between layers)
        ordered-nodes (order-nodes nodes-with-dummies edge-segments)

        ;; 4. Coordinates
        coord-result (coordinate/assign-coordinates ordered-nodes edge-segments options)
        nodes-with-coords (:nodes coord-result)

        ;; Optional: Straighten dummy nodes to improve orthogonal look
        nodes-straightened (straighten-dummy-nodes original-edges nodes-with-coords options)

        ;; 5. Routing (Reconstruct paths for ORIGINAL edges using dummy node positions)
        edges-with-points (apply-edge-points original-edges nodes-straightened)

        final-nodes (remove :dummy? nodes-straightened)

        layout-w (:width coord-result)
        layout-h (:height coord-result)]

    (if is-horizontal?
      {:nodes (geo/swap-xy final-nodes)
       :edges (geo/swap-edges-xy edges-with-points)
       :width layout-h
       :height layout-w}
      {:nodes final-nodes
       :edges edges-with-points
       :width layout-w
       :height layout-h})))
