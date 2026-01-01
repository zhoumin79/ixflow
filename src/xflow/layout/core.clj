(ns xflow.layout.core
  (:require [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.strategy.swimlane :as swimlane]
            [xflow.layout.strategy.swimlane-ordering :as swimlane-ordering]
            [xflow.layout.strategy.cluster :as cluster]
            [xflow.layout.strategy.compound :as compound]
            [xflow.layout.strategy.simple :as simple]
            [xflow.layout.routing.manhattan :as manhattan]
            [xflow.layout.routing.ortho :as ortho]
            [xflow.layout.routing.spline :as spline]
            [xflow.layout.routing.architecture :as architecture]
            [xflow.theme.rule :as rule]))

(defn- parse-number [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble v)
                  (catch Exception _ nil))
    :else nil))

(defn- coerce-node-size [node]
  (let [props (:props node)
        w (or (parse-number (:w node))
              (parse-number (:width props))
              (parse-number (:w props))
              120) ;; Default width
        h (or (parse-number (:h node))
              (parse-number (:height props))
              (parse-number (:h props))
              60)] ;; Default height
    (merge node {:w w :h h})))

(defn- straighten-dummy-nodes [edges nodes options]
  (let [dummy-nodes (filter #(:dummy %) nodes)
        real-nodes (remove #(:dummy %) nodes)
        node-map (into {} (map (juxt :id identity) nodes))
        layout-dir (:direction options "lr")
        horizontal? (or (= layout-dir "lr") (= layout-dir "rl"))

        ;; Helper to find the chain of dummy nodes for an edge
        find-dummy-chain (fn [edge]
                           (let [start-node (get node-map (:from edge))
                                 end-node (get node-map (:to edge))
                                 ;; Find dummy nodes that belong to this edge
                                 chain (filter #(and (:dummy %) (= (:edge-id %) (:id edge))) nodes)
                                 sorted-chain (sort-by :rank chain)]
                             {:start start-node
                              :end end-node
                              :dummies sorted-chain}))

        ;; Function to adjust a single chain
        adjust-chain (fn [nodes-map {:keys [start end dummies]}]
                       (if (empty? dummies)
                         nodes-map
                         (let [start-pos (if horizontal? (:y start) (:x start))
                               end-pos (if horizontal? (:y end) (:x end))

                               ;; If start and end are close (e.g. same swimlane), align perfectly to start
                               target-pos (if (< (Math/abs (- start-pos end-pos)) 50)
                                            start-pos
                                            ;; If different, stick to start's level for now to prefer L-shape over Z-shape
                                            start-pos)

                               ;; Update dummy nodes in the map
                               updated-dummies (map (fn [d]
                                                      (if horizontal?
                                                        (assoc d :y target-pos)
                                                        (assoc d :x target-pos)))
                                                    dummies)]
                           (reduce (fn [m n] (assoc m (:id n) n)) nodes-map updated-dummies))))]

    (vals (reduce adjust-chain
                  (into {} (map (juxt :id identity) nodes))
                  (map find-dummy-chain edges)))))

(defn- assign-coordinates [nodes edges pools options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)

        ;; --- Phase 3: Insert Dummy Nodes for Long Edges ---
        ;; This splits long edges into segments with dummy nodes
        {:keys [nodes dummies] :as graph-with-dummies}
        (sugiyama/insert-dummy-nodes ranked-nodes edges)

        ;; Assign swimlane-id to dummy nodes (inherit from source)
        ;; This ensures they are placed in the correct lane (or source lane)
        nodes-map (into {} (map (fn [n] [(:id n) n]) ranked-nodes)) ;; Use original nodes for lookup

        nodes-with-dummies
        (map (fn [n]
               (if (:dummy? n)
                 (let [parent-edge (:parent-edge n)
                       src-id (:from parent-edge)
                       src-node (get nodes-map src-id)]
                   (assoc n :swimlane-id (:swimlane-id src-node)))
                 n))
             (:nodes graph-with-dummies))

        edges-with-dummies (:edges graph-with-dummies) ;; Segments
        ;; --------------------------------------------------

        ;; --- Optimization: Apply Crossing Minimization ---
        ordered-nodes
        (if (or (= (:layout options) "swimlane") (= (:layout options) "cluster"))
          (swimlane-ordering/order-nodes nodes-with-dummies edges-with-dummies pools)
          (sugiyama/order-nodes nodes-with-dummies edges-with-dummies))

        ;; Sort by rank then order to ensure correct processing order
        sorted-nodes (sort-by (juxt :rank :order) ordered-nodes)
        ;; -------------------------------------------------

        layout-mode (:layout options)
        mode (if (= layout-mode "cluster") "vertical" (:swimlane-mode options "horizontal"))
        direction (:direction options "lr")

        ;; Group by Swimlane
        lanes-data (swimlane/group-by-swimlane sorted-nodes pools options)
        lanes (swimlane/calculate-lane-dimensions lanes-data mode)

        ;; Calculate Node Positions (Initial Pass)
        initial-nodes (swimlane/assign-initial-coordinates sorted-nodes lanes options)

        ;; -- Cluster Layout Post-Processing --
        processed-nodes
        (if (= layout-mode "cluster")
          (cluster/process-layout initial-nodes)
          initial-nodes)

        ;; Recalculate Dimensions based on processed nodes
        width (if (= mode "horizontal")
                (apply max (map #(+ (:x %) (:w %) 50) processed-nodes))
                (reduce + (map :size lanes)))

        height (if (= mode "horizontal")
                 (reduce + (map :size lanes))
                 (apply max (map #(+ (:y %) (:h %) 50) processed-nodes)))

        ;; Construct Swimlane Geometry
        swimlane-geoms
        (if (= layout-mode "cluster")
          (cluster/calculate-cluster-geometries lanes processed-nodes)
          (swimlane/calculate-strip-geometries lanes width height mode))

      ;; --- Phase 3: Extract Dummy Coordinates ---
      ;; Optimization: Straighten dummy nodes to avoid zigzag lines
        straightened-nodes (straighten-dummy-nodes edges processed-nodes options)

      ;; Apply coordinates from dummy nodes to original edges as waypoints
        edges-with-points (sugiyama/apply-edge-points edges straightened-nodes)

      ;; Remove dummy nodes
        final-nodes (remove :dummy? processed-nodes)]

    {:nodes final-nodes
     :edges edges-with-points
     :swimlanes swimlane-geoms
     :width width
     :height height}))

(defn- calculate-label-pos [points]
  (let [cnt (count points)]
    (if (= cnt 2)
      ;; Midpoint of segment
      (let [p0 (first points)
            p1 (second points)
            ;; Initial guess at 0.4
            t 0.4
            pos-x (+ (:x p0) (* (- (:x p1) (:x p0)) t))
            pos-y (+ (:y p0) (* (- (:y p1) (:y p0)) t))

            ;; Heuristic to avoid overlap with target (p1)
            dx (- (:x p1) (:x p0))
            dy (- (:y p1) (:y p0))
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))]

        ;; If distance is small, ensure we are far enough from p1
        (if (> (Math/abs dy) (Math/abs dx))
          ;; Vertical layout: check Y distance
          (let [safe-y (- (:y p1) 35)] {:x pos-x :y (min pos-y safe-y)})
          ;; Horizontal layout: check X distance (assuming L->R)
          (if (> dx 0)
            (let [safe-x (- (:x p1) 40)] {:x (min pos-x safe-x) :y pos-y})
            {:x pos-x :y pos-y}))) ;; Backwards/other cases, leave as is

      ;; Multi-point path (Spline or Manhattan): Find longest segment
      (let [segments (map vector points (rest points))
            length-sq (fn [[p1 p2]]
                        (+ (Math/pow (- (:x p1) (:x p2)) 2)
                           (Math/pow (- (:y p1) (:y p2)) 2)))
            longest-segment (apply max-key length-sq segments)
            [p1 p2] longest-segment]
        {:x (/ (+ (:x p1) (:x p2)) 2)
         :y (/ (+ (:y p1) (:y p2)) 2)}))))

(defn normalize-layout [{:keys [nodes edges swimlanes width height] :as layout-result}]
  (let [;; Pre-calculate edge label positions
        edges-with-labels (mapv (fn [e]
                                  (if (and (:label e) (:points e) (seq (:points e)))
                                    (assoc e :label-pos (calculate-label-pos (:points e)))
                                    e))
                                edges)

        ;; 辅助函数：获取实体的有效坐标点
        get-points (fn [item]
                     (when (and (number? (:x item)) (number? (:y item)))
                       (let [w (or (:w item) 0)
                             h (or (:h item) 0)]
                         [{:x (:x item) :y (:y item)}
                          {:x (+ (:x item) w) :y (+ (:y item) h)}])))

        ;; 辅助函数：获取边的所有点
        get-edge-points (fn [edge]
                          (filter (fn [p] (and (number? (:x p)) (number? (:y p))))
                                  (:points edge)))

        ;; 辅助函数：获取标签的边界点 (估算)
        get-label-points (fn [item]
                           (when (or (:label item) (:id item))
                             (let [pos (or (:label-pos item)
                                           (when (and (:x item) (:y item))
                                             {:x (+ (:x item) (/ (or (:w item) 0) 2))
                                              :y (+ (:y item) (/ (or (:h item) 0) 2))}))]
                               (when pos
                                 (let [text (str (or (:label item) (:id item)))
                                       len (count text)
                                       approx-w (* len 8) ;; 估算每个字符 8px 宽
                                       approx-h 20 ;; 估算高度 20px
                                       cx (:x pos)
                                       cy (:y pos)]
                                   (when (and (number? cx) (number? cy))
                                     [{:x (- cx (/ approx-w 2)) :y (- cy (/ approx-h 2))}
                                      {:x (+ cx (/ approx-w 2)) :y (+ cy (/ approx-h 2))}]))))))

        ;; 收集所有点以计算边界
        node-points (mapcat get-points nodes)
        node-label-points (mapcat get-label-points nodes)
        edge-points (mapcat get-edge-points edges-with-labels)
        edge-label-points (mapcat get-label-points edges-with-labels)
        swimlane-points (mapcat get-points swimlanes)

        all-points (concat node-points node-label-points edge-points edge-label-points swimlane-points)

        min-x (if (seq all-points) (apply min (map :x all-points)) 0)
        min-y (if (seq all-points) (apply min (map :y all-points)) 0)

        padding 50
        ;; 计算偏移量，确保最小坐标位于 padding 处
        shift-x (- padding min-x)
        shift-y (- padding min-y)

        ;; 始终重新计算节点和边，确保画布尺寸正确包含右侧/底部的 padding
        nodes-shifted (if (and (zero? shift-x) (zero? shift-y))
                        nodes
                        (mapv (fn [n]
                                (if (and (number? (:x n)) (number? (:y n)))
                                  (assoc n :x (+ (:x n) shift-x) :y (+ (:y n) shift-y))
                                  n))
                              nodes))

        edges-shifted (if (and (zero? shift-x) (zero? shift-y))
                        edges-with-labels
                        (mapv (fn [e]
                                (let [e (if (:points e)
                                          (update e :points (fn [pts]
                                                              (mapv (fn [p]
                                                                      (if (and (number? (:x p)) (number? (:y p)))
                                                                        (assoc p :x (+ (:x p) shift-x) :y (+ (:y p) shift-y))
                                                                        p))
                                                                    pts)))
                                          e)
                                      e (if (:p1 e)
                                          (update e :p1 (fn [p] (if (and (number? (:x p)) (number? (:y p)))
                                                                  (assoc p :x (+ (:x p) shift-x) :y (+ (:y p) shift-y))
                                                                  p)))
                                          e)
                                      e (if (:p2 e)
                                          (update e :p2 (fn [p] (if (and (number? (:x p)) (number? (:y p)))
                                                                  (assoc p :x (+ (:x p) shift-x) :y (+ (:y p) shift-y))
                                                                  p)))
                                          e)]
                                  (if (:label-pos e)
                                    (update e :label-pos (fn [p]
                                                           (assoc p :x (+ (:x p) shift-x) :y (+ (:y p) shift-y))))
                                    e)))
                              edges-with-labels))

        swimlanes-shifted (if (and (zero? shift-x) (zero? shift-y))
                            swimlanes
                            (mapv (fn [n]
                                    (if (and (number? (:x n)) (number? (:y n)))
                                      (assoc n :x (+ (:x n) shift-x) :y (+ (:y n) shift-y))
                                      n))
                                  swimlanes))

        ;; 基于偏移后的实体重新计算画布尺寸
        all-shifted-points (if (and (zero? shift-x) (zero? shift-y))
                             all-points ;; Optimization: if no shift, points are same
                             (let [;; Need to shift the points we calculated earlier
                                   shift-pt (fn [p] {:x (+ (:x p) shift-x) :y (+ (:y p) shift-y)})]
                               (map shift-pt all-points)))

        max-x (if (seq all-shifted-points) (apply max (map :x all-shifted-points)) width)
        max-y (if (seq all-shifted-points) (apply max (map :y all-shifted-points)) height)]

    (assoc layout-result
           :nodes nodes-shifted
           :edges edges-shifted
           :swimlanes swimlanes-shifted
           :width (+ max-x padding)
           :height (+ max-y padding))))

(defn- apply-layout-rules [nodes]
  (let [rules (rule/load-rules)
        layout-rules (rule/get-layout-rules (:rules rules))]
    (mapv (fn [node]
            (let [matched-rule (rule/get-matching-rule layout-rules {} node)]
              (merge node matched-rule)))
          nodes)))

(defn layout [nodes edges pools options]
  (let [layout-mode (:layout options)
        ;; Apply layout rules (ports, sizing, etc.) before any processing
        nodes-with-rules (apply-layout-rules nodes)

        routing-mode (:routing options "manhattan")
        route-fn (case routing-mode
                   "spline" spline/route-edges
                   "ortho" ortho/route-edges
                   "architecture" architecture/route-edges
                   manhattan/route-edges)
        sized-nodes (mapv coerce-node-size nodes-with-rules) ;; Use nodes with rules
        hidden-edge? (fn [e]
                       (let [t (:type e)
                             hidden (:hidden e)]
                         (or (= t :invisible)
                             (= t "invisible")
                             (= hidden true)
                             (= hidden "true"))))
        route-visible (fn [layout]
                        (route-fn (update layout :edges (fn [es] (vec (remove hidden-edge? es))))
                                  options))]
    (cond
      (= layout-mode "compound")
      (-> (compound/layout sized-nodes edges pools options)
          (route-visible)
          (normalize-layout))

      (= layout-mode "swimlane")
      ;; Swimlane 模式使用排序后的泳道，再进入统一坐标分配流程
      ;; FIX: Filter out pool nodes from content nodes, as they are containers, not layout nodes
      (let [content-nodes (filterv #(not= (:type %) :pool) sized-nodes)
            ordered-swimlanes (swimlane-ordering/order-swimlanes content-nodes edges pools options)]
        (-> (assign-coordinates content-nodes edges ordered-swimlanes options)
            (route-visible)
            (normalize-layout)))

      (= layout-mode "cluster")
      ;; Cluster 模式复用泳道的坐标分配与集群后处理，确保节点 x/y/w/h 完整
      ;; FIX: Filter out pool nodes here too
      (let [content-nodes (filterv #(not= (:type %) :pool) sized-nodes)]
        (-> (assign-coordinates content-nodes edges pools options)
            (route-visible)
            (normalize-layout)))

      :else
      (let [simple-layout (simple/layout sized-nodes edges options)]
        (-> simple-layout
            (route-visible)
            (normalize-layout))))))