(ns xflow.layout.strategy.compound
  (:require [xflow.layout.sugiyama :as sugiyama]
            [clojure.set :as set]
            [xflow.geometry :as geo]))

(defn- get-all-descendants
  "Recursively get all descendant node IDs for a node (including itself)."
  [node]
  (if (or (not= (:type node) :group) (empty? (:children node)))
    #{(:id node)}
    (apply set/union #{(:id node)}
           (map get-all-descendants (:children node)))))

(defn- build-descendant-map
  "Builds a map of {descendant-id -> direct-child-id} for a list of direct children.
   Used to lift edges from deep descendants to the current layout level."
  [children]
  (reduce (fn [acc child]
            (let [descendants (get-all-descendants child)]
              (reduce (fn [m desc-id] (assoc m desc-id (:id child)))
                      acc
                      descendants)))
          {}
          children))

(defn- lift-edges
  "Finds edges relevant to the current level by 'lifting' connections from descendants.
   Returns edges where from/to are mapped to the IDs of the direct children."
  [children all-edges]
  (let [node-map (build-descendant-map children)]
    (->> all-edges
         (keep (fn [edge]
                 (let [u (get node-map (:from edge))
                       v (get node-map (:to edge))]
                   (when (and u v (not= u v)) ;; Both ends are in this group (or descendants), and not self-loop
                     (assoc edge :from u :to v)))))
         (distinct)))) ;; Remove duplicates created by lifting

(defn- process-pool-tree [pool nodes-by-swimlane parent-path]
  (let [current-path (if parent-path
                       (str parent-path " / " (:name pool))
                       (:name pool))

        ;; 1. 获取当前层级的原子节点 (根据 swimlane-id)
        atomic-children (get nodes-by-swimlane current-path [])

        ;; 2. 获取子分组 (从 parser 解析的嵌套 nodes 中筛选 :pool 类型)
        sub-pools (filter #(= (:type %) :pool) (:nodes pool))

        ;; 3. 递归处理子分组
        group-children (map #(process-pool-tree % nodes-by-swimlane current-path) sub-pools)

        ;; 4. 合并所有子节点 (原子节点 + 子分组)
        all-children (concat atomic-children group-children)]

    {:id (:name pool)
     :type :group
     :props (:props pool)
     :children all-children}))

(defn- build-hierarchy [nodes pools]
  (let [nodes-by-swimlane (group-by :swimlane-id nodes)

        ;; 处理顶层分组 (递归构建树状结构)
        groups (map #(process-pool-tree % nodes-by-swimlane nil) pools)

        ;; 获取顶层原子节点 (没有 swimlane-id 的节点)
        top-level-atomic (get nodes-by-swimlane nil [])]

    {:groups groups
     :top-level-atomic top-level-atomic}))

(defn- spine-align-layout [layout-res]
  (let [nodes (:nodes layout-res)
        real-nodes (remove :dummy? nodes)
        rank-counts (->> real-nodes (group-by :rank) vals (map count))
        max-per-rank (if (seq rank-counts) (apply max rank-counts) 0)]
    (if (<= max-per-rank 1)
      (let [centers (map (fn [n] (+ (:x n) (/ (:w n) 2.0))) real-nodes)
            spine (if (seq centers) (/ (reduce + centers) (count centers)) 0.0)
            aligned (mapv (fn [n]
                            (assoc n :x (- spine (/ (:w n) 2.0))))
                          nodes)
            bounds (geo/bounding-box aligned)
            min-x (or (:min-x bounds) 0.0)
            min-y (or (:min-y bounds) 0.0)
            shifted (geo/shift-items aligned (- min-x) (- min-y))
            new-bounds (geo/bounding-box shifted)]
        (assoc layout-res :nodes shifted
               :width (or (:max-x new-bounds) 0.0)
               :height (or (:max-y new-bounds) 0.0)))
      layout-res)))

(defn- layout-recursive [node all-edges options]
  (if (or (not= (:type node) :group) (empty? (:children node)))
    ;; 原子节点或空分组: 直接返回 (如果未设置大小，使用默认值)
    (assoc node :w (or (:w node) 180) :h (or (:h node) 50))

    ;; 分组节点: 对子节点进行布局
    (let [children (:children node)
          ;; 递归布局子节点 (处理嵌套分组)
          processed-children (map #(layout-recursive % all-edges options) children)

          ;; 查找并提升边 (Lift Edges)
          ;; 关键修改：不再只查找直接连接子节点的边，而是将所有后代节点的边映射到当前层级
          internal-edges (lift-edges processed-children all-edges)

          ;; 检查分组特定的方向设置 (rankdir)
          group-dir (-> node :props :rankdir)
          group-options (if group-dir
                          (assoc options :direction (clojure.string/lower-case group-dir))
                          options)

          ;; 对子图运行 Sugiyama 布局算法
          layout-res (sugiyama/layout-graph processed-children internal-edges group-options)
          layout-res (spine-align-layout layout-res)

          ;; 设置内边距 (Padding)
          padding 40
          inner-w (:width layout-res)
          inner-h (:height layout-res)

          ;; 更新分组节点尺寸和内部布局信息
          final-group (assoc node
                             :w (+ inner-w (* 2 padding))
                             :h (+ inner-h (* 2 padding))
                             :children-layout (:nodes layout-res)
                             :internal-edges (:edges layout-res))]
      final-group)))

(defn- offset-children [parent-node]
  (let [parent-x (:x parent-node)
        parent-y (:y parent-node)
        padding 40]
    (map (fn [child]
           (let [abs-x (+ parent-x (:x child) padding)
                 abs-y (+ parent-y (:y child) padding)
                 child-with-pos (assoc child :x abs-x :y abs-y)]
             ;; Recurse if child is also a group
             (if (:children-layout child)
               (assoc child-with-pos :children-layout (offset-children child-with-pos))
               child-with-pos)))
         (:children-layout parent-node))))

(defn- flatten-results [nodes]
  (mapcat (fn [n]
            (if (:children-layout n)
              ;; It's a group: emit the group node itself AND its children
              (concat [(dissoc n :children-layout)] ;; The box
                      (flatten-results (:children-layout n))) ;; The contents
              [n]))
          nodes))

(defn layout [nodes edges pools options]
  (let [{:keys [groups top-level-atomic]} (build-hierarchy nodes pools)

        ;; 1. Recursive Layout (Bottom-Up)
        processed-groups (map #(layout-recursive % edges options) groups)

        ;; 2. Top-Level Layout
        ;; Construct Top-Level Graph: Top-Level Atomic + Sized Groups
        top-nodes (concat top-level-atomic processed-groups)

        ;; Top-Level Edges:
        ;; Use lift-edges for top level as well
        top-edges (lift-edges top-nodes edges)

        ;; FILTER: Ignore dashed edges AND cross edges for top-level ranking to allow side-by-side
        filtered-top-edges (filter #(and (not= (:type %) :dashed)
                                         (not= (:type %) :cross)) top-edges)

        ;; Run Top-Level Layout
        top-layout (sugiyama/layout-graph top-nodes filtered-top-edges options)

        ;; 3. Final Coordinate Assignment (Top-Down)

        ;; Post-Processing: Bottom-Alignment for Independent Columns in BT mode
        ;; If direction is BT and there are no vertical dependencies (filtered edges empty),
        ;; we assume a horizontal layout of columns and align them to the bottom.
        should-bottom-align? (and (= (clojure.string/lower-case (or (:direction options) "tb")) "bt")
                                  (empty? filtered-top-edges))

        final-top-nodes (if should-bottom-align?
                          (let [max-h (:height top-layout)]
                            (map (fn [n]
                                   (assoc n :y (- max-h (:h n))))
                                 (:nodes top-layout)))
                          (:nodes top-layout))

        ;; Apply offsets to children
        final-nodes-tree (map (fn [n]
                                (if (:children-layout n)
                                  (assoc n :children-layout (offset-children n))
                                  n))
                              final-top-nodes)

        ;; Flatten everything
        all-nodes (flatten-results final-nodes-tree)

        ;; Final Width/Height
        width (:width top-layout)
        height (:height top-layout)]

    {:nodes all-nodes
     :edges edges
     :width width
     :height height
     :swimlanes []}))