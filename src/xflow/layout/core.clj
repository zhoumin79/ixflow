(ns xflow.layout.core
  (:require [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.strategy.swimlane :as swimlane]
            [xflow.layout.strategy.cluster :as cluster]
            [xflow.layout.strategy.simple :as simple]
            [xflow.layout.routing.manhattan :as manhattan]))

(defn- assign-coordinates [nodes edges pools options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)

        ;; --- Phase 3: Insert Dummy Nodes for Long Edges ---
        ;; This splits long edges into segments with dummy nodes
        {:keys [nodes edges dummies] :as graph-with-dummies}
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
        ordered-nodes (sugiyama/order-nodes nodes-with-dummies edges-with-dummies)
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
        ;; Apply coordinates from dummy nodes to original edges as waypoints
        edges-with-points (sugiyama/apply-edge-points edges processed-nodes)

        ;; Remove dummy nodes
        final-nodes (remove :dummy? processed-nodes)]

    {:nodes final-nodes
     :edges edges-with-points
     :swimlanes swimlane-geoms
     :width width
     :height height}))

(defn- normalize-layout [{:keys [nodes edges swimlanes width height] :as layout-result}]
  (let [;; Collect all points to find bounds
        node-points (mapcat (fn [n] [{:x (:x n) :y (:y n)}
                                     {:x (+ (:x n) (:w n)) :y (+ (:y n) (:h n))}]) nodes)
        edge-points (mapcat (fn [e] (:points e)) edges)
        swimlane-points (mapcat (fn [s] [{:x (:x s) :y (:y s)}
                                         {:x (+ (:x s) (:w s)) :y (+ (:y s) (:h s))}]) swimlanes)

        all-points (concat node-points edge-points swimlane-points)

        min-x (if (seq all-points) (apply min (map :x all-points)) 0)
        min-y (if (seq all-points) (apply min (map :y all-points)) 0)

        padding 20
        ;; Shift to ensure min is at padding
        shift-x (- padding min-x)
        shift-y (- padding min-y)]

    (if (and (zero? shift-x) (zero? shift-y))
      layout-result
      (let [new-nodes (map (fn [n] (assoc n :x (+ (:x n) shift-x) :y (+ (:y n) shift-y))) nodes)
            new-edges (map (fn [e] (if (:points e)
                                     (update e :points (fn [pts] (mapv #(assoc % :x (+ (:x %) shift-x) :y (+ (:y %) shift-y)) pts)))
                                     e)) edges)
            new-swimlanes (map (fn [s] (assoc s :x (+ (:x s) shift-x) :y (+ (:y s) shift-y))) swimlanes)

            ;; Re-calculate dimensions
            all-new-points (concat (mapcat (fn [n] [{:x (:x n) :y (:y n)} {:x (+ (:x n) (:w n)) :y (+ (:y n) (:h n))}]) new-nodes)
                                   (mapcat (fn [e] (:points e)) new-edges)
                                   (mapcat (fn [s] [{:x (:x s) :y (:y s)} {:x (+ (:x s) (:w s)) :y (+ (:y s) (:h s))}]) new-swimlanes))

            max-x (if (seq all-new-points) (apply max (map :x all-new-points)) width)
            max-y (if (seq all-new-points) (apply max (map :y all-new-points)) height)]

        (assoc layout-result
               :nodes new-nodes
               :edges new-edges
               :swimlanes new-swimlanes
               :width (+ max-x padding)
               :height (+ max-y padding))))))

(defn layout [nodes edges pools options]
  (let [layout-mode (:layout options)]
    (if (= layout-mode "simple")
      ;; Simple Layout Strategy (No pools/lanes/clusters)
      (let [direction (:direction options "tb")
            ;; Map direction to routing mode: tb -> vertical, lr -> horizontal
            routing-mode (if (= direction "tb") "vertical" "horizontal")
            options (assoc options :swimlane-mode routing-mode)]
        (-> (simple/assign-coordinates nodes edges options)
            (manhattan/route-edges options)
            (normalize-layout)))

      ;; Complex Layout Strategy (Swimlanes/Clusters)
      (let [options (if (= layout-mode "cluster")
                      (assoc options :swimlane-mode "vertical")
                      options)]
        (-> (assign-coordinates nodes edges pools options)
            (manhattan/route-edges options)
            (normalize-layout))))))