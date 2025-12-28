(ns xflow.layout.core
  (:require [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.strategy.swimlane :as swimlane]
            [xflow.layout.strategy.cluster :as cluster]
            [xflow.layout.strategy.simple :as simple]
            [xflow.layout.routing.manhattan :as manhattan]))

(defn- assign-coordinates [nodes edges pools options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)
        ;; --- Optimization: Apply Crossing Minimization ---
        ordered-nodes (sugiyama/order-nodes ranked-nodes edges)
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
          (swimlane/calculate-strip-geometries lanes width height mode))]

    {:nodes processed-nodes
     :edges edges
     :swimlanes swimlane-geoms
     :width width
     :height height}))

(defn layout [nodes edges pools options]
  (let [layout-mode (:layout options)]
    (if (= layout-mode "simple")
      ;; Simple Layout Strategy (No pools/lanes/clusters)
      (let [direction (:direction options "tb")
            ;; Map direction to routing mode: tb -> vertical, lr -> horizontal
            routing-mode (if (= direction "tb") "vertical" "horizontal")
            options (assoc options :swimlane-mode routing-mode)]
        (-> (simple/assign-coordinates nodes edges options)
            (manhattan/route-edges options)))

      ;; Complex Layout Strategy (Swimlanes/Clusters)
      (let [options (if (= layout-mode "cluster")
                      (assoc options :swimlane-mode "vertical")
                      options)]
        (-> (assign-coordinates nodes edges pools options)
            (manhattan/route-edges options))))))