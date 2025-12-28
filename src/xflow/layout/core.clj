(ns xflow.layout.core
  (:require [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.strategy.swimlane :as swimlane]
            [xflow.layout.strategy.cluster :as cluster]
            [xflow.layout.routing.manhattan :as manhattan]))

(defn- assign-coordinates [nodes edges pools options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)
        layout-mode (:layout options)
        mode (if (= layout-mode "cluster") "vertical" (:swimlane-mode options "horizontal"))
        direction (:direction options "lr")

        ;; Group by Swimlane
        lanes-data (swimlane/group-by-swimlane ranked-nodes pools options)
        lanes (swimlane/calculate-lane-dimensions lanes-data mode)

        ;; Calculate Node Positions (Initial Pass)
        initial-nodes (swimlane/assign-initial-coordinates ranked-nodes lanes options)

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
  (let [layout-mode (:layout options)
        ;; Force vertical mode for cluster layout to ensure routing matches coordinate assignment
        options (if (= layout-mode "cluster")
                  (assoc options :swimlane-mode "vertical")
                  options)]
    (-> (assign-coordinates nodes edges pools options)
        (manhattan/route-edges options))))
