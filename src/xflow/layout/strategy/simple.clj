(ns xflow.layout.strategy.simple
  (:require [xflow.layout.sugiyama :as sugiyama]))

(defn assign-coordinates [nodes edges options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)

        ;; --- Phase 3: Dummy Nodes for Long Edges ---
        ;; Insert dummy nodes to break long edges into segments
        ;; This ensures long edges are routed correctly through layers
        dummy-result (sugiyama/insert-dummy-nodes ranked-nodes edges)
        expanded-nodes (:nodes dummy-result)
        segment-edges (:edges dummy-result)

        ;; --- Optimization: Apply Crossing Minimization (交叉最小化) ---
        ;; 使用重心法重新排序节点以减少连线交叉 (Uses expanded graph)
        ordered-nodes (sugiyama/order-nodes expanded-nodes segment-edges)

        ;; --- Optimization: Apply Advanced Coordinate Assignment (坐标分配) ---
        ;; 使用中位数启发式算法垂直对齐长连线，提高美观度 (Uses expanded graph)
        processed-all-nodes (sugiyama/assign-coordinates ordered-nodes segment-edges options)

        ;; --- Restoration ---
        ;; Filter out dummy nodes and apply their positions as waypoints to original edges
        final-nodes (remove :dummy? processed-all-nodes)
        final-edges (sugiyama/apply-edge-points edges processed-all-nodes)

        ;; Calculate total graph dimensions based on processed nodes
        width (if (seq final-nodes)
                (+ (apply max (map #(+ (:x %) (:w %)) final-nodes)) 50)
                0)
        height (if (seq final-nodes)
                 (+ (apply max (map #(+ (:y %) (:h %)) final-nodes)) 50)
                 0)]

    {:nodes final-nodes
     :edges final-edges
     :width width
     :height height
     :swimlanes []}))

(defn layout [nodes edges options]
  (assign-coordinates nodes edges options)) ;; No swimlanes for simple layout
