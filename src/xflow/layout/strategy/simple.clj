(ns xflow.layout.strategy.simple
  (:require [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.coordinate :as coordinate]
            [xflow.geometry :as geo]))

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
        coord-result (coordinate/assign-coordinates ordered-nodes segment-edges options)
        processed-all-nodes (:nodes coord-result)

        ;; --- Restoration ---
        ;; Filter out dummy nodes and apply their positions as waypoints to original edges
        final-nodes (remove :dummy? processed-all-nodes)
        final-edges (sugiyama/apply-edge-points edges processed-all-nodes)

        ;; Calculate total graph dimensions based on processed nodes
        bounds (geo/bounding-box final-nodes)
        width (+ (or (:max-x bounds) 0) 50)
        height (+ (or (:max-y bounds) 0) 50)]

    {:nodes final-nodes
     :edges final-edges
     :width width
     :height height
     :swimlanes []}))

(defn- rotate-point [pt]
  (if (map? pt)
    (assoc pt :x (:y pt) :y (:x pt))
    [(second pt) (first pt)]))

(defn- rotate-nodes [nodes]
  (mapv (fn [n]
          (assoc n
                 :x (:y n)
                 :y (:x n)
                 :w (:h n)
                 :h (:w n)))
        nodes))

(defn- rotate-edges [edges]
  (mapv (fn [e]
          (if (:points e)
            (assoc e :points (mapv rotate-point (:points e)))
            e))
        edges))

(defn layout [nodes edges options]
  (let [direction (:direction options "tb")
        horizontal? (or (= direction "lr") (= direction "rl"))

        ;; 1. Prepare nodes (swap w/h if horizontal)
        input-nodes (if horizontal?
                      (mapv #(assoc % :w (:h %) :h (:w %)) nodes)
                      nodes)

        ;; 2. Run Layout (Always Top-Down)
        result (assign-coordinates input-nodes edges options)

        ;; 3. Transform Result (swap x/y if horizontal)
        final-nodes (if horizontal?
                      (rotate-nodes (:nodes result))
                      (:nodes result))
        final-edges (if horizontal?
                      (rotate-edges (:edges result))
                      (:edges result))
        width (if horizontal? (:height result) (:width result))
        height (if horizontal? (:width result) (:height result))]

    (assoc result
           :nodes final-nodes
           :edges final-edges
           :width width
           :height height)))