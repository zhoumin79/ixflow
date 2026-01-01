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
            [xflow.theme.rule :as rule]
            [xflow.geometry :as geo]))

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
        straightened-nodes (sugiyama/straighten-dummy-nodes edges processed-nodes options)

      ;; Apply coordinates from dummy nodes to original edges as waypoints
        edges-with-points (sugiyama/apply-edge-points edges straightened-nodes)

      ;; Remove dummy nodes
        final-nodes (remove :dummy? processed-nodes)]

    {:nodes final-nodes
     :edges edges-with-points
     :swimlanes swimlane-geoms
     :width width
     :height height}))

(defn normalize-layout [{:keys [nodes edges swimlanes width height] :as layout-result}]
  (let [;; 1. Calculate label positions for edges if missing
        edges-with-labels (mapv (fn [e]
                                  (if (and (:label e) (:points e) (seq (:points e)) (not (:label-pos e)))
                                    (assoc e :label-pos (geo/calculate-label-pos (:points e)))
                                    e))
                                edges)

        ;; 2. Collect all items that have coordinates for bounding box calculation
        ;; Nodes and Swimlanes have :x, :y, :w, :h
        ;; Edges have :points (vector of points) and :label-pos

        node-points (mapcat (fn [n] [{:x (:x n) :y (:y n)}
                                     {:x (+ (:x n) (or (:w n) 0)) :y (+ (:y n) (or (:h n) 0))}])
                            nodes)
        swimlane-points (mapcat (fn [s] [{:x (:x s) :y (:y s)}
                                         {:x (+ (:x s) (or (:w s) 0)) :y (+ (:y s) (or (:h s) 0))}])
                                swimlanes)
        edge-points (mapcat :points edges-with-labels)
        label-points (keep :label-pos edges-with-labels)

        all-points (concat node-points swimlane-points edge-points label-points)
        bounds (geo/bounding-box all-points)

        min-x (or (:min-x bounds) 0)
        min-y (or (:min-y bounds) 0)

        padding 50
        shift-x (- padding min-x)
        shift-y (- padding min-y)]

    (if (and (zero? shift-x) (zero? shift-y))
      (assoc layout-result :edges edges-with-labels)
      (let [shifted-nodes (geo/shift-items nodes shift-x shift-y)
            shifted-edges (geo/shift-items edges-with-labels shift-x shift-y)
            shifted-swimlanes (geo/shift-items swimlanes shift-x shift-y)

            new-max-x (+ (or (:max-x bounds) width) shift-x)
            new-max-y (+ (or (:max-y bounds) height) shift-y)]

        (assoc layout-result
               :nodes shifted-nodes
               :edges shifted-edges
               :swimlanes shifted-swimlanes
               :width (+ new-max-x padding)
               :height (+ new-max-y padding))))))

(defn- apply-layout-rules [nodes]
  (let [rules (rule/load-rules)
        layout-rules (rule/get-layout-rules (:rules rules))]
    (mapv (fn [node]
            (let [matched-rule (rule/get-matching-rule layout-rules {} node)]
              (merge node matched-rule)))
          nodes)))

(defmulti layout-strategy
  "Dispatch layout strategy based on options.
   Returns a map with :nodes, :edges, and optionally :swimlanes."
  (fn [ctx] (get-in ctx [:options :layout])))

(defmethod layout-strategy "compound" [ctx]
  (compound/layout (:nodes ctx) (:edges ctx) (:pools ctx) (:options ctx)))

(defmethod layout-strategy "swimlane" [ctx]
  (let [{:keys [nodes edges pools options]} ctx
        ;; Filter out pool nodes from content nodes
        content-nodes (filterv #(not= (:type %) :pool) nodes)
        ordered-swimlanes (swimlane-ordering/order-swimlanes content-nodes edges pools options)]
    (assign-coordinates content-nodes edges ordered-swimlanes options)))

(defmethod layout-strategy "cluster" [ctx]
  (let [{:keys [nodes edges pools options]} ctx
        content-nodes (filterv #(not= (:type %) :pool) nodes)]
    (assign-coordinates content-nodes edges pools options)))

(defmethod layout-strategy :default [ctx]
  (simple/layout (:nodes ctx) (:edges ctx) (:options ctx)))

(defn- prepare-layout
  "Prepare layout context: apply rules, coerce sizes."
  [{:keys [nodes edges pools options] :as ctx}]
  (let [;; Apply layout rules (ports, sizing, etc.)
        nodes-with-rules (apply-layout-rules nodes)
        ;; Coerce node sizes
        sized-nodes (mapv coerce-node-size nodes-with-rules)]
    (assoc ctx :nodes sized-nodes)))

(defn- execute-strategy
  "Execute the selected layout strategy."
  [ctx]
  (let [result (layout-strategy ctx)]
    (merge ctx result)))

(defn- route-layout
  "Apply routing to the layout result."
  [{:keys [nodes edges options] :as ctx}]
  (let [routing-mode (:routing options "manhattan")
        route-fn (case routing-mode
                   "spline" spline/route-edges
                   "ortho" ortho/route-edges
                   "architecture" architecture/route-edges
                   manhattan/route-edges)

        hidden-edge? (fn [e]
                       (let [t (:type e)
                             hidden (:hidden e)]
                         (or (= t :invisible)
                             (= t "invisible")
                             (= hidden true)
                             (= hidden "true"))))

        ;; Filter edges for routing, but keep original edges structure if needed?
        ;; The original code updated the layout edges.
        visible-edges (vec (remove hidden-edge? edges))

        ;; Run routing
        routed-result (route-fn {:nodes nodes :edges visible-edges} options)]

    (assoc ctx
           :edges (:edges routed-result)
           :nodes (:nodes routed-result)))) ;; Some routers might adjust nodes (e.g. ports)

(defn- finalize-layout
  "Normalize and clean up the layout result."
  [ctx]
  (normalize-layout (select-keys ctx [:nodes :edges :swimlanes :width :height])))

(defn layout
  "Main layout entry point using a pipeline."
  [nodes edges pools options]
  (-> {:nodes nodes :edges edges :pools pools :options options}
      prepare-layout
      execute-strategy
      route-layout
      finalize-layout))