(ns xflow.layout.strategy.swimlane-ordering
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; --- Swimlane ID Extraction ---

(defn get-ordered-lane-ids
  "Extracts a list of swimlane IDs based on DFS traversal of pools."
  [pools]
  (letfn [(visit [pool path]
            (let [current-path (conj path (:name pool))
                  full-id (str/join " / " current-path)
                  child-ids (mapcat #(when (= (:type %) :pool) (visit % current-path))
                                    (:nodes pool))]
              (cons full-id child-ids)))]
    (mapcat #(visit % []) pools)))

(defn- get-swimlane-order [pools]
  "Extracts a map of {swimlane-id -> index} based on the DFS traversal of pools.
   Matches the ID construction logic in xflow.layout.strategy.swimlane."
  (let [ids (get-ordered-lane-ids pools)]
    (into {} (map-indexed (fn [i id] [id i]) ids))))

;; --- Constrained Sorting Helpers ---

(defn- barycenter [node neighbors order-map]
  "Calculates the barycenter (average position) of neighbors.
   Returns the current position if no neighbors exist."
  (if (empty? neighbors)
    (get order-map (:id node) 0)
    (/ (reduce + (map #(get order-map % 0) neighbors))
       (count neighbors))))

(defn- sort-layer-constrained [layer neighbors-map order-map swimlane-order-map]
  "Sorts a layer while strictly respecting swimlane boundaries.
   1. Groups nodes by swimlane.
   2. Sorts the groups based on swimlane definition order.
   3. Sorts nodes WITHIN each group based on barycenter values."

  ;; 1. Compute Barycenter values for all nodes in layer
  (let [nodes-with-val
        (map (fn [node]
               (let [neighbors (get neighbors-map (:id node) [])
                     val (barycenter node neighbors order-map)]
                 {:node node :val val}))
             layer)

        ;; 2. Group by Swimlane ID
        ;; Use a default key for nodes without swimlane (e.g. global)
        nodes-by-lane (group-by #(get-in % [:node :swimlane-id]) nodes-with-val)

        ;; 3. Sort Lanes
        ;; Lanes not in the map get a high index (placed at bottom/right)
        sorted-lane-ids (sort-by #(get swimlane-order-map % 9999) (keys nodes-by-lane))]

    ;; 4. Flatten back to a list, sorting within lanes
    (mapcat (fn [lane-id]
              (let [lane-nodes (get nodes-by-lane lane-id)
                    ;; Sort by barycenter value
                    sorted-lane-nodes (sort-by :val lane-nodes)]
                (map :node sorted-lane-nodes)))
            sorted-lane-ids)))

(defn- group-by-swimlane-in-rank [layer swimlane-order-map]
  "Initial grouping for the first pass.
   Groups by swimlane and sorts lanes, preserving original relative order within lanes."
  (let [grouped (group-by :swimlane-id layer)
        sorted-lanes (sort-by #(get swimlane-order-map % 9999) (keys grouped))]
    (mapcat (fn [lane-id]
              (get grouped lane-id))
            sorted-lanes)))

;; --- Main Entry Point ---

(defn order-nodes [nodes edges pools]
  "Assigns :order to nodes using a Swimlane-Constrained Sugiyama method.
   Ensures nodes never cross swimlane boundaries during the ordering phase."
  (let [swimlane-order-map (get-swimlane-order pools)

        ;; Group by Rank
        ranked-nodes (sort-by :rank nodes)
        ranks (group-by :rank ranked-nodes)
        max-rank (if (seq ranks) (apply max (keys ranks)) 0)

        ;; Build Adjacency Matrices
        node-ids (set (map :id nodes))
        valid-edges (filter (fn [e] (and (node-ids (:from e)) (node-ids (:to e)))) edges)
        parents (reduce (fn [m e] (update m (:to e) (fnil conj []) (:from e))) {} valid-edges)
        children (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} valid-edges)

        iterations 12] ;; Standard Sugiyama iterations

    ;; Initial Sort: Just group by swimlane, ignore edge crossings initially
    (loop [iter 0
           current-ranks (update-vals ranks #(group-by-swimlane-in-rank % swimlane-order-map))
           ;; Initialize order-map based on this grouping
           order-map (into {} (mapcat (fn [[r layer]]
                                        (map-indexed (fn [i n] [(:id n) i]) layer))
                                      (update-vals ranks #(group-by-swimlane-in-rank % swimlane-order-map))))]

      (if (>= iter iterations)
        ;; Done: Flatten and assign final :order
        (flatten
         (map (fn [r]
                (let [layer (get current-ranks r)]
                  (map-indexed (fn [i n] (assoc n :order i)) layer)))
              (sort (keys current-ranks))))

        ;; Sweep with Constraint
        (let [;; Down sweep (Rank 1 -> Max)
              [down-ranks down-order-map]
              (reduce
               (fn [[rs om] r]
                 (let [layer (get rs r)
                       ;; Sort based on parents (rank r-1)
                       sorted-layer (sort-layer-constrained layer parents om swimlane-order-map)
                       ;; Update order map for this layer
                       new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                   [(assoc rs r sorted-layer) new-om]))
               [current-ranks order-map]
               (range 1 (inc max-rank)))

              ;; Up sweep (Rank Max-1 -> 0)
              [up-ranks up-order-map]
              (reduce
               (fn [[rs om] r]
                 (let [layer (get rs r)
                       ;; Sort based on children (rank r+1)
                       sorted-layer (sort-layer-constrained layer children om swimlane-order-map)
                       new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                   [(assoc rs r sorted-layer) new-om]))
               [down-ranks down-order-map]
               (range (dec max-rank) -1 -1))]

          (recur (inc iter) up-ranks up-order-map))))))

(defn order-swimlanes [nodes edges pools options]
  pools)
