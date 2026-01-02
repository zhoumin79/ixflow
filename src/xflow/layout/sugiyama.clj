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
  "Builds adjacency lists for parents and children."
  (let [node-ids (set (map :id nodes))
        valid-edges (filter (fn [e] (and (node-ids (:from e)) (node-ids (:to e)))) edges)]
    {:parents (reduce (fn [m e] (update m (:to e) (fnil conj []) (:from e))) {} valid-edges)
     :children (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} valid-edges)}))

(defn- barycenter [node neighbors order-map]
  "Calculates the barycenter (average position) of neighbors."
  (if (empty? neighbors)
    (get order-map (:id node) 0) ;; Keep current position if no neighbors
    (/ (reduce + (map #(get order-map % 0) neighbors))
       (count neighbors))))

(defn- sort-layer [layer neighbors-map order-map]
  "Sorts a layer based on the barycenter of its neighbors."
  (let [nodes-with-val
        (map (fn [node]
               (let [neighbors (get neighbors-map (:id node) [])
                     val (barycenter node neighbors order-map)]
                 {:node node :val val}))
             layer)]
    (map :node (sort-by :val nodes-with-val))))

;; --- Phase 1: Layering ---

(defn- remove-cycles [nodes edges]
  "Returns a set of edges that form a DAG by reversing back-edges."
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
  "Assigns ranks to nodes using Network Simplex algorithm.
   Handles cycles by temporarily reversing back-edges."
  (let [acyclic-edges (remove-cycles nodes edges)]
    (network-simplex/assign-ranks nodes acyclic-edges)))

;; --- Phase 2: Crossing Minimization ---

(defn- crossing-minimization-sweep [ranks max-rank parents children iterations]
  "Performs the iterative sweep to minimize crossings."
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
  "Orders nodes within each rank to minimize edge crossings."
  (let [ranked-nodes (sort-by :rank nodes)
        ranks (group-by :rank ranked-nodes)
        max-rank (apply max (keys ranks))
        {:keys [parents children]} (build-adj-matrix nodes edges)
        iterations 8]
    (crossing-minimization-sweep ranks max-rank parents children iterations)))

;; --- Phase 3: Dummy Nodes (Coordinate Assignment Prep) ---

(defn insert-dummy-nodes [nodes edges]
  "Splits long edges (spanning > 1 rank) into segments with dummy nodes."
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
  "Constructs full paths for edges, including source/target centers and any dummy node waypoints."
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
  "Full Sugiyama layout pipeline (mostly for testing or simple cases)."
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
