(ns xflow.layout.sugiyama
  (:require [clojure.set :as set]))

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

(defn assign-ranks [nodes edges]
  ;; Simple Longest Path layering
  (let [node-map (group-by :id nodes)
        adj (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} edges)
        ranks (atom {})]

    (letfn [(visit [node-id depth visited]
              (when-not (contains? visited node-id) ;; Cycle detection
                (swap! ranks update node-id #(max (or % 0) depth))
                (doseq [child (get adj node-id)]
                  (visit child (inc depth) (conj visited node-id)))))]

      (let [roots (get-roots nodes edges)]
        (doseq [root (if (seq roots) roots nodes)] ;; If no roots (cycle), start anywhere
          (visit (:id root) 0 #{}))))

    ;; Assign rank to node objects
    (map (fn [n] (assoc n :rank (get @ranks (:id n) 0))) nodes)))

;; --- Crossing Minimization (Barycenter Method) ---

(defn- build-adj-matrix [nodes edges]
  (let [node-ids (set (map :id nodes))
        valid-edges (filter (fn [e] (and (node-ids (:from e)) (node-ids (:to e)))) edges)]
    {:parents (reduce (fn [m e] (update m (:to e) (fnil conj []) (:from e))) {} valid-edges)
     :children (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} valid-edges)}))

(defn- barycenter [node neighbors order-map]
  (if (empty? neighbors)
    (get order-map (:id node) 0) ;; Keep current position if no neighbors
    (/ (reduce + (map #(get order-map % 0) neighbors))
       (count neighbors))))

(defn- sort-layer [layer neighbors-map order-map]
  (let [nodes-with-val
        (map (fn [node]
               (let [neighbors (get neighbors-map (:id node) [])
                     val (barycenter node neighbors order-map)]
                 {:node node :val val}))
             layer)]
    (map :node (sort-by :val nodes-with-val))))

(defn order-nodes [nodes edges]
  (let [ranked-nodes (sort-by :rank nodes)
        ranks (group-by :rank ranked-nodes)
        max-rank (apply max (keys ranks))
        {:keys [parents children]} (build-adj-matrix nodes edges)
        iterations 8] ;; Standard number of iterations

    ;; Initial random order (just preserve list order from group-by, but assign index)
    (loop [iter 0
           current-ranks ranks
           ;; Build initial order-map: {node-id -> index}
           order-map (into {} (mapcat (fn [[r layer]]
                                        (map-indexed (fn [i n] [(:id n) i]) layer))
                                      ranks))]
      (if (>= iter iterations)
        ;; Done: Assign final :order to nodes
        (flatten
         (map (fn [[r layer]]
                (map-indexed (fn [i n] (assoc n :order i)) layer))
              current-ranks))

        ;; Sweep
        (let [;; Down sweep (Rank 1 -> Max)
              [down-ranks down-order-map]
              (reduce
               (fn [[rs om] r]
                 (let [layer (get rs r)
                       ;; Sort based on parents (rank r-1)
                       sorted-layer (sort-layer layer parents om)
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
                       sorted-layer (sort-layer layer children om)
                       new-om (merge om (into {} (map-indexed (fn [i n] [(:id n) i]) sorted-layer)))]
                   [(assoc rs r sorted-layer) new-om]))
               [down-ranks down-order-map]
               (range (dec max-rank) -1 -1))]

          (recur (inc iter) up-ranks up-order-map))))))

;; --- Coordinate Assignment (Median Heuristic) ---

(defn insert-dummy-nodes [nodes edges]
  (let [nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Helper to check span
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

(defn remove-dummy-nodes [nodes edges dummy-nodes-map]
  (let [;; Filter out dummy nodes
        real-nodes (remove :dummy? nodes)

        ;; Map of dummy-id -> coordinates
        dummy-coords (into {} (map (fn [n] [(:id n) {:x (:x n) :y (:y n)}]) (filter :dummy? nodes)))

        ;; Reconstruct edges
        ;; Edges passed here are the SEGMENTS. We need the ORIGINAL edges.
        ;; Wait, the caller (simple.clj) has the original edges.
        ;; We just need to attach points to the original edges using dummy-coords.
        ]
    {:nodes real-nodes
     :dummy-coords dummy-coords}))

;; Helper to apply waypoints to original edges
(defn apply-edge-points [original-edges dummy-nodes-with-coords]
  (let [edge-dummies (group-by #(:id (:parent-edge %)) (filter :dummy? dummy-nodes-with-coords))]
    (map (fn [edge]
           (if-let [dummies (get edge-dummies (:id edge))]
             ;; Sort dummies by rank to ensure correct order
             (let [sorted-dummies (sort-by :rank dummies)
                   points (mapv (fn [d] {:x (:x d) :y (:y d)}) sorted-dummies)]
               (assoc edge :points points))
             edge))
         original-edges)))

(defn assign-coordinates [ordered-nodes edges options]
  (let [direction (:direction options "tb")
        vertical? (or (nil? direction) (= direction "tb"))

        ;; Config
        node-w 180
        node-h 50
        gap-x 50
        gap-y 80

        ;; Dimensions based on direction
        w (if vertical? node-w node-h)
        h (if vertical? node-h node-w)
        sep-in-rank (if vertical? gap-x gap-y)
        sep-across-rank (if vertical? gap-y gap-x)

        ;; Group by rank
        ranks (group-by :rank ordered-nodes)
        max-rank (if (seq ranks) (apply max (keys ranks)) 0)

        coords (atom {})]

    ;; 1. Place Rank 0 (Roots)
    (let [rank0 (sort-by :order (get ranks 0))]
      (loop [nodes rank0
             pos 0]
        (when (seq nodes)
          (let [n (first nodes)
                curr-w (if (:dummy? n) 0 w)]
            (swap! coords assoc (:id n) pos)
            (recur (rest nodes) (+ pos curr-w sep-in-rank))))))

    ;; 2. Forward Sweep (Rank 1 -> Max)
    (doseq [r (range 1 (inc max-rank))]
      (let [layer (sort-by :order (get ranks r))
            parents-map (group-by :to (filter (fn [e] (some #(= (:id %) (:from e)) (get ranks (dec r)))) edges))]

        ;; Determine ideal positions
        (let [placements
              (map (fn [n]
                     (let [parents (get parents-map (:id n))
                           parent-coords (keep #(get @coords (:from %)) parents)
                           ideal (if (seq parent-coords)
                                   (/ (reduce + parent-coords) (count parent-coords))
                                   nil)]
                       {:node n :ideal ideal}))
                   layer)]

          ;; Resolve overlaps and place
          (let [placed-items
                (loop [items placements
                       last-pos nil
                       last-w nil
                       results []]
                  (if (seq items)
                    (let [{:keys [node ideal]} (first items)
                          curr-w (if (:dummy? node) 0 w)
                          min-pos (if last-pos (+ last-pos last-w sep-in-rank) 0)
                          pos (if ideal (max min-pos ideal) min-pos)]
                      (recur (rest items) pos curr-w (conj results {:node node :pos pos})))
                    results))]

            ;; Center Alignment Optimization
            (let [current-center (if (seq placed-items)
                                   (/ (reduce + (map :pos placed-items)) (count placed-items))
                                   0)
                  nodes-with-parents (filter #(get parents-map (:id (:node %))) placed-items)
                  parent-centers (mapcat (fn [item]
                                           (let [parents (get parents-map (:id (:node item)))]
                                             (keep #(get @coords (:from %)) parents)))
                                         nodes-with-parents)
                  avg-parent-center (if (seq parent-centers)
                                      (/ (reduce + parent-centers) (count parent-centers))
                                      current-center)

                  shift (- avg-parent-center current-center)]

              (doseq [{:keys [node pos]} placed-items]
                (swap! coords assoc (:id node) (+ pos shift))))))))

    ;; 3. Return updated nodes with X/Y
    (let [final-nodes
          (map (fn [n]
                 (let [c (get @coords (:id n) 0)
                       rank-pos (* (:rank n) (+ h sep-across-rank))
                       nw (if (:dummy? n) 0 node-w)
                       nh (if (:dummy? n) 0 node-h)]
                   (if vertical?
                     (assoc n :x c :y rank-pos :w nw :h nh)
                     (assoc n :x rank-pos :y c :w nw :h nh))))
               ordered-nodes)

          min-x (if (seq final-nodes) (apply min (map :x final-nodes)) 0)
          min-y (if (seq final-nodes) (apply min (map :y final-nodes)) 0)]

      (map (fn [n]
             (assoc n
                    :x (- (:x n) min-x)
                    :y (- (:y n) min-y)))
           final-nodes))))
