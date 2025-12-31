(ns xflow.layout.sugiyama
  (:require [clojure.set :as set]))

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

(defn assign-ranks [nodes edges]
  ;; Simple Longest Path layering
  (let [node-map (group-by :id nodes)
        adj (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} edges)
        ranks (atom {})
        visited-global (atom #{})]

    (letfn [(visit [node-id depth visited]
              (when-not (contains? visited node-id) ;; Cycle detection within path
                (swap! ranks update node-id #(max (or % 0) depth))
                (swap! visited-global conj node-id)
                (doseq [child (get adj node-id)]
                  (visit child (inc depth) (conj visited node-id)))))]

      (let [roots (get-roots nodes edges)]
        (doseq [root (if (seq roots) roots nodes)] ;; If no roots (cycle), start anywhere
          ;; Only process if not already visited (avoids infinite rank increment in cycles)
          (when-not (contains? @visited-global (:id root))
            (visit (:id root) 0 #{})))))

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
        dummy-coords (into {} (map (fn [n] [(:id n) {:x (:x n) :y (:y n)}]) (filter :dummy? nodes)))]
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
  (let [direction (name (:direction options "tb")) ;; Ensure string
        vertical? (contains? #{"tb" "bt" "vertical"} direction) ;; Support bt/vertical
        reverse-rank? (= direction "bt")
        reverse-align? (= direction "rl")

        ;; Config
        default-w 180
        default-h 50
        gap-x 50
        gap-y 80

        ;; Dimensions based on direction
        ;; Note: We can't use fixed w/h anymore if nodes vary in size
        sep-in-rank (if vertical? gap-x gap-y)
        sep-across-rank (if vertical? gap-y gap-x)

        ;; Group by rank
        ranks (group-by :rank ordered-nodes)
        max-rank (if (seq ranks) (apply max (keys ranks)) 0)

        nodes-map (into {} (map (fn [n] [(:id n) n]) ordered-nodes))

        coords (atom {})]))

    ;; 1. Place Rank 0 (Roots)

(defn layout-graph [nodes edges options]
  (let [ranked-nodes (assign-ranks nodes edges)
        {:keys [nodes dummies] :as graph-with-dummies} (insert-dummy-nodes ranked-nodes edges)

        ;; Use standard ordering
        ;; FIX: Use nodes from graph-with-dummies which includes dummy nodes!
        ordered-nodes (order-nodes (:nodes graph-with-dummies) (:edges graph-with-dummies))

        ;; Assign coordinates
        nodes-with-coords (assign-coordinates ordered-nodes (:edges graph-with-dummies) options)

        ;; Apply edge points
        edges-with-points (apply-edge-points edges nodes-with-coords)

        ;; Filter real nodes
        final-nodes (remove :dummy? nodes-with-coords)

        ;; Calculate dimensions
        min-x (if (seq final-nodes) (apply min (map :x final-nodes)) 0)
        min-y (if (seq final-nodes) (apply min (map :y final-nodes)) 0)
        max-x (if (seq final-nodes) (apply max (map #(+ (:x %) (:w %)) final-nodes)) 0)
        max-y (if (seq final-nodes) (apply max (map #(+ (:y %) (:h %)) final-nodes)) 0)]

    {:nodes final-nodes
     :edges edges-with-points
     :width (- max-x min-x)
     :height (- max-y min-y)}))
