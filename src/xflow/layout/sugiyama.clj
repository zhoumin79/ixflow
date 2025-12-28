(ns xflow.layout.sugiyama)

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
