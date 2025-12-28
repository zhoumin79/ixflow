(ns xflow.layout.strategy.cluster)

(defn process-layout [initial-nodes]
  (let [;; Separate global nodes (empty swimlane-id)
        global-nodes (filter #(empty? (:swimlane-id %)) initial-nodes)
        cluster-nodes (remove #(empty? (:swimlane-id %)) initial-nodes)

        ;; Calculate actual content width of clusters
        cluster-min-x (if (seq cluster-nodes) (apply min (map :x cluster-nodes)) 0)
        cluster-max-x (if (seq cluster-nodes) (apply max (map #(+ (:x %) (:w %)) cluster-nodes)) 0)
        cluster-center (/ (+ cluster-min-x cluster-max-x) 2)

        ;; Rank Split Strategy for Vertical Separation
        ;; Identify ranks that contain BOTH Global and Cluster nodes
        nodes-by-rank (group-by :rank initial-nodes)
        max-r (if (seq initial-nodes) (apply max (map :rank initial-nodes)) 0)

        ;; Calculate Y-shift for each rank
        rank-shifts
        (first
         (reduce (fn [[shifts current-shift] r]
                   (let [r-nodes (get nodes-by-rank r [])
                         has-global? (some #(empty? (:swimlane-id %)) r-nodes)
                         has-cluster? (some #(not (empty? (:swimlane-id %))) r-nodes)

                         ;; If conflict, we push Global nodes down by adding GAP to current-shift
                         ;; And subsequent ranks inherit the TOTAL shift
                         conflict? (and has-global? has-cluster?)
                         gap 150 ;; Extra vertical space

                         next-shift (if conflict? (+ current-shift gap) current-shift)]

                     [(assoc shifts r {:base current-shift :conflict? conflict?})
                      next-shift]))
                 [{} 0]
                 (range (inc max-r))))

        ;; Apply rank shifts (Layer 1 Processing)
        spaced-nodes
        (map (fn [n]
               (let [r (:rank n)
                     {:keys [base conflict?]} (get rank-shifts r)
                     is-global? (empty? (:swimlane-id n))

                     ;; Global nodes in conflict ranks get an EXTRA push
                     y-shift (if (and conflict? is-global?)
                               (+ base 150)
                               base)

                     new-y (+ (:y n) y-shift)

                     ;; Center global nodes horizontally
                     new-x (if is-global?
                             (- cluster-center (/ (:w n) 2))
                             (:x n))]
                 (assoc n :x new-x :y new-y)))
             initial-nodes)

        ;; Vertical Centering Strategy (Layer 2 Processing)
        ;; 1. Group by cluster
        clusters (group-by :swimlane-id (filter #(not (empty? (:swimlane-id %))) spaced-nodes))

        ;; 2. Calculate height of each cluster
        cluster-bounds
        (into {}
              (map (fn [[id nodes]]
                     (let [min-y (apply min (map :y nodes))
                           max-y (apply max (map #(+ (:y %) (:h %)) nodes))
                           height (- max-y min-y)]
                       [id {:min-y min-y :height height}]))
                   clusters))

        ;; 3. Find max height
        max-cluster-height (if (seq cluster-bounds)
                             (apply max (map (comp :height val) cluster-bounds))
                             0)

        ;; 4. Calculate shifts for centering
        centered-nodes
        (map (fn [n]
               (if (empty? (:swimlane-id n))
                 n ;; Global node, untouched
                 (let [id (:swimlane-id n)
                       {:keys [height]} (get cluster-bounds id)
                       shift (/ (- max-cluster-height height) 2)]
                   (update n :y + shift))))
             spaced-nodes)]

    centered-nodes))

(defn calculate-cluster-geometries [lanes processed-nodes]
  (keep (fn [l]
          (let [l-nodes (filter #(= (:swimlane-id %) (:id l)) processed-nodes)]
            (if (and (seq l-nodes) (not (empty? (:id l)))) ;; Skip global lane
              (let [min-x (apply min (map :x l-nodes))
                    max-x (apply max (map #(+ (:x %) (:w %)) l-nodes))
                    min-y (apply min (map :y l-nodes))
                    max-y (apply max (map #(+ (:y %) (:h %)) l-nodes))
                    padding 30]
                {:x (- min-x padding)
                 :y (- min-y padding)
                 :w (+ (- max-x min-x) (* 2 padding))
                 :h (+ (- max-y min-y) (* 2 padding))
                 :label (:label l)
                 :props (:props l)
                 :index (:index l)})
              nil)))
        lanes))
