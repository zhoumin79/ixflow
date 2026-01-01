(ns xflow.layout.strategy.cluster
  (:require [xflow.geometry :as geo]))

(defn process-layout [initial-nodes]
  (let [;; Separate global nodes (empty swimlane-id)
        global-nodes (filter #(empty? (:swimlane-id %)) initial-nodes)
        cluster-nodes (remove #(empty? (:swimlane-id %)) initial-nodes)

        ;; Calculate actual content width of clusters
        cluster-bounds (geo/bounding-box cluster-nodes)
        cluster-center (if cluster-bounds
                         (/ (+ (:min-x cluster-bounds) (:max-x cluster-bounds)) 2)
                         0)

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
        cluster-bounds-map
        (into {}
              (map (fn [[id nodes]]
                     (let [bounds (geo/bounding-box nodes)
                           height (- (:max-y bounds) (:min-y bounds))]
                       [id {:min-y (:min-y bounds) :height height}]))
                   clusters))

        ;; 3. Find max height
        max-cluster-height (if (seq cluster-bounds-map)
                             (apply max (map (comp :height val) cluster-bounds-map))
                             0)

        ;; 4. Calculate shifts for centering
        centered-nodes
        (map (fn [n]
               (if (empty? (:swimlane-id n))
                 n ;; Global node, untouched
                 (let [id (:swimlane-id n)
                       {:keys [height]} (get cluster-bounds-map id)
                       shift (/ (- max-cluster-height height) 2)]
                   (update n :y + shift))))
             spaced-nodes)]

    centered-nodes))

(defn calculate-cluster-geometries [lanes processed-nodes]
  (keep (fn [l]
          (let [l-nodes (filter #(and (= (:swimlane-id %) (:id l))
                                      (not (:dummy? %))) ;; Exclude dummy nodes from cluster box
                                processed-nodes)]
            (if (and (seq l-nodes) (not (empty? (:id l)))) ;; Skip global lane
              (let [bounds (geo/bounding-box l-nodes)
                    padding 30]
                {:x (- (:min-x bounds) padding)
                 :y (- (:min-y bounds) padding)
                 :w (+ (- (:max-x bounds) (:min-x bounds)) (* 2 padding))
                 :h (+ (- (:max-y bounds) (:min-y bounds)) (* 2 padding))
                 :label (:label l)
                 :props (:props l)
                 :index (:index l)})
              nil)))
        lanes))

(defn layout [nodes edges pools options]
  {:nodes nodes :edges edges :width 100 :height 100})
