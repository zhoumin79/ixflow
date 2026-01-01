(ns xflow.layout.coordinate
  (:require [xflow.layout.config :as config]))

;; --- Helper Functions ---

(defn- get-layers [nodes]
  (->> nodes
       (group-by :rank)
       (sort-by key)
       (map val)
       (mapv #(sort-by :order %))))

(defn- get-node-width [n] (or (:w n) 0))
(defn- get-node-height [n] (or (:h n) 0))

;; --- Iterative Coordinate Solver (Functional) ---

(defn- calculate-y-coords [layers rank-sep]
  "Calculates Y coordinates (ranks) based on layer heights."
  (first
   (reduce
    (fn [[acc current-y] layer]
      (let [layer-h (if (seq layer) (apply max (map get-node-height layer)) 0)
            next-y (+ current-y layer-h rank-sep)
            ;; Assign same Y to all nodes in layer
            layer-y-map (reduce (fn [m n] (assoc m (:id n) current-y)) {} layer)]
        [(merge acc layer-y-map) next-y]))
    [{} 0]
    layers)))

(defn- initial-packing [layers node-sep]
  "Performs initial compact packing of nodes."
  (reduce
   (fn [coords layer]
     (loop [nodes (sort-by :order layer)
            x 0
            layer-coords coords]
       (if (empty? nodes)
         layer-coords
         (let [n (first nodes)
               w (get-node-width n)
               next-x (+ x w node-sep)]
           (recur (rest nodes)
                  next-x
                  (assoc layer-coords (:id n) x))))))
   {}
   layers))

(defn- get-neighbor-coords [node edges x-coords direction]
  "Gets X coordinates of neighbors (parents for down sweep, children for up sweep)."
  (let [neighbors (if (= direction :down)
                    (filter #(= (:to %) (:id node)) edges) ;; Parents (incoming edges)
                    (filter #(= (:from %) (:id node)) edges)) ;; Children (outgoing edges)
        neighbor-ids (map (if (= direction :down) :from :to) neighbors)]
    (keep #(get x-coords %) neighbor-ids)))

(defn- constrain-x [x target-x node layer x-coords node-sep]
  "Constrains the target X coordinate to ensure no overlap with siblings."
  (let [order (:order node)
        prev-sibling (first (filter #(= (:order %) (dec order)) layer))
        next-sibling (first (filter #(= (:order %) (inc order)) layer))

        min-x (if prev-sibling
                (+ (get x-coords (:id prev-sibling) -100000) (get-node-width prev-sibling) node-sep)
                -100000)
        max-x (if next-sibling
                (- (get x-coords (:id next-sibling) 100000) (get-node-width node) node-sep)
                100000)]
    (max min-x (min max-x target-x))))

(defn- sweep-layer [layer x-coords edges direction node-sep]
  "Relaxes a single layer based on neighbors in the specified direction."
  ;; For down sweep, we process from left to right (order 0 -> N)
  ;; For constraints to work best, we might want to respect the previous placement
  ;; But pure functional update means we use 'x-coords' which might be stale within the layer
  ;; if we don't update it incrementally.
  ;; So we use reduce over the layer to propagate changes within the layer (like siblings pushing each other).
  (reduce
   (fn [current-coords node]
     (let [neighbor-xs (get-neighbor-coords node edges x-coords direction)]
       (if (seq neighbor-xs)
         (let [target-x (/ (reduce + neighbor-xs) (count neighbor-xs))
               current-x (get current-coords (:id node) 0)
               new-x (constrain-x current-x target-x node layer current-coords node-sep)]
           (assoc current-coords (:id node) new-x))
         current-coords))) ;; No neighbors, keep position
   x-coords
   (sort-by :order layer)))

(defn- iterative-relaxation [layers edges initial-coords iterations node-sep]
  "Performs iterative sweeps to straighten edges."
  (loop [i 0
         coords initial-coords]
    (if (>= i iterations)
      coords
      (let [;; Down Sweep (Rank 1 -> N)
            coords-down (reduce
                         (fn [acc-coords layer]
                           (sweep-layer layer acc-coords edges :down node-sep))
                         coords
                         (rest layers)) ;; Skip rank 0

            ;; Up Sweep (Rank N-1 -> 0)
            coords-up (reduce
                       (fn [acc-coords layer]
                         (sweep-layer layer acc-coords edges :up node-sep))
                       coords-down
                       (reverse (drop-last layers)))]
        (recur (inc i) coords-up)))))

(defn- iterative-layout [layers edges config]
  (let [node-sep (or (:node-sep config) 50)
        rank-sep (or (:rank-sep config) 50)
        iterations 4

        ;; 1. Y Coordinates
        y-map (calculate-y-coords layers rank-sep)

        ;; 2. Initial X Coordinates
        initial-x-map (initial-packing layers node-sep)

        ;; 3. Relaxation
        final-x-map (iterative-relaxation layers edges initial-x-map iterations node-sep)]

    ;; Assemble final nodes
    (mapcat (fn [layer]
              (map (fn [n]
                     (assoc n
                            :x (get final-x-map (:id n) 0)
                            :y (get y-map (:id n) 0)))
                   layer))
            layers)))

(defn- center-nodes [nodes]
  (if (empty? nodes)
    nodes
    (let [xs (keep :x nodes)]
      (if (empty? xs)
        nodes
        (let [min-x (apply min xs)
              shift (- min-x)]
          (map (fn [n]
                 (if (:x n)
                   (update n :x + shift)
                   n))
               nodes))))))

(defn assign-coordinates [ordered-nodes edges options]
  (let [config (config/resolve-config options)
        layers (get-layers ordered-nodes)

        ;; 使用纯函数式迭代求解器
        ;; 1. 初始打包 (Initial Packing)
        ;; 2. 迭代松弛 (Iterative Relaxation)
        ;; 3. 约束求解 (Constraint Solving)
        final-nodes (iterative-layout layers edges config)

        ;; 归一化 (Normalization)
        normalized (center-nodes final-nodes)

        ;; 计算总尺寸 (Calculate Total Dimensions)
        ;; 增加空值检查，防止 NPE
        width (if (seq normalized)
                (apply max (map #(+ (:x %) (get-node-width %)) normalized))
                0)
        height (if (seq normalized)
                 (apply max (map #(+ (:y %) (get-node-height %)) normalized))
                 0)]

    {:nodes normalized
     :edges edges
     :width width
     :height height}))
