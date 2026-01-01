(ns xflow.layout.routing.ortho
  (:require [xflow.layout.config :as config]
            [xflow.geometry :as geo]))

(defn- get-node-bounds [node padding]
  (let [x (or (:x node) 0)
        y (or (:y node) 0)
        w (or (:w node) 0)
        h (or (:h node) 0)]
    {:x1 (- x padding)
     :y1 (- y padding)
     :x2 (+ x w padding)
     :y2 (+ y h padding)}))

;; Replaced by xflow.geometry/distribute-points
(def distribute-points geo/distribute-points)

(defn- is-vertical? [direction]
  (let [d (keyword direction)]
    (or (= d :tb) (= d :bt) (= d :vertical))))

;; --- 智能防重叠 (Smart Overlap Prevention) ---

(defn- assign-segment-tracks [segments interval-key-start interval-key-end key-fn]
  ;; 简单的区间着色算法 / 轨道分配
  (let [sorted (sort-by interval-key-start segments)
        tracks (atom [])] ;; vector of end-positions
    (reduce (fn [results seg]
              (let [start (interval-key-start seg)
                    end (interval-key-end seg)
                    ;; 查找可用的轨道
                    track-idx (first (keep-indexed
                                      (fn [idx track-end]
                                        (if (<= track-end start) idx nil))
                                      @tracks))
                    final-idx (if track-idx
                                (do
                                  (swap! tracks assoc track-idx end)
                                  track-idx)
                                (do
                                  (swap! tracks conj end)
                                  (dec (count @tracks))))]
                (assoc results (key-fn seg) final-idx)))
            {}
            sorted)))

(defn- nudge-segments-smart [segments axis gap]
  ;; axis: :x or :y (current routing axis being nudged)
  (let [relevant-segs (filter #(= (:axis %) axis) segments)
        ;; Group by the coordinate on the OTHER axis
        other-axis (if (= axis :x) :y :x)
        grouped (group-by #(get-in % [:p1 other-axis]) relevant-segs)]

    (reduce-kv
     (fn [nudges fixed-coord segs]
       (if (<= (count segs) 1)
         nudges
         (let [;; Sort segments by their interval on the main axis
               sorted-segs (sort-by #(min (get-in % [:p1 axis]) (get-in % [:p2 axis])) segs)
                ;; Assign tracks/offsets
               track-assignments (assign-segment-tracks
                                  sorted-segs
                                  #(min (get-in % [:p1 axis]) (get-in % [:p2 axis]))
                                  #(max (get-in % [:p1 axis]) (get-in % [:p2 axis]))
                                  :id)
                ;; Calculate nudge amount based on track index
               max-track (apply max (vals track-assignments))
               total-width (* max-track gap)
               start-offset (- (/ total-width 2.0))]

           (reduce (fn [acc [edge-id track-idx]]
                     (let [offset (+ start-offset (* track-idx gap))]
                       (assoc acc edge-id offset)))
                   nudges
                   track-assignments))))
     {}
     grouped)))

(defn- extract-segments [routed-edges]
  ;; Extract all straight segments from routed edges
  (mapcat (fn [edge]
            (let [points (:points edge)
                  id (:id edge)]
              (map (fn [p1 p2]
                     (let [is-vertical (= (:x p1) (:x p2))
                           axis (if is-vertical :y :x)]
                       {:id id :p1 p1 :p2 p2 :axis axis}))
                   points
                   (rest points))))
          routed-edges))

(defn- apply-nudges [edges nudges direction]
  (let [vertical? (is-vertical? direction)
        ;; If vertical (TB), shift-axis is :x (shift vertical segments)
        shift-axis (if vertical? :x :y)]
    (map (fn [edge]
           (if-let [offset (get nudges (:id edge))]
             (let [points (:points edge)]
               (if (> (count points) 2)
                 (let [start (first points)
                       end (last points)
                       mid-points (subvec (vec points) 1 (dec (count points)))
                       shifted-mids (mapv (fn [p]
                                            (assoc p shift-axis (+ (get p shift-axis) offset)))
                                          mid-points)]
                   (assoc edge :points (into [start] (conj shifted-mids end))))
                 edge))
             edge))
         edges)))

;; --- 端口分配 (Port Assignment) ---

(defn- assign-ports [nodes edges options]
  (let [nodes-map (into {} (map (juxt :id identity) nodes))
        direction (:direction options :tb)
        vertical? (is-vertical? direction)]

    (when-let [n (some #(when (= (:id %) "Gateway") %) nodes)]
      (println "DEBUG: assign-ports Gateway:" (:x n) (:y n) (:w n) (:h n)))

    (reduce
     (fn [acc node]
       (let [node-id (:id node)
             my-inputs (filter #(= (:to %) node-id) edges)
             my-outputs (filter #(= (:from %) node-id) edges)

              ;; Sort edges to minimize crossings
             sort-fn (fn [e is-input?]
                       (let [other-id (if is-input? (:from e) (:to e))
                             other-node (get nodes-map other-id)]
                         (if vertical?
                           (or (:x other-node) 0)
                           (or (:y other-node) 0))))

             sorted-in (sort-by #(sort-fn % true) my-inputs)
             sorted-out (sort-by #(sort-fn % false) my-outputs)

              ;; Distribute ports
              ;; TB: Inputs Top, Outputs Bottom
             in-ports (distribute-points
                       (if vertical? (or (:x node) 0) (or (:y node) 0))
                       (if vertical? (or (:w node) 0) (or (:h node) 0))
                       (if vertical? (or (:y node) 0) (or (:x node) 0))
                       (count sorted-in)
                       (if vertical? :x :y))

             out-ports (distribute-points
                        (if vertical? (or (:x node) 0) (or (:y node) 0))
                        (if vertical? (or (:w node) 0) (or (:h node) 0))
                        (if vertical? (+ (or (:y node) 0) (or (:h node) 0)) (+ (or (:x node) 0) (or (:w node) 0)))
                        (count sorted-out)
                        (if vertical? :x :y))

              ;; Create map: edge-id -> port
             in-map (zipmap (map :id sorted-in) in-ports)
             out-map (zipmap (map :id sorted-out) out-ports)]

         (as-> acc m
           (reduce (fn [m2 [eid port]] (assoc-in m2 [eid :target] port)) m in-map)
           (reduce (fn [m2 [eid port]] (assoc-in m2 [eid :source] port)) m out-map))))
     {}
     nodes)))

;; --- 路径简化 (Path Simplification) ---

;; Replaced by xflow.geometry/collinear?

;; Replaced by xflow.geometry/simplify-points

;; --- 主路由逻辑 (Main Routing Logic) ---

(defn- check-node-intersection [p1 p2 node padding]
  (let [b (get-node-bounds node padding)
        ;; Segment bounds
        sx1 (min (:x p1) (:x p2))
        sx2 (max (:x p1) (:x p2))
        sy1 (min (:y p1) (:y p2))
        sy2 (max (:y p1) (:y p2))

        ;; Intersection check
        intersects? (and (< sx1 (:x2 b)) (> sx2 (:x1 b))
                         (< sy1 (:y2 b)) (> sy2 (:y1 b)))]
    intersects?))

(defn- find-safe-mid [p1 p2 nodes vertical? padding]
  (let [mid-val (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))
        fixed-axis (if vertical? :x :y) ;; The axis that is constant in the middle segment

        ;; Construct test segment for the "middle bar" of the Z-shape
        ;; If vertical layout (TB), middle bar is horizontal: (x1, mid-y) -> (x2, mid-y)
        ;; Wait, Z-shape: p1 -> (x1, mid-y) -> (x2, mid-y) -> p2
        ;; The middle segment is (x1, mid-y) -> (x2, mid-y)
        ;; This segment is Horizontal.
        ;; But the Vertical segments are p1->(x1, mid-y) and (x2, mid-y)->p2

        ;; We need to check collision for ALL 3 segments.
        ;; But usually, the middle bar is the one that crosses columns.
        ;; OR the vertical bars cross rows.

        ;; Let's try a few candidates for the middle split position.
        candidates (concat [mid-val]
                           (map (fn [n]
                                  (if vertical?
                                    (+ (or (:y n) 0) (or (:h n) 0) padding) ;; Below node
                                    (+ (or (:x n) 0) (or (:w n) 0) padding))) ;; Right of node
                                nodes)
                           (map (fn [n]
                                  (if vertical?
                                    (- (or (:y n) 0) padding) ;; Above node
                                    (- (or (:x n) 0) padding))) ;; Left of node
                                nodes))

        ;; Filter candidates that are reasonable (between start and end roughly, or slightly outside)
        valid-candidates (take 10 (distinct candidates))] ;; Limit to 10 tries for perf

    (or (first (filter (fn [val]
                         (let [;; Construct 3 segments based on this split value
                               m1 (if vertical? {:x (:x p1) :y val} {:x val :y (:y p1)})
                               m2 (if vertical? {:x (:x p2) :y val} {:x val :y (:y p2)})

                               s1 [p1 m1]
                               s2 [m1 m2]
                               s3 [m2 p2]

                               ;; Check if ANY segment intersects ANY node
                               collision? (some (fn [node]
                                                  (or (check-node-intersection (first s1) (second s1) node padding)
                                                      (check-node-intersection (first s2) (second s2) node padding)
                                                      (check-node-intersection (first s3) (second s3) node padding)))
                                                nodes)]
                           (not collision?)))
                       valid-candidates))
        mid-val))) ;; Fallback to default mid

(defn- check-path-intersection [points nodes padding]
  (some (fn [node]
          (some (fn [i]
                  (let [p1 (nth points i)
                        p2 (nth points (inc i))]
                    (check-node-intersection p1 p2 node padding)))
                (range (dec (count points)))))
        nodes))

(defn- generate-z-path [p1 p2 mid vertical?]
  (if vertical?
    [p1 {:x (:x p1) :y mid} {:x (:x p2) :y mid} p2]
    [p1 {:x mid :y (:y p1)} {:x mid :y (:y p2)} p2]))

(defn- find-safe-z-path [p1 p2 nodes vertical? padding]
  (let [mid-val (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))
        candidates (concat [mid-val]
                           (map (fn [n]
                                  (if vertical?
                                    (+ (or (:y n) 0) (or (:h n) 0) padding) ;; Below node
                                    (+ (or (:x n) 0) (or (:w n) 0) padding))) ;; Right of node
                                nodes)
                           (map (fn [n]
                                  (if vertical?
                                    (- (or (:y n) 0) padding) ;; Above node
                                    (- (or (:x n) 0) padding))) ;; Left of node
                                nodes))

        valid-candidates (take 15 (distinct candidates))]

    (first (keep (fn [val]
                   (let [path (generate-z-path p1 p2 val vertical?)]
                     (if (not (check-path-intersection path nodes padding))
                       path
                       nil)))
                 valid-candidates))))

(defn- generate-detour-path [p1 p2 detour-val vertical? padding]
  ;; Detour path (C-shape / U-shape logic)
  ;; For TB (vertical):
  ;; p1 -> (x1, y1+pad) -> (detour, y1+pad) -> (detour, y2-pad) -> (x2, y2-pad) -> p2
  ;; This assumes p1 is Top/Bottom.
  ;; If we just want a generic detour:
  ;; We move orthogonal to direction first?

  (let [eps padding]
    (if vertical?
      ;; Vertical Layout (TB)
      (let [y1-shift (if (< (:y p1) (:y p2)) (+ (:y p1) eps) (- (:y p1) eps))
            y2-shift (if (< (:y p1) (:y p2)) (- (:y p2) eps) (+ (:y p2) eps))
            ;; But we should verify if y1-shift and y2-shift make sense relative to p1/p2 direction
            ;; Let's just use fixed small offsets from p1/p2
            y1-m (+ (:y p1) (if (< (:y p2) (:y p1)) (- eps) eps))
            y2-m (- (:y p2) (if (< (:y p2) (:y p1)) (- eps) eps))]

        [p1
         {:x (:x p1) :y y1-m}
         {:x detour-val :y y1-m}
         {:x detour-val :y y2-m}
         {:x (:x p2) :y y2-m}
         p2])

      ;; Horizontal Layout (LR)
      (let [x1-m (+ (:x p1) (if (< (:x p2) (:x p1)) (- eps) eps))
            x2-m (- (:x p2) (if (< (:x p2) (:x p1)) (- eps) eps))]
        [p1
         {:x x1-m :y (:y p1)}
         {:x x1-m :y detour-val}
         {:x x2-m :y detour-val}
         {:x x2-m :y (:y p2)}
         p2]))))

(defn- find-safe-detour-path [p1 p2 nodes vertical? padding]
  (let [;; Try detour values on the cross-axis
        ;; For TB, cross-axis is X.
        base-val (if vertical? (:x p1) (:y p1))

        ;; Candidates: Right/Left (or Down/Up) of nodes, plus some fixed offsets
        candidates (concat
                    [(+ base-val 40) (- base-val 40) (+ base-val 80) (- base-val 80)]
                    (map (fn [n]
                           (if vertical?
                             (+ (or (:x n) 0) (or (:w n) 0) padding) ;; Right of node
                             (+ (or (:y n) 0) (or (:h n) 0) padding))) ;; Below node
                         nodes)
                    (map (fn [n]
                           (if vertical?
                             (- (or (:x n) 0) padding) ;; Left of node
                             (- (or (:y n) 0) padding))) ;; Above node
                         nodes))

        valid-candidates (take 20 (distinct candidates))]

    (first (keep (fn [val]
                   (let [path (generate-detour-path p1 p2 val vertical? padding)]
                     (if (not (check-path-intersection path nodes padding))
                       path
                       nil)))
                 valid-candidates))))

(defn- route-segment-smart [p1 p2 direction nodes]
  (let [vertical? (is-vertical? direction)
        padding 15

        ;; 1. Try simple Z-path
        z-path (find-safe-z-path p1 p2 nodes vertical? padding)

        ;; 2. If failed, try Detour path
        final-path (or z-path
                       (find-safe-detour-path p1 p2 nodes vertical? padding)
                       ;; 3. Fallback to direct Z-path (even if collides)
                       (let [mid (if vertical? (/ (+ (:y p1) (:y p2)) 2.0) (/ (+ (:x p1) (:x p2)) 2.0))]
                         (generate-z-path p1 p2 mid vertical?)))]
    final-path))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        config (config/resolve-config options)
        direction (:direction options :tb)
        _ (println "DEBUG: route-edges direction=" direction " type=" (type direction) " is-vertical=" (is-vertical? direction))

        ;; 1. Assign ports
        port-assignments (assign-ports nodes edges options)

        ;; 2. Initial routing with Obstacle Avoidance
        routed-edges
        (map (fn [edge]
               (let [source-port (get-in port-assignments [(:id edge) :source])
                     target-port (get-in port-assignments [(:id edge) :target])]
                 (if (and source-port target-port)
                   (let [;; Exclude source and target nodes from obstacle checks to avoid self-collision at ports
                         relevant-nodes (filter #(and (not= (:id %) (:from edge))
                                                      (not= (:id %) (:to edge)))
                                                nodes)
                         points (route-segment-smart source-port target-port direction relevant-nodes)
                         simplified (geo/simplify-points points)]
                     (assoc edge :points simplified))
                   edge))) ;; Keep original if ports not found
             edges)

        ;; 3. Nudging (Optimization)
        ;; In TB, we nudge vertical segments (Y-segments) to avoid overlap in X.
        ;; axis=:y means we look at Y-segments.
        axis (if (is-vertical? direction) :y :x)
        nudges (nudge-segments-smart (extract-segments routed-edges) axis 10)

        final-edges (apply-nudges routed-edges nudges direction)]

    (assoc layout :edges final-edges)))
