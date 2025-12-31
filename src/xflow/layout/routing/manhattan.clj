(ns xflow.layout.routing.manhattan
  (:require [clojure.set :as set]))

(defn- intersects-node? [segment node]
  (let [{:keys [x1 y1 x2 y2]} segment
        {:keys [x y w h]} node]
    (if (and (number? x1) (number? y1) (number? x2) (number? y2)
             (number? x) (number? y) (number? w) (number? h))
      (let [pad 10
            nx (- x pad)
            ny (- y pad)
            nw (+ w (* 2 pad))
            nh (+ h (* 2 pad))]
        (if (= x1 x2)
          (and (>= x1 nx)
               (<= x1 (+ nx nw))
               (or (and (>= y1 ny) (<= y1 (+ ny nh)))
                   (and (>= y2 ny) (<= y2 (+ ny nh)))
                   (and (<= y1 ny) (>= y2 (+ ny nh)))))
          (and (>= y1 ny)
               (<= y1 (+ ny nh))
               (or (and (>= x1 nx) (<= x1 (+ nx nw)))
                   (and (>= x2 nx) (<= x2 (+ nx nw)))
                   (and (<= x1 nx) (>= x2 (+ nx nw)))))))
      false)))

(defn- find-safe-mid [p1 p2 nodes mode]
  (let [lr? (= mode "horizontal")
        start-mid (if lr? (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2))
        seg-min (if lr? (min (:y p1) (:y p2)) (min (:x p1) (:x p2)))
        seg-max (if lr? (max (:y p1) (:y p2)) (max (:x p1) (:x p2)))
        check (fn [mid]
                (let [segment (if lr?
                                {:x1 mid :x2 mid :y1 seg-min :y2 seg-max}
                                {:x1 seg-min :x2 seg-max :y1 mid :y2 mid})]
                  (some #(intersects-node? segment %) nodes)))]
    (if (not (check start-mid))
      start-mid
      (loop [offset 20]
        (if (> offset 200)
          (do
            (when (and (= seg-min 20.0) (= seg-max 275.0))
              (println "DEBUG: find-safe-mid gave up for" p1 p2 "start-mid" start-mid))
            start-mid)
          (let [try-pos (+ start-mid offset)
                try-neg (- start-mid offset)]
            (cond
              (not (check try-pos)) try-pos
              (not (check try-neg))
              (do
                (when (and (= seg-min 20.0) (= seg-max 275.0))
                  (println "DEBUG: find-safe-mid FOUND" try-neg))
                try-neg)
              :else (recur (+ offset 20)))))))))

(defn- find-safe-channel [p1 p2 nodes mode side]
  ;; Finds a safe channel for side-loop
  ;; mode: "horizontal" (LR) or "vertical" (TB)
  ;; side: :left, :right, :top, :bottom

  (let [vertical? (not= mode "horizontal")

        ;; Determine coordinate accessors and search direction
        val-fn (if vertical? :x :y)
        min-fn min
        max-fn max

        ;; Base coordinate to start searching from
        ;; For Left/Top: min - 30
        ;; For Right/Bottom: max + 30
        start-val (case side
                    :left (- (min (:x p1) (:x p2)) 30)
                    :right (+ (max (+ (:x p1) (:w p1 0)) (+ (:x p2) (:w p2 0))) 30)
                    :top (- (min (:y p1) (:y p2)) 30)
                    :bottom (+ (max (+ (:y p1) (:h p1 0)) (+ (:y p2) (:h p2 0))) 30)
                    0)

        ;; Segment bounds (orthogonal to search direction)
        seg-min (if vertical? (min (:y p1) (:y p2)) (min (:x p1) (:x p2)))
        seg-max (if vertical? (max (:y p1) (:y p2)) (max (:x p1) (:x p2)))

        ;; Search increment (+20 or -20)
        increment (if (contains? #{:right :bottom} side) 20 -20)

        check (fn [val]
                (let [segment (if vertical?
                                {:x1 val :x2 val :y1 seg-min :y2 seg-max} ;; Vertical line
                                {:x1 seg-min :x2 seg-max :y1 val :y2 val})] ;; Horizontal line
                  (some #(intersects-node? segment %) nodes)))]

    (loop [offset 0]
      (if (> offset 200)
        start-val
        (let [try-pos (+ start-val (* offset (/ increment 20)))] ;; offset is always +ve in loop
          (if (not (check try-pos))
            try-pos
            (recur (+ offset 20))))))))

(defn- path-valid? [points nodes]
  (every? (fn [[p1 p2]]
            (let [segment {:x1 (:x p1) :y1 (:y p1) :x2 (:x p2) :y2 (:y p2)}
                  ;; Helper to check if a point is inside a node (including padding)
                  point-in-node? (fn [p n]
                                   (intersects-node? {:x1 (:x p) :y1 (:y p) :x2 (:x p) :y2 (:y p)} n))

                  ;; Filter out nodes that contain the start or end point of the segment.
                  ;; This is crucial because a segment naturally touches the node it connects to,
                  ;; but this shouldn't count as a "collision" that invalidates the path.
                  ;; 过滤掉包含线段起点或终点的节点。这是因为线段自然会连接到节点，
                  ;; 但这不应被视为导致路径无效的“碰撞”。
                  relevant-nodes (remove (fn [n]
                                           (or (point-in-node? p1 n)
                                               (point-in-node? p2 n)))
                                         nodes)]
              (not-any? #(intersects-node? segment %) relevant-nodes)))
          (partition 2 1 points)))

(defn- route-segment [p1 p2 nodes mode]
  (let [vertical? (not= mode "horizontal")
        ;; Try 1: Standard Mid
        mid-std (if vertical? (/ (+ (:y p1) (:y p2)) 2) (/ (+ (:x p1) (:x p2)) 2))
        pts-std (if vertical?
                  [p1 {:x (:x p1) :y mid-std} {:x (:x p2) :y mid-std} p2]
                  [p1 {:x mid-std :y (:y p1)} {:x mid-std :y (:y p2)} p2])
        valid-std? (path-valid? pts-std nodes)

        ;; Try 2: Safe Mid (Search)
        mid-safe (find-safe-mid p1 p2 nodes mode)
        pts-safe (if vertical?
                   [p1 {:x (:x p1) :y mid-safe} {:x (:x p2) :y mid-safe} p2]
                   [p1 {:x mid-safe :y (:y p1)} {:x mid-safe :y (:y p2)} p2])
        valid-safe? (path-valid? pts-safe nodes)

        ;; Try 3: Side Loop
        side (if vertical?
               (if (< (:x p1) (:x p2)) :left :right)
               (if (< (:y p1) (:y p2)) :top :bottom))
        channel (find-safe-channel p1 p2 nodes mode side)
        pts-loop (if vertical?
                   (if (= side :right)
                     [p1 {:x channel :y (:y p1)} {:x channel :y (:y p2)} p2]
                     [p1 {:x channel :y (:y p1)} {:x channel :y (:y p2)} p2])
                   (if (= side :bottom)
                     [p1 {:x (:x p1) :y channel} {:x (:x p2) :y channel} p2]
                     [p1 {:x (:x p1) :y channel} {:x (:x p2) :y channel} p2]))]

    (cond
      valid-std? pts-std
      valid-safe? pts-safe
      :else pts-loop)))

(defn select-ports [n1 n2 mode]
  (let [vertical? (not= mode "horizontal")
        margin 20
        x1 (double (or (:x n1) 0.0))
        y1 (double (or (:y n1) 0.0))
        w1 (double (or (:w n1) 0.0))
        h1 (double (or (:h n1) 0.0))
        x2 (double (or (:x n2) 0.0))
        y2 (double (or (:y n2) 0.0))
        w2 (double (or (:w n2) 0.0))
        h2 (double (or (:h n2) 0.0))
        res (if vertical?
              (let [s-bottom (+ y1 h1)
                    t-top y2
                    enough-vertical? (> t-top (+ s-bottom margin))]
                (if enough-vertical?
                  {:p1 {:x (+ x1 (/ w1 2.0)) :y s-bottom}
                   :p2 {:x (+ x2 (/ w2 2.0)) :y t-top}
                   :strategy :standard}
                  (let [s-right (+ x1 w1)
                        t-left x2
                        s-left x1
                        t-right (+ x2 w2)]
                    (cond
                      (> t-left (+ s-right margin))
                      {:p1 {:x s-right :y (+ y1 (/ h1 2.0))}
                       :p2 {:x t-left :y (+ y2 (/ h2 2.0))}
                       :strategy :direct-horizontal}

                      (> s-left (+ t-right margin))
                      {:p1 {:x s-left :y (+ y1 (/ h1 2.0))}
                       :p2 {:x t-right :y (+ y2 (/ h2 2.0))}
                       :strategy :direct-horizontal}

                      :else
                      (let [cx1 (+ x1 (/ w1 2.0))
                            cx2 (+ x2 (/ w2 2.0))
                            side (if (> cx1 cx2) :right :left)]
                        {:p1 (if (= side :left)
                               {:x x1 :y (+ y1 (/ h1 2.0))}
                               {:x (+ x1 w1) :y (+ y1 (/ h1 2.0))})
                         :p2 (if (= side :left)
                               {:x x2 :y (+ y2 (/ h2 2.0))}
                               {:x (+ x2 w2) :y (+ y2 (/ h2 2.0))})
                         :strategy :side-loop
                         :side side})))))
              (let [s-right (+ x1 w1)
                    t-left x2
                    enough-horizontal? (> t-left (+ s-right margin))]
                (if enough-horizontal?
                  {:p1 {:x s-right :y (+ y1 (/ h1 2.0))}
                   :p2 {:x t-left :y (+ y2 (/ h2 2.0))}
                   :strategy :standard}
                  (let [s-bottom (+ y1 h1)
                        t-top y2
                        s-top y1
                        t-bottom (+ y2 h2)]
                    (cond
                      (> t-top (+ s-bottom margin))
                      {:p1 {:x (+ x1 (/ w1 2.0)) :y s-bottom}
                       :p2 {:x (+ x2 (/ w2 2.0)) :y t-top}
                       :strategy :direct-vertical}

                      (> s-top (+ t-bottom margin))
                      {:p1 {:x (+ x1 (/ w1 2.0)) :y s-top}
                       :p2 {:x (+ x2 (/ w2 2.0)) :y t-bottom}
                       :strategy :direct-vertical}

                      :else
                      (let [cy1 (+ y1 (/ h1 2.0))
                            cy2 (+ y2 (/ h2 2.0))
                            side (if (> cy1 cy2) :bottom :top)]
                        {:p1 (if (= side :top)
                               {:x (+ x1 (/ w1 2.0)) :y y1}
                               {:x (+ x1 (/ w1 2.0)) :y (+ y1 h1)})
                         :p2 (if (= side :top)
                               {:x (+ x2 (/ w2 2.0)) :y y2}
                               {:x (+ x2 (/ w2 2.0)) :y (+ y2 h2)})
                         :strategy :side-loop
                         :side side}))))))]
    res))

(defn- collinear? [p1 p2 p3]
  (let [x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)
        x3 (:x p3) y3 (:y p3)]
    ;; Check if slopes are equal: (y2-y1)/(x2-x1) == (y3-y2)/(x3-x2)
    ;; Use cross product to avoid division by zero: (y2-y1)*(x3-x2) == (y3-y2)*(x2-x1)
    (< (Math/abs (- (* (- y2 y1) (- x3 x2))
                    (* (- y3 y2) (- x2 x1))))
       0.001)))

(defn- simplify-path [points]
  (if (< (count points) 3)
    points
    (loop [pts points
           result [(first points)]]
      (if (empty? (rest (rest pts)))
        (conj result (second pts))
        (let [p1 (last result)
              p2 (first (rest pts))
              p3 (second (rest pts))]
          (if (collinear? p1 p2 p3)
            ;; Skip p2, continue with p1, p3, ...
            (recur (rest pts) result)
            ;; Keep p2
            (recur (rest pts) (conj result p2))))))))

(defn- to-double [p]
  (when (or (nil? (:x p)) (nil? (:y p)))
    (println "ERROR: Nil coord in point:" p))
  {:x (double (:x p)) :y (double (:y p))})

(defn- clean-points [pts]
  (->> pts
       (map to-double)
       dedupe
       vec
       simplify-path))

(defn- simple-path? [points mode]
  (if (< (count points) 3)
    true
    (let [vertical? (not= mode "horizontal")
          m1 (second points)
          m2 (nth points 2)]
      (if vertical?
        (= (:y m1) (:y m2)) ;; Horizontal middle segment in Vertical mode
        (= (:x m1) (:x m2)))))) ;; Vertical middle segment in Horizontal mode

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Determine mode:
        ;; 1. If Layout is Swimlane, prioritize swimlane-mode (default horizontal)
        ;; 2. Otherwise use direction (tb=vertical, lr=horizontal)
        layout-type (:layout options)
        direction (:direction options)
        swimlane-mode (:swimlane-mode options "horizontal")

        mode (cond
               (= layout-type "swimlane")
               (if (= swimlane-mode "vertical") "vertical" "horizontal")

               (= direction "tb") "vertical"
               (= direction "lr") "horizontal"
               :else "vertical")

        ;; Group edges by from node to handle branching overlaps
        edges-by-from (group-by :from (:edges layout))]

    (update layout :edges
            (fn [_]
              (into []
                    (mapcat (fn [[_ siblings]]
                              (let [edge-geoms
                                    (map (fn [e]
                                           (let [n1 (get nodes-map (:from e))
                                                 n2 (get nodes-map (:to e))]
                                             (if (and n1 n2)
                                               (let [{:keys [p1 p2 strategy side]} (select-ports n1 n2 mode)]
                                                 {:e e :p1 p1 :p2 p2 :strategy strategy :side side :valid true})
                                               {:e e :valid false})))
                                         siblings)

                                    valid-geoms (filter :valid edge-geoms)

                                    ;; --- 路径点路由 (处理 Dummy Nodes 产生的长连线) ---
                                    waypoint-results
                                    (keep (fn [geom]
                                            (let [{:keys [e p1 p2]} geom]
                                              (when (seq (:points e))
                                                (let [waypoints (:points e)
                                                      vertical? (not= mode "horizontal")
                                                      preferred-side (if vertical? :right :bottom)
                                                      channel (find-safe-channel p1 p2 nodes mode preferred-side)
                                                      preferred-path (if vertical?
                                                                       [p1 {:x channel :y (:y p1)} {:x channel :y (:y p2)} p2]
                                                                       [p1 {:x (:x p1) :y channel} {:x (:x p2) :y channel} p2])
                                                      preferred (clean-points preferred-path)
                                                      ;; Try direct routing first to see if we can avoid zigzag
                                                      direct-path (route-segment p1 p2 nodes mode)
                                                      direct (clean-points direct-path)]

                                                  (cond
                                                    (path-valid? preferred nodes)
                                                    (assoc e :points preferred)

                                                    (path-valid? direct nodes)
                                                    (assoc e :points direct)

                                                    :else
                                                    (let [full-path (concat [p1] waypoints [p2])
                                                          final-points
                                                          (loop [pts full-path
                                                                 result []]
                                                            (if (< (count pts) 2)
                                                              result
                                                              (let [curr (first pts)
                                                                    next (second pts)
                                                                    segment-pts (route-segment curr next nodes mode)]
                                                                (recur (rest pts)
                                                                       (into result (if (empty? result)
                                                                                      segment-pts
                                                                                      (rest segment-pts)))))))]
                                                      (assoc e :points (clean-points final-points))))))))
                                          valid-geoms)

                                    ;; 过滤掉已通过 Waypoint 路由的边
                                    remaining-geoms (remove (fn [g] (seq (:points (:e g)))) valid-geoms)

                                    ;; --- 标准路由 (无中间点) ---
                                    standard-geoms (filter #(= (:strategy %) :standard) remaining-geoms)
                                    direct-horiz-geoms (filter #(= (:strategy %) :direct-horizontal) remaining-geoms)
                                    direct-vert-geoms (filter #(= (:strategy %) :direct-vertical) remaining-geoms)
                                    side-loop-geoms (filter #(= (:strategy %) :side-loop) remaining-geoms)

                                    ;; --- Helper for Standard Routing ---
                                    route-standard (fn [geoms routing-mode]
                                                     (if (seq geoms)
                                                       (let [mid-xs (map #(/ (+ (:x (:p1 %)) (:x (:p2 %))) 2) geoms)
                                                             mid-ys (map #(/ (+ (:y (:p1 %)) (:y (:p2 %))) 2) geoms)
                                                             avg-mid-x (/ (reduce + mid-xs) (count mid-xs))
                                                             avg-mid-y (/ (reduce + mid-ys) (count mid-ys))

                                                             bundle-min (if (= routing-mode "horizontal")
                                                                          (apply min (map #(min (:y (:p1 %)) (:y (:p2 %))) geoms))
                                                                          (apply min (map #(min (:x (:p1 %)) (:x (:p2 %))) geoms)))
                                                             bundle-max (if (= routing-mode "horizontal")
                                                                          (apply max (map #(max (:y (:p1 %)) (:y (:p2 %))) geoms))
                                                                          (apply max (map #(max (:x (:p1 %)) (:x (:p2 %))) geoms)))

                                                             safe-mid (if (= routing-mode "horizontal")
                                                                        (find-safe-mid {:x avg-mid-x :y bundle-min} {:x avg-mid-x :y bundle-max} nodes routing-mode)
                                                                        (find-safe-mid {:x bundle-min :y avg-mid-y} {:x bundle-max :y avg-mid-y} nodes routing-mode))]

                                                         (map (fn [geom]
                                                                (let [{:keys [e p1 p2]} geom]
                                                                  (assoc e :points
                                                                         (clean-points
                                                                          (if (= routing-mode "horizontal")
                                                                            [p1
                                                                             {:x safe-mid :y (:y p1)}
                                                                             {:x safe-mid :y (:y p2)}
                                                                             p2]
                                                                            [p1
                                                                             {:x (:x p1) :y safe-mid}
                                                                             {:x (:x p2) :y safe-mid}
                                                                             p2])))))
                                                              geoms))
                                                       []))

                                    standard-results (route-standard standard-geoms mode)
                                    direct-horiz-results (route-standard direct-horiz-geoms "horizontal")
                                    direct-vert-results (route-standard direct-vert-geoms "vertical")

                                    ;; --- 侧边回环路由 (Side Loop Routing) - 用于回退边 ---
                                    side-loop-results
                                    (map (fn [geom]
                                           (let [{:keys [e p1 p2 side]} geom
                                                 channel (find-safe-channel p1 p2 nodes mode side)]
                                             (assoc e :points
                                                    (clean-points
                                                     (if (not= mode "horizontal") ;; TB
                                                       ;; TB Layout
                                                       (if (= side :right)
                                                          ;; Right Loop
                                                         [p1
                                                          {:x channel :y (:y p1)}
                                                          {:x channel :y (:y p2)}
                                                          p2]
                                                          ;; Left Loop (Default)
                                                         [p1
                                                          {:x channel :y (:y p1)}
                                                          {:x channel :y (:y p2)}
                                                          p2])

                                                       ;; LR Layout
                                                       (if (= side :bottom)
                                                          ;; Bottom Loop
                                                         [p1
                                                          {:x (:x p1) :y channel}
                                                          {:x (:x p2) :y channel}
                                                          p2]
                                                          ;; Top Loop (Default)
                                                         [p1
                                                          {:x (:x p1) :y channel}
                                                          {:x (:x p2) :y channel}
                                                          p2]))))))
                                         side-loop-geoms)]

                                (concat waypoint-results standard-results direct-horiz-results direct-vert-results side-loop-results)))
                            edges-by-from))))))
