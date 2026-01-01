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
                   ;; Spanning check: handles both y1 < y2 and y1 > y2
                   (and (<= (min y1 y2) ny) (>= (max y1 y2) (+ ny nh)))))
          (and (>= y1 ny)
               (<= y1 (+ ny nh))
               (or (and (>= x1 nx) (<= x1 (+ nx nw)))
                   (and (>= x2 nx) (<= x2 (+ nx nw)))
                   ;; Spanning check: handles both x1 < x2 and x1 > x2
                   (and (<= (min x1 x2) nx) (>= (max x1 x2) (+ nx nw)))))))
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
          start-mid
          (let [try-pos (+ start-mid offset)
                try-neg (- start-mid offset)]
            (cond
              (not (check try-pos)) try-pos
              (not (check try-neg)) try-neg
              :else (recur (+ offset 20)))))))))

(defn- find-safe-channel [p1 p2 nodes mode side]
  ;; Finds a safe channel for side-loop
  ;; mode: "horizontal" (LR) or "vertical" (TB)
  ;; side: :left, :right, :top, :bottom

  (let [vertical? (not= mode "horizontal")

        ;; Determine coordinate accessors and search direction
        ;; Scan ALL nodes to find the true boundary to avoid cutting through wider nodes
        all-bounds (map (fn [n]
                          (case side
                            :right (+ (double (:x n)) (double (:w n)))
                            :left (double (:x n))
                            :bottom (+ (double (:y n)) (double (:h n)))
                            :top (double (:y n))))
                        nodes)

        limit-val (if (seq all-bounds)
                    (case side
                      :right (apply max all-bounds)
                      :left (apply min all-bounds)
                      :bottom (apply max all-bounds)
                      :top (apply min all-bounds))
                    0)

        ;; Base coordinate to start searching from
        ;; Increased padding to 40 for better aesthetics
        start-val (case side
                    :left (- limit-val 40)
                    :right (+ limit-val 40)
                    :top (- limit-val 40)
                    :bottom (+ limit-val 40)
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

(defn- get-port-point [node side]
  (let [x (double (or (:x node) 0.0))
        y (double (or (:y node) 0.0))
        w (double (or (:w node) 0.0))
        h (double (or (:h node) 0.0))
        cx (+ x (/ w 2.0))
        cy (+ y (/ h 2.0))]
    (case side
      :top {:x cx :y y}
      :bottom {:x cx :y (+ y h)}
      :left {:x x :y cy}
      :right {:x (+ x w) :y cy}
      {:x cx :y cy})))

(defn- get-best-port [node allowed-sides target-node]
  (if (empty? allowed-sides)
    nil
    (let [tx (double (or (:x target-node) 0.0))
          tw (double (or (:w target-node) 0.0))
          ty (double (or (:y target-node) 0.0))
          th (double (or (:h target-node) 0.0))
          target-point {:x (+ tx (/ tw 2.0))
                        :y (+ ty (/ th 2.0))}
          dist-sq (fn [p1 p2]
                    (+ (Math/pow (- (:x p1) (:x p2)) 2)
                       (Math/pow (- (:y p1) (:y p2)) 2)))
          candidates (map (fn [side] {:side side :point (get-port-point node side)}) allowed-sides)]
      (:point (apply min-key #(dist-sq (:point %) target-point) candidates)))))

(defn select-ports [n1 n2 mode]
  (let [vertical? (= mode "vertical")
        n1-pos (if vertical? (:y n1) (:x n1))
        n2-pos (if vertical? (:y n2) (:x n2))
        reversed? (> n1-pos n2-pos)
        n1-ports (get-in n1 [:ports :out])
        n2-ports-in (get-in n2 [:ports :in])
        n2-ports-out (get-in n2 [:ports :out])
        preferred-out (if vertical? :bottom :right)
        preferred-in (if vertical? :top :left)
        loop-side (if vertical? :right :bottom)
        end-side preferred-in
        n2-ports (if reversed?
                   (cond
                     (and (seq n2-ports-out) (contains? (set n2-ports-out) loop-side))
                     (vec (conj (or n2-ports-in []) loop-side))
                     :else n2-ports-in)
                   n2-ports-in)
        n1-loop-allowed? (or (empty? n1-ports) (contains? (set n1-ports) loop-side))
        n2-loop-allowed? (or (empty? n2-ports)
                             (contains? (set n2-ports) loop-side)
                             (contains? (set n2-ports) end-side))
        should-loop? (and reversed? n1-loop-allowed? n2-loop-allowed?)
        p1 (when (seq n1-ports) (get-best-port n1 n1-ports n2))
        p2 (when (seq n2-ports) (get-best-port n2 n2-ports n1))
        preferred-out-allowed? (or (empty? n1-ports) (contains? (set n1-ports) preferred-out))
        preferred-in-allowed? (or (empty? n2-ports) (contains? (set n2-ports) preferred-in))]
    (if (and (or p1 p2) (not should-loop?))
      (let [final-p1 (if (and vertical? (not reversed?) preferred-out-allowed?)
                       (get-port-point n1 preferred-out)
                       (or p1 (get-port-point n1 preferred-out)))
            final-p2 (if (and vertical? (not reversed?) preferred-in-allowed?)
                       (get-port-point n2 preferred-in)
                       (or p2 (get-port-point n2 preferred-in)))]
        {:p1 final-p1
         :p2 final-p2
         :strategy :constrained})
      (if reversed?
        (let [side (if (and n1-loop-allowed? n2-loop-allowed?) loop-side (if vertical? :right :bottom))
              p1 (if (and (seq n1-ports) (not (contains? (set n1-ports) side)))
                   (get-best-port n1 n1-ports {:x (if vertical? 100000 (:x n1)) :y (if vertical? (:y n1) 100000)})
                   (get-port-point n1 side))
              p2 (if (and (seq n2-ports) (not (contains? (set n2-ports) side)))
                   (if (contains? (set n2-ports) end-side)
                     (get-port-point n2 end-side)
                     (get-best-port n2 n2-ports {:x (if vertical? 100000 (:x n2)) :y (if vertical? (:y n2) 100000)}))
                   (get-port-point n2 side))]
          {:p1 p1 :p2 p2 :strategy :side-loop :side side})
        (let [p1 (get-port-point n1 preferred-out)
              p2 (get-port-point n2 preferred-in)]
          {:p1 p1 :p2 p2 :strategy :direct})))))

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

(defn- get-smart-ports [n1 n2 waypoints mode]
  (let [vertical? (not= mode "horizontal")
        ;; Get allowed ports from node props/rules
        n1-allowed (set (or (get-in n1 [:ports :out])
                            (if vertical? [:bottom :right] [:right :bottom])))
        n2-allowed (set (or (get-in n2 [:ports :in])
                            (if vertical? [:top :left] [:left :top])))

        allowed? (fn [ports side] (contains? ports side))

        wp-xs (map :x waypoints)
        wp-ys (map :y waypoints)
        avg-wp-x (/ (reduce + wp-xs) (count wp-xs))
        avg-wp-y (/ (reduce + wp-ys) (count wp-ys))]

    (if vertical?
      ;; Vertical Mode
      (let [n1-right (+ (:x n1) (:w n1))
            n2-right (+ (:x n2) (:w n2))
            n1-left (:x n1)
            n2-left (:x n2)
            n1-cy (+ (:y n1) (/ (:h n1) 2))
            n2-cy (+ (:y n2) (/ (:h n2) 2))

            right-side? (and (> avg-wp-x n1-right) (> avg-wp-x n2-right))
            left-side? (and (< avg-wp-x n1-left) (< avg-wp-x n2-left))

            p1-side (cond
                      (and right-side? (allowed? n1-allowed :right)) :right
                      (and left-side? (allowed? n1-allowed :left)) :left
                      :else nil)

            p2-side (cond
                      (and right-side? (allowed? n2-allowed :right)) :right
                      (and left-side? (allowed? n2-allowed :left)) :left
                      :else nil)]

        (when (or p1-side p2-side)
          {:p1 (if p1-side (get-port-point n1 p1-side) nil)
           :p2 (if p2-side (get-port-point n2 p2-side) nil)}))

      ;; Horizontal Mode
      (let [n1-bottom (+ (:y n1) (:h n1))
            n2-bottom (+ (:y n2) (:h n2))
            n1-top (:y n1)
            n2-top (:y n2)
            n1-cx (+ (:x n1) (/ (:w n1) 2))
            n2-cx (+ (:x n2) (/ (:w n2) 2))

            bottom-side? (and (> avg-wp-y n1-bottom) (> avg-wp-y n2-bottom))
            top-side? (and (< avg-wp-y n1-top) (< avg-wp-y n2-top))

            p1-side (cond
                      (and bottom-side? (allowed? n1-allowed :bottom)) :bottom
                      (and top-side? (allowed? n1-allowed :top)) :top
                      :else nil)

            p2-side (cond
                      (and bottom-side? (allowed? n2-allowed :bottom)) :bottom
                      (and top-side? (allowed? n2-allowed :top)) :top
                      :else nil)]

        (when (or p1-side p2-side)
          {:p1 (if p1-side (get-port-point n1 p1-side) nil)
           :p2 (if p2-side (get-port-point n2 p2-side) nil)})))))

(defn- get-port-side [node point]
  (let [x (:x point) y (:y point)
        nx (:x node) ny (:y node)
        nw (:w node) nh (:h node)
        right (+ nx nw)
        bottom (+ ny nh)
        eps 1.0]
    (cond
      (< (Math/abs (- y ny)) eps) :top
      (< (Math/abs (- y bottom)) eps) :bottom
      (< (Math/abs (- x nx)) eps) :left
      (< (Math/abs (- x right)) eps) :right
      :else nil)))

(defn- get-dock-point [point side]
  (let [dist 20]
    (case side
      :top {:x (:x point) :y (- (:y point) dist)}
      :bottom {:x (:x point) :y (+ (:y point) dist)}
      :left {:x (- (:x point) dist) :y (:y point)}
      :right {:x (+ (:x point) dist) :y (:y point)}
      point)))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        raw-mode (get options :direction "vertical")
        mode (if (or (= raw-mode "vertical") (= raw-mode "tb") (= raw-mode :vertical) (= raw-mode :tb))
               "vertical"
               "horizontal")]
    (assoc layout :edges
           (mapv (fn [e]
                   (let [src-id (or (:source e) (:from e))
                         tgt-id (or (:target e) (:to e))
                         n1 (some #(when (= (:id %) src-id) %) nodes)
                         n2 (some #(when (= (:id %) tgt-id) %) nodes)]
                     (if (and n1 n2)
                       (let [port-info (select-ports n1 n2 mode)
                             p1 (:p1 port-info)
                             p2 (:p2 port-info)
                             strategy (:strategy port-info)
                             side (:side port-info)]

                         (cond
                           (= strategy :side-loop)
                           (let [p1-side (get-port-side n1 p1)
                                 p2-side (get-port-side n2 p2)
                                 p1-dock (get-dock-point p1 p1-side)
                                 p2-dock (get-dock-point p2 p2-side)
                                 channel (find-safe-channel p1 p2 nodes mode side)
                                 vertical? (not= mode "horizontal")
                                 points (if vertical?
                                          ;; Vertical Side Loop
                                          [p1
                                           p1-dock
                                           {:x channel :y (:y p1-dock)}
                                           {:x channel :y (:y p2-dock)}
                                           p2-dock
                                           p2]
                                          ;; Horizontal Side Loop
                                          [p1
                                           p1-dock
                                           {:x (:x p1-dock) :y channel}
                                           {:x (:x p2-dock) :y channel}
                                           p2-dock
                                           p2])]
                             (assoc e :points (clean-points points) :p1 p1 :p2 p2 :strategy :side-loop :side side))

                           :else
                           (let [points (route-segment p1 p2 nodes mode)]
                             (assoc e :points (clean-points points) :p1 p1 :p2 p2 :strategy strategy))))

                       e)))
                 edges))))