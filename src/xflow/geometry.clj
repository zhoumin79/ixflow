(ns xflow.geometry
  "Core geometry utilities for layout calculations.
   
   This namespace focuses on mathematical operations, coordinate transformations,
   and geometric algorithms needed for graph layout (e.g., bounding boxes,
   node shifting, point simplification, routing helpers).
   
   For SVG path generation and shape rendering, see `xcommon.geometry`."
  (:require [clojure.string :as str]
            [xcommon.geometry :as geo]))

(defn- segment-lengths [points]
  (map (fn [[p1 p2]] (geo/distance p1 p2))
       (partition 2 1 points)))

(defn- point-at-ratio [points ratio]
  (let [segs (partition 2 1 points)
        lengths (segment-lengths points)
        total-len (reduce + lengths)
        target-dist (* total-len ratio)]
    (loop [segs segs
           lens lengths
           covered 0.0]
      (if (or (empty? segs) (empty? lens))
        (last points)
        (let [[p1 p2] (first segs)
              len (first lens)]
          (if (and (> len 0) (<= (+ covered len) target-dist))
            (recur (rest segs) (rest lens) (+ covered len))
            ;; Found segment
            (let [rem-dist (max 0.0 (- target-dist covered))
                  t (if (zero? len) 0.0 (/ rem-dist len))]
              (geo/interpolate p1 p2 t))))))))

(defn calculate-label-pos
  "Calculate the best position for a label on an edge defined by points."
  [points]
  (let [cnt (count points)]
    (cond
      (< cnt 2) nil
      (= cnt 2)
      (let [p0 (first points)
            p1 (second points)
            ;; Initial guess at 0.4 (kept for backward compatibility/preference)
            t 0.4
            base-pos (geo/interpolate p0 p1 t)

            dx (- (:x p1) (:x p0))
            dy (- (:y p1) (:y p0))]

        ;; Heuristic to avoid overlap with target (p1)
        (if (> (Math/abs dy) (Math/abs dx))
          ;; Vertical layout: check Y distance
          (let [safe-y (- (:y p1) 35)]
            (assoc base-pos :y (min (:y base-pos) safe-y)))
          ;; Horizontal layout: check X distance (assuming L->R)
          (if (> dx 0)
            (let [safe-x (- (:x p1) 40)]
              (assoc base-pos :x (min (:x base-pos) safe-x)))
            base-pos)))

      :else
      ;; Multi-point path (Spline or Manhattan)
      ;; Use the point at 50% of the total path length
      (point-at-ratio points 0.5))))

(defn bounding-box
  "Calculate the bounding box of a collection of items with :x, :y, :w, :h."
  [items]
  (when (seq items)
    (let [x-vals (map :x items)
          y-vals (map :y items)
          xw-vals (map #(+ (:x %) (or (:w %) 0)) items)
          yh-vals (map #(+ (:y %) (or (:h %) 0)) items)]
      {:min-x (apply min x-vals)
       :min-y (apply min y-vals)
       :max-x (apply max xw-vals)
       :max-y (apply max yh-vals)})))

(defn shift-point
  "Shift a point by dx, dy."
  [p dx dy]
  (if (and (:x p) (:y p))
    (assoc p :x (+ (:x p) dx) :y (+ (:y p) dy))
    p))

(defn shift-items
  "Shift a collection of items (nodes, edges with points) by dx, dy."
  [items dx dy]
  (mapv (fn [item]
          (cond-> item
            ;; Shift :points (vector of points)
            (:points item)
            (update :points (fn [pts] (mapv #(shift-point % dx dy) pts)))

            ;; Shift :p1, :p2
            (:p1 item) (update :p1 #(shift-point % dx dy))
            (:p2 item) (update :p2 #(shift-point % dx dy))

            ;; Shift :label-pos
            (:label-pos item) (update :label-pos #(shift-point % dx dy))

            ;; Shift :x, :y
            (and (:x item) (:y item))
            (shift-point dx dy)))
        items))

(defn simplify-points
  "Remove redundant collinear points from a path."
  [points]
  (if (< (count points) 3)
    points
    (reduce
     (fn [acc p]
       (if (< (count acc) 2)
         (conj acc p)
         (let [p2 (peek acc)
               p1 (peek (pop acc))]
           (if (geo/collinear? p1 p2 p)
             (conj (pop acc) p) ;; Replace middle point
             (conj acc p)))))
     [(first points) (second points)]
     (drop 2 points))))

(defn intersects-box?
  "Check if segment p1-p2 intersects with box (min-x, min-y, max-x, max-y)."
  [p1 p2 box]
  (let [sx1 (min (:x p1) (:x p2))
        sx2 (max (:x p1) (:x p2))
        sy1 (min (:y p1) (:y p2))
        sy2 (max (:y p1) (:y p2))

        bx1 (:min-x box)
        by1 (:min-y box)
        bx2 (:max-x box)
        by2 (:max-y box)]

    (and (< sx1 bx2) (> sx2 bx1)
         (< sy1 by2) (> sy2 by1))))

(defn swap-xy
  "Swaps :x and :y coordinates for a point or collection of items."
  [item-or-items]
  (if (map? item-or-items)
    (let [item item-or-items]
      (assoc item
             :x (:y item)
             :y (:x item)
             ;; Also swap dimensions if present? usually yes for layout rotation
             :w (:h item)
             :h (:w item)))
    (mapv swap-xy item-or-items)))

(defn swap-points-xy
  "Swaps x and y for a list of points (e.g. in an edge)."
  [points]
  (mapv (fn [p] {:x (:y p) :y (:x p)}) points))

(defn distribute-points
  "Returns a sequence of {:x ... :y ...} points distributed along a line.
   axis: :x (vary x, fixed y) or :y (vary y, fixed x)"
  [start length fixed-coord count axis]
  (if (and (number? start) (number? length) (number? fixed-coord) (number? count) (pos? count))
    (let [step (double (/ length (inc count)))]
      (map (fn [i]
             (let [var-coord (double (+ start (* (inc i) step)))]
               (if (= axis :x)
                 {:x var-coord :y (double fixed-coord)}
                 {:x (double fixed-coord) :y var-coord})))
           (range count)))
    []))

(defn swap-edges-xy
  "Swaps x and y for the points inside a list of edges."
  [edges]
  (mapv (fn [e]
          (if (:points e)
            (update e :points swap-points-xy)
            e))
        edges))

(defn node-center
  "Calculate the center point of a node or rectangle with :x, :y, :w, :h."
  [{:keys [x y w h]}]
  {:x (+ (or x 0) (/ (or w 0) 2.0))
   :y (+ (or y 0) (/ (or h 0) 2.0))})

(defn clip-line-to-rect
  "Find the intersection point of a line segment p1-p2 with the border of a rectangle.
   rect: {:x :y :w :h}
   p1: point inside or on border (e.g. center)
   p2: point outside (e.g. next waypoint)"
  [p1 p2 rect]
  (let [cx (+ (:x rect) (/ (:w rect) 2))
        cy (+ (:y rect) (/ (:h rect) 2))
        dx (- (:x p2) cx)
        dy (- (:y p2) cy)]
    (if (and (zero? dx) (zero? dy))
      p1
      (let [slope (if (zero? dx) 1000000.0 (/ (double dy) (double dx)))
            half-w (/ (:w rect) 2)
            half-h (/ (:h rect) 2)

            ;; Check intersections with vertical sides
            ix1 (if (> dx 0) half-w (- half-w))
            iy1 (* ix1 slope)

            ;; Check intersections with horizontal sides
            iy2 (if (> dy 0) half-h (- half-h))
            ix2 (if (zero? slope) 1000000.0 (/ iy2 slope))]

        (if (<= (Math/abs (double iy1)) (double half-h))
          {:x (+ cx ix1) :y (+ cy iy1)}
          {:x (+ cx ix2) :y (+ cy iy2)})))))

