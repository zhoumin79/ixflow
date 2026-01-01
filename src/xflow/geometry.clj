(ns xflow.geometry
  "Core geometry utilities for layout calculations.
   
   This namespace focuses on mathematical operations, coordinate transformations,
   and geometric algorithms needed for graph layout (e.g., bounding boxes,
   node shifting, point simplification, routing helpers).
   
   For SVG path generation and shape rendering, see `xcommon.geometry`."
  (:require [clojure.string :as str]))

(defn distance
  "Calculate Euclidean distance between two points."
  [{:keys [x y] :as p1} p2]
  (let [dx (- (:x p2) x)
        dy (- (:y p2) y)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn midpoint
  "Calculate midpoint between two points."
  [p1 p2]
  {:x (/ (+ (:x p1) (:x p2)) 2)
   :y (/ (+ (:y p1) (:y p2)) 2)})

(defn interpolate
  "Linear interpolation between p1 and p2 at t (0.0 to 1.0)."
  [p1 p2 t]
  {:x (+ (:x p1) (* (- (:x p2) (:x p1)) t))
   :y (+ (:y p1) (* (- (:y p2) (:y p1)) t))})

(defn calculate-label-pos
  "Calculate the best position for a label on an edge defined by points."
  [points]
  (let [cnt (count points)]
    (cond
      (< cnt 2) nil
      (= cnt 2)
      (let [p0 (first points)
            p1 (second points)
            ;; Initial guess at 0.4
            t 0.4
            base-pos (interpolate p0 p1 t)

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
      ;; Find the middle segment or point
      (let [mid-idx (int (/ cnt 2))]
        (if (odd? cnt)
          (nth points mid-idx)
          (midpoint (nth points (dec mid-idx)) (nth points mid-idx)))))))

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

(defn collinear?
  "Check if three points are collinear (same line)."
  [p1 p2 p3]
  (let [x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)
        x3 (:x p3) y3 (:y p3)
        epsilon 0.001]
    ;; Check cross product for general collinearity
    (< (Math/abs (- (* (- y2 y1) (- x3 x2))
                    (* (- y3 y2) (- x2 x1))))
       epsilon)))

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
           (if (collinear? p1 p2 p)
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
