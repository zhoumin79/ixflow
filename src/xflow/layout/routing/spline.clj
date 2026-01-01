(ns xflow.layout.routing.spline
  (:require [xflow.layout.routing.ortho :as ortho]))

(defn- clip-line-to-rect [p1 p2 rect]
  ;; Simple Cohen-Sutherland-like clipping or just finding intersection with rect border
  ;; rect: {:x :y :w :h}
  ;; p1: inside or on border (start/end node center)
  ;; p2: outside (next waypoint)
  ;; We want point on border of rect towards p2.
  (let [cx (+ (:x rect) (/ (:w rect) 2))
        cy (+ (:y rect) (/ (:h rect) 2))
        dx (- (:x p2) cx)
        dy (- (:y p2) cy)]
    (if (and (zero? dx) (zero? dy))
      p1
      (let [slope (if (zero? dx) 1000000.0 (/ (double dy) (double dx))) ;; Avoid div by zero, use doubles
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

(defn- route-sugiyama-edge [edge nodes-map]
  (let [points (:points edge)]
    (cond
      (and points (> (count points) 1))
      ;; We have waypoints (at least start/end)
      (let [start-node (get nodes-map (:from edge))
            end-node (get nodes-map (:to edge))

            p-start (first points)
            p-next (second points)

            p-end (last points)
            p-prev (nth points (- (count points) 2))

            ;; Clip start and end
            new-start (if start-node (clip-line-to-rect p-start p-next start-node) p-start)
            new-end (if end-node (clip-line-to-rect p-end p-prev end-node) p-end)

            ;; Reassemble points
            new-points (if (> (count points) 2)
                         (concat [new-start] (subvec (vec points) 1 (dec (count points))) [new-end])
                         [new-start new-end])]
        (assoc edge :points (vec new-points) :routing-type :spline))

      :else
      ;; No points or single point? Should have been handled before.
      edge)))

(defn route-edges [layout options]
  (let [edges (:edges layout)
        nodes (:nodes layout)
        nodes-map (into {} (map (juxt :id identity) nodes))

        ;; Helper to get node center
        get-center (fn [node]
                     {:x (+ (:x node) (/ (:w node) 2))
                      :y (+ (:y node) (/ (:h node) 2))})

        ;; Ensure edges have full path: [StartNode, ...Intermediate..., EndNode]
        edges-with-full-path
        (mapv (fn [edge]
                (let [src (get nodes-map (:from edge))
                      dst (get nodes-map (:to edge))]
                  (if (and src dst)
                    (let [src-center (get-center src)
                          dst-center (get-center dst)
                          ;; Existing points are intermediate waypoints from Sugiyama
                          intermediate-points (or (:points edge) [])]
                      (assoc edge :points (vec (concat [src-center] intermediate-points [dst-center]))
                             :routing-type :spline))
                    ;; If src/dst missing, keep as is
                    edge)))
              edges)]

    (assoc layout :edges
           (mapv #(route-sugiyama-edge % nodes-map) edges-with-full-path))))
