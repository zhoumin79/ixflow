(ns xflow.layout.routing.spline
  (:require [xflow.layout.routing.ortho :as ortho]
            [xflow.geometry :as geo]))

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
            new-start (if start-node (geo/clip-line-to-rect p-start p-next start-node) p-start)
            new-end (if end-node (geo/clip-line-to-rect p-end p-prev end-node) p-end)

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

        ;; Ensure edges have full path: [StartNode, ...Intermediate..., EndNode]
        edges-with-full-path
        (mapv (fn [edge]
                (let [src (get nodes-map (:from edge))
                      dst (get nodes-map (:to edge))]
                  (if (and src dst)
                    (let [src-center (geo/node-center src)
                          dst-center (geo/node-center dst)
                          ;; Existing points are intermediate waypoints from Sugiyama
                          intermediate-points (or (:points edge) [])]
                      (assoc edge :points (vec (concat [src-center] intermediate-points [dst-center]))
                             :routing-type :spline))
                    ;; If src/dst missing, keep as is
                    edge)))
              edges)]

    (assoc layout :edges
           (mapv #(route-sugiyama-edge % nodes-map) edges-with-full-path))))
