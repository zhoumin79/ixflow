(ns xflow.layout.routing.spline
  (:require [clojure.string :as str]))

(defn- get-port [node type direction]
  (let [x (:x node)
        y (:y node)
        w (:w node)
        h (:h node)
        cx (+ x (/ w 2))
        cy (+ y (/ h 2))]
    (case direction
      "tb" (if (= type :source)
             {:x cx :y (+ y h)} ;; Bottom
             {:x cx :y y}) ;; Top
      "bt" (if (= type :source)
             {:x cx :y y} ;; Top
             {:x cx :y (+ y h)}) ;; Bottom
      "lr" (if (= type :source)
             {:x (+ x w) :y cy} ;; Right
             {:x x :y cy}) ;; Left
      "rl" (if (= type :source)
             {:x x :y cy} ;; Left
             {:x (+ x w) :y cy}) ;; Right
      ;; Default to tb
      (if (= type :source)
        {:x cx :y (+ y h)}
        {:x cx :y y}))))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        node-map (into {} (map (fn [n] [(:id n) n]) nodes))
        direction (:direction options "tb")]
    (assoc layout :edges
           (map (fn [edge]
                  (let [src (get node-map (:from edge))
                        tgt (get node-map (:to edge))
                        start-pt (get-port src :source direction)
                        end-pt (get-port tgt :target direction)
                        ;; Existing points are from dummy nodes (intermediate waypoints)
                        intermediate-points (or (:points edge) [])]
                    (assoc edge
                           :points (concat [start-pt] intermediate-points [end-pt])
                           :routing-type :spline)))
                edges))))
