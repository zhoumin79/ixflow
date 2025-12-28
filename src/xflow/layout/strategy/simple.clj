(ns xflow.layout.strategy.simple
  (:require [xflow.layout.sugiyama :as sugiyama]))

(defn assign-coordinates [nodes edges options]
  (let [ranked-nodes (sugiyama/assign-ranks nodes edges)
        rank-groups (group-by :rank ranked-nodes)
        max-rank (apply max (keys rank-groups))

        direction (:direction options "tb") ;; "tb" or "lr"
        node-width 180
        node-height 50
        x-gap 50
        y-gap 80

        ;; Calculate dimensions for each rank
        rank-dimensions
        (reduce
         (fn [acc r]
           (let [nodes-in-rank (get rank-groups r)
                 count (count nodes-in-rank)
                 ;; Dimension along the rank (width for TB, height for LR)
                 rank-size (if (= direction "lr")
                             (+ (* count node-height) (* (dec count) y-gap))
                             (+ (* count node-width) (* (dec count) x-gap)))]
             (assoc acc r rank-size)))
         {}
         (range (inc max-rank)))

        max-rank-size (apply max (vals rank-dimensions))

        processed-nodes
        (mapcat
         (fn [r]
           (let [nodes-in-rank (get rank-groups r)
                 rank-size (get rank-dimensions r)
                 start-offset (/ (- max-rank-size rank-size) 2)]

             (map-indexed
              (fn [idx node]
                (if (= direction "lr")
                  ;; Left-to-Right
                  (let [x (* r (+ node-width x-gap))
                        y (+ start-offset (* idx (+ node-height y-gap)))]
                    (assoc node
                           :x x
                           :y y
                           :w node-width
                           :h node-height))
                  ;; Top-to-Bottom (default)
                  (let [x (+ start-offset (* idx (+ node-width x-gap)))
                        y (* r (+ node-height y-gap))]
                    (assoc node
                           :x x
                           :y y
                           :w node-width
                           :h node-height))))
              nodes-in-rank)))
         (range (inc max-rank)))

        ;; Calculate total graph dimensions
        total-width (if (= direction "lr")
                      (+ (* (inc max-rank) node-width) (* max-rank x-gap))
                      max-rank-size)
        total-height (if (= direction "lr")
                       max-rank-size
                       (+ (* (inc max-rank) node-height) (* max-rank y-gap)))]

    {:nodes processed-nodes
     :edges edges
     :width total-width
     :height total-height
     :swimlanes []})) ;; No swimlanes for simple layout
