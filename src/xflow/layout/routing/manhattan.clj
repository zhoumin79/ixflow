(ns xflow.layout.routing.manhattan)

(defn route-edges [layout options]
  (let [nodes-map (into {} (map (fn [n] [(:id n) n]) (:nodes layout)))
        mode (:swimlane-mode options "horizontal")
        ;; Group edges by from node to handle branching overlaps
        edges-by-from (group-by :from (:edges layout))]

    (update layout :edges
            (fn [_] ;; We ignore original edges list and rebuild from groups to ensure order
              (flatten
               (map (fn [[_ siblings]]
                      (let [;; Pre-calculate geometry for all siblings to find common alignment
                            edge-geoms
                            (map (fn [e]
                                   (let [n1 (get nodes-map (:from e))
                                         n2 (get nodes-map (:to e))]
                                     (if (and n1 n2)
                                       (let [p1 {:x (+ (:x n1) (if (= mode "horizontal") (:w n1) (/ (:w n1) 2)))
                                                 :y (+ (:y n1) (if (= mode "horizontal") (/ (:h n1) 2) (:h n1)))}
                                             p2 {:x (if (= mode "horizontal") (:x n2) (+ (:x n2) (/ (:w n2) 2)))
                                                 :y (if (= mode "horizontal") (+ (:y n2) (/ (:h n2) 2)) (:y n2))}

                                             mid-x (/ (+ (:x p1) (:x p2)) 2)
                                             mid-y (/ (+ (:y p1) (:y p2)) 2)]
                                         {:e e :p1 p1 :p2 p2 :mid-x mid-x :mid-y mid-y :valid true})
                                       {:e e :valid false})))
                                 siblings)

                            ;; Calculate common alignment line (min mid-point to keep branches high/left)
                            valid-geoms (filter :valid edge-geoms)
                            common-mid-x (if (seq valid-geoms) (apply min (map :mid-x valid-geoms)) 0)
                            common-mid-y (if (seq valid-geoms) (apply min (map :mid-y valid-geoms)) 0)]

                        (map (fn [geom]
                               (if (:valid geom)
                                 (let [{:keys [e p1 p2]} geom
                                       ;; Use common mid for branching alignment
                                       final-mid-x common-mid-x
                                       final-mid-y common-mid-y]

                                   (assoc e :points
                                          (if (= mode "horizontal")
                                            ;; Horizontal: Right -> Right
                                            ;; Step at common mid-x
                                            [p1
                                             {:x final-mid-x :y (:y p1)}
                                             {:x final-mid-x :y (:y p2)}
                                             p2]

                                            ;; Vertical: Bottom -> Top
                                            ;; Step at common mid-y
                                            [p1
                                             {:x (:x p1) :y final-mid-y}
                                             {:x (:x p2) :y final-mid-y}
                                             p2])))
                                 (:e geom)))
                             edge-geoms)))
                    edges-by-from))))))
