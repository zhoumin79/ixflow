(ns xflow.layout.sugiyama)

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

(defn assign-ranks [nodes edges]
  ;; Simple Longest Path layering
  (let [node-map (group-by :id nodes)
        adj (reduce (fn [m e] (update m (:from e) (fnil conj []) (:to e))) {} edges)
        ranks (atom {})]

    (letfn [(visit [node-id depth visited]
              (when-not (contains? visited node-id) ;; Cycle detection
                (swap! ranks update node-id #(max (or % 0) depth))
                (doseq [child (get adj node-id)]
                  (visit child (inc depth) (conj visited node-id)))))]

      (let [roots (get-roots nodes edges)]
        (doseq [root (if (seq roots) roots nodes)] ;; If no roots (cycle), start anywhere
          (visit (:id root) 0 #{}))))

    ;; Assign rank to node objects
    (map (fn [n] (assoc n :rank (get @ranks (:id n) 0))) nodes)))
