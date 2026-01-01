(ns xflow.layout.layering.network-simplex
  (:require [clojure.set :as set]))

;; --- 1. Basic Helpers ---

(defn- slack [ranks edge]
  (let [u (:from edge)
        v (:to edge)
        rank-u (get ranks u 0)
        rank-v (get ranks v 0)
        min-len (get edge :min-len 1)]
    (- rank-v rank-u min-len)))

(defn- tight? [ranks edge]
  (zero? (slack ranks edge)))

;; --- 2. Initial Feasible Ranking (Longest Path) ---

;; --- 3. Feasible Tree Construction & Rank Tightening ---

(defn- build-tight-tree [nodes edges initial-ranks]
  "Constructs a spanning tree of tight edges.
   If the graph is not connected by tight edges, it finds an edge with minimal slack
   connecting the tree to the rest of the graph, adjusts ranks to make it tight,
   and continues. This effectively compacts the layout."
  (let [all-node-ids (set (map :id nodes))
        node-count (count all-node-ids)

        ;; Pick a start node: prefer sources (in-degree 0)
        ;; This ensures the tree grows from the 'start' of the flow
        in-degrees (reduce (fn [acc e] (update acc (:to e) (fnil inc 0)))
                           (zipmap all-node-ids (repeat 0))
                           edges)
        start-node (apply min-key #(get in-degrees % 0) (sort all-node-ids))]

    (loop [tree-nodes #{start-node}
           tree-edges #{}
           ranks initial-ranks]

      (if (= (count tree-nodes) node-count)
        {:tree-edges tree-edges :ranks ranks}

        ;; Try to find a tight edge connecting Tree to Non-Tree
        (let [tight-edge (first (filter (fn [e]
                                          (let [u-in (contains? tree-nodes (:from e))
                                                v-in (contains? tree-nodes (:to e))]
                                            (and (not= u-in v-in) ;; Exactly one endpoint in tree
                                                 (tight? ranks e))))
                                        edges))]

          (if tight-edge
            ;; Case A: Found a tight edge. Add to tree.
            (let [new-node (if (contains? tree-nodes (:from tight-edge))
                             (:to tight-edge)
                             (:from tight-edge))]
              (recur (conj tree-nodes new-node)
                     (conj tree-edges tight-edge)
                     ranks))

            ;; Case B: No tight edge. Must adjust ranks.
            (let [candidates (filter (fn [e]
                                       (and (contains? tree-nodes (:from e))
                                            (not (contains? tree-nodes (:to e)))))
                                     edges)

                  min-slack-edge (if (seq candidates)
                                   (apply min-key #(slack ranks %) candidates)
                                   ;; If no outgoing edges, check incoming
                                   (first (filter (fn [e]
                                                    (and (not (contains? tree-nodes (:from e)))
                                                         (contains? tree-nodes (:to e))))
                                                  edges)))

                  delta (if min-slack-edge (slack ranks min-slack-edge) 0)]

              (if min-slack-edge
                ;; Adjust ranks of ALL Non-Tree nodes
                (let [is-outgoing (contains? tree-nodes (:from min-slack-edge))
                      op (if is-outgoing - +)
                      new-ranks (reduce (fn [r n-id]
                                          (if (not (contains? tree-nodes n-id))
                                            (update r n-id #(op % delta))
                                            r))
                                        ranks
                                        all-node-ids)]
                  (recur tree-nodes tree-edges new-ranks))

                ;; Graph is disconnected. Pick an unvisited node and start a new component.
                (let [next-node (first (remove tree-nodes all-node-ids))]
                  (recur (conj tree-nodes next-node) tree-edges ranks))))))))))

;; --- 4. Main Entry Point ---

(defn- init-ranks [nodes edges]
  "Computes an initial feasible ranking using Longest Path.
   Ensures rank(v) >= rank(u) + min-len(u,v)."
  (let [;; Map node -> [edge1, edge2...] where edge has :to
        adj (reduce (fn [m e] (update m (:from e) (fnil conj []) e)) {} edges)

        ;; Calculate in-degrees to visit source nodes first
        in-degrees (reduce (fn [acc e] (update acc (:to e) (fnil inc 0)))
                           (zipmap (map :id nodes) (repeat 0))
                           edges)
        ;; Sort nodes by in-degree (ascending) so sources (degree 0) are visited first
        sorted-nodes (sort-by (fn [n] (get in-degrees (:id n) 0)) nodes)

        ranks (atom {})]
    (letfn [(visit [node-id depth visited]
              (when-not (contains? visited node-id)
                (swap! ranks update node-id #(max (or % 0) depth))
                (doseq [edge (get adj node-id)]
                  (let [len (get edge :min-len 1)]
                    (visit (:to edge) (+ depth len) (conj visited node-id))))))]
      (doseq [n sorted-nodes]
        (visit (:id n) 0 #{})))
    @ranks))

(defn assign-ranks [nodes edges]
  (if (empty? nodes)
    nodes
    (let [;; 1. Initial Ranks
          initial (init-ranks nodes edges)

          ;; 2. Tighten Ranks
          {:keys [ranks]} (build-tight-tree nodes edges initial)

          ;; 3. Normalize (start at 0)
          min-rank (if (seq ranks) (apply min (vals ranks)) 0)
          final-ranks (update-vals ranks #(- % min-rank))]

      (map (fn [n]
             (assoc n :rank (get final-ranks (:id n) 0)))
           nodes))))
