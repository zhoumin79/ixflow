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

(defn- init-ranks [nodes edges]
  "Computes an initial feasible ranking using Longest Path.
   Ensures rank(v) >= rank(u) + min-len(u,v)."
  (let [;; Map node -> [edge1, edge2...] where edge has :to
        adj (reduce (fn [m e] (update m (:from e) (fnil conj []) e)) {} edges)
        ranks (atom {})]
    (letfn [(visit [node-id depth visited]
              (when-not (contains? visited node-id)
                (swap! ranks update node-id #(max (or % 0) depth))
                (doseq [edge (get adj node-id)]
                  (let [len (get edge :min-len 1)]
                    (visit (:to edge) (+ depth len) (conj visited node-id))))))]
      (doseq [n nodes]
        (visit (:id n) 0 #{})))
    @ranks))

;; --- 3. Feasible Tree Construction & Rank Tightening ---

(defn- build-tight-tree [nodes edges initial-ranks]
  "Constructs a spanning tree of tight edges.
   If the graph is not connected by tight edges, it finds an edge with minimal slack
   connecting the tree to the rest of the graph, adjusts ranks to make it tight,
   and continues. This effectively compacts the layout."
  (let [all-node-ids (set (map :id nodes))
        node-count (count all-node-ids)
        ;; Start with an arbitrary node in the tree
        start-node (first all-node-ids)]

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
            ;; Find edge with minimal slack connecting Tree -> Non-Tree
            ;; (We only look at outgoing edges from Tree to Non-Tree to 'pull' Non-Tree nodes up,
            ;;  or incoming from Non-Tree to Tree? 
            ;;  To minimize ranks, we usually want to pull nodes 'up' (lower rank number).
            ;;  If u in Tree, v in Non-Tree: slack = r(v) - r(u) - minlen.
            ;;  We want to reduce slack. Decrease r(v).
            ;;  So we look for minimal slack edges (u,v) where u in Tree, v not in Tree.)
            (let [candidates (filter (fn [e]
                                       (and (contains? tree-nodes (:from e))
                                            (not (contains? tree-nodes (:to e)))))
                                     edges)

                  min-slack-edge (if (seq candidates)
                                   (apply min-key #(slack ranks %) candidates)
                                   ;; If no outgoing edges, check incoming?
                                   ;; If the graph is connected, there must be some edge.
                                   ;; If we only have incoming edges from Non-Tree to Tree:
                                   ;; v in Non-Tree, u in Tree. slack = r(u) - r(v) - minlen.
                                   ;; We want to reduce slack. Increase r(v).
                                   (first (filter (fn [e]
                                                    (and (not (contains? tree-nodes (:from e)))
                                                         (contains? tree-nodes (:to e))))
                                                  edges)))

                  delta (if min-slack-edge (slack ranks min-slack-edge) 0)]

              (if min-slack-edge
                ;; Adjust ranks of ALL Non-Tree nodes
                ;; If we found (Tree -> Non-Tree), we decrease Non-Tree ranks by delta.
                ;; If we found (Non-Tree -> Tree), we increase Non-Tree ranks by delta.
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
