(ns xflow.layout.strategy.swimlane
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [xflow.layout.layering.network-simplex :as network-simplex]
            [xflow.layout.sugiyama :as sugiyama]
            [xflow.layout.strategy.swimlane-ordering :as swimlane-ordering]))

(def NODE-WIDTH 150)
(def NODE-HEIGHT 80)
(def NODE-SEP 50)
(def LAYER-SEP 100)

;; --- 辅助函数 (Helpers) ---

(defn- swap-node-dims [nodes]
  (map (fn [n] (assoc n :w (:h n) :h (:w n))) nodes))

(defn- swap-coords [nodes]
  (map (fn [n] (assoc n :x (:y n) :y (:x n))) nodes))

(defn- swap-edge-points [edges]
  (map (fn [e]
         (if (:points e)
           (update e :points (fn [pts] (mapv (fn [p] {:x (:y p) :y (:x p)}) pts)))
           e))
       edges))

(defn- find-pool-by-name [pools name]
  ;; Recursive search for pool with name
  (some (fn [p]
          (if (= (:name p) name)
            p
            (when (seq (:nodes p))
              (find-pool-by-name (filter #(= (:type %) :pool) (:nodes p)) name))))
        pools))

(defn- get-ordered-lane-ids [pools]
  (let [ids (atom [])]
    (letfn [(visit [pool path]
              (let [current-path (conj path (:name pool))
                    full-id (str/join " / " current-path)]
                (swap! ids conj full-id)
                (doseq [child (:nodes pool)]
                  (when (= (:type child) :pool)
                    (visit child current-path)))))]
      (doseq [p pools]
        (visit p [])))
    @ids))

;; --- 核心逻辑 (Core Logic) ---

(defn group-by-swimlane [nodes pools options]
  (let [direction (:direction options)
        ;; Group nodes by their :swimlane-id
        lanes (group-by :swimlane-id nodes)
        ;; Get explicitly ordered pool IDs
        ordered-pool-ids (get-ordered-lane-ids pools)
        ;; Filter out only those that exist in lanes, removing nil (Global nodes)
        existing-lanes (disj (set (keys lanes)) nil)
        sorted-lane-ids (concat
                         (filter existing-lanes ordered-pool-ids)
                         (sort (set/difference existing-lanes (set ordered-pool-ids))))]

    (map-indexed (fn [idx lane-id]
                   (let [lane-name (last (str/split lane-id #" / "))
                         pool (find-pool-by-name pools lane-name)]
                     {:id lane-id
                      :index idx
                      :nodes (get lanes lane-id)
                      :label lane-name
                      :props (:props pool)}))
                 sorted-lane-ids)))

(defn- assign-ranks [nodes edges]
  ;; 使用 Network Simplex 算法进行全局分层
  (network-simplex/assign-ranks nodes edges))

(defn calculate-lane-dimensions [lanes mode]
  (let [is-horizontal? (= mode "horizontal")]
    (map (fn [lane]
           (let [nodes (:nodes lane)
                 ranks (group-by :rank nodes)
                 max-size (if (seq ranks)
                            (apply max (map (fn [[r ns]]
                                              (let [node-sizes (map #(if (:dummy? %) 10 (or (if is-horizontal? (:h %) (:w %)) 0)) ns)
                                                    total (reduce + node-sizes)
                                                    seps (* (dec (count ns)) NODE-SEP)]
                                                (+ total seps)))
                                            ranks))
                            0)]
             (assoc lane :size (max 200 (+ max-size 100)))))
         lanes)))

(defn assign-initial-coordinates [nodes lanes options]
  (let [mode (if (= (:layout options) "cluster") "vertical" (:swimlane-mode options "horizontal"))
        is-horizontal? (= mode "horizontal")

        lane-map (into {} (map (fn [l] [(:id l) l]) lanes))
        lane-ids (map :id lanes)
        lane-sizes (map :size lanes)
        lane-offsets (reductions + 0 lane-sizes)
        lane-offset-map (zipmap lane-ids lane-offsets)]

    (map (fn [node]
           (let [lane-id (:swimlane-id node)
                 rank (:rank node)
                 lane-pos (get lane-offset-map lane-id 0)
                 lane-size (get-in lane-map [lane-id :size] 200)

                 siblings (sort-by :order (filter #(and (= (:swimlane-id %) lane-id)
                                                        (= (:rank %) rank))
                                                  nodes))
                 idx (.indexOf (vec siblings) node)

                 sizes (map #(if (:dummy? %) 10 (or (if is-horizontal? (:h %) (:w %)) 0)) siblings)
                 total-size (+ (reduce + sizes) (* (dec (count siblings)) NODE-SEP))

                 start-pos (+ lane-pos (/ (- lane-size total-size) 2))

                 prev-siblings (take idx siblings)
                 prev-size (+ (reduce + (map #(if (:dummy? %) 10 (or (if is-horizontal? (:h %) (:w %)) 0)) prev-siblings))
                              (* idx NODE-SEP))

                 pos (+ start-pos prev-size)
                 rank-pos (* rank (+ (if is-horizontal? NODE-WIDTH NODE-HEIGHT) LAYER-SEP))]

             (if is-horizontal?
               (assoc node
                      :y (double pos)
                      :x (double rank-pos)
                      :w (double (if (:dummy? node) 0 (or (:w node) 0)))
                      :h (double (if (:dummy? node) 0 (or (:h node) 0))))
               (assoc node
                      :x (double pos)
                      :y (double rank-pos)
                      :w (double (if (:dummy? node) 0 (or (:w node) 0)))
                      :h (double (if (:dummy? node) 0 (or (:h node) 0)))))))
         nodes)))

(defn calculate-strip-geometries [lanes total-width total-height mode]
  (let [is-horizontal? (= mode "horizontal")
        lane-offsets (reductions + 0 (map :size lanes))]
    (map-indexed (fn [i l]
                   (let [offset (nth lane-offsets i)
                         size (:size l)]
                     (if is-horizontal?
                       ;; Horizontal strips (Rows)
                       {:x (double 0) :y (double offset)
                        :w (double total-width) :h (double size)
                        :label (:label l) :index (:index l) :vertical? false
                        :props (:props l)}
                       ;; Vertical strips (Cols)
                       {:x (double offset) :y (double 0)
                        :w (double size) :h (double total-height)
                        :label (:label l) :index (:index l) :vertical? true
                        :props (:props l)})))
                 lanes)))

;; Legacy internal layout function (kept for reference or standalone use)
(defn layout [nodes edges swimlanes options]
  (let [direction (get options :direction :TB)
        is-horizontal? (or (= direction :LR) (= direction :RL)) ;; LR = Horizontal Swimlanes

        ;; 1. Pre-processing
        nodes-proc (if is-horizontal? (swap-node-dims nodes) nodes)

        ;; 2. Global Ranking
        ranked-nodes (assign-ranks nodes-proc edges)

        ;; 3. Insert Dummy Nodes
        {:keys [nodes dummies] :as graph-with-dummies} (sugiyama/insert-dummy-nodes ranked-nodes edges)

        ;; Propagate swimlane-id
        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes-proc))
        nodes-with-dummies (map (fn [n]
                                  (if (:dummy? n)
                                    (let [parent-edge (:parent-edge n)
                                          src-id (:from parent-edge)
                                          src-node (get nodes-map src-id)]
                                      (assoc n :swimlane-id (:swimlane-id src-node)
                                             :w 10 :h 0))
                                    n))
                                (:nodes graph-with-dummies))

        ;; 4. Ordering
        ordered-nodes (swimlane-ordering/order-nodes nodes-with-dummies (:edges graph-with-dummies) swimlanes)

        ;; 5. Grouping & Geometry
        ;; Note: Reusing new public functions requires adapting data structures or calling them appropriately.
        ;; For now, maintaining original logic flow but simplified.
        lanes (group-by-swimlane ordered-nodes swimlanes options)

        ;; Use new function for dimensions (assuming Vertical Columns for internal layout logic)
        lanes-with-dims (calculate-lane-dimensions lanes "vertical")

        total-width (reduce + (map :size lanes-with-dims))

        ;; 6. Positioning
        ;; Use new function
        placed-nodes (assign-initial-coordinates ordered-nodes lanes-with-dims {:swimlane-mode "vertical"})

        ;; 7. Total Height
        max-rank (if (seq placed-nodes) (apply max (map :rank placed-nodes)) 0)
        total-height (* (inc max-rank) (+ NODE-HEIGHT LAYER-SEP))

        ;; 8. Strips
        strips (calculate-strip-geometries lanes-with-dims total-width total-height "vertical")

        ;; 9. Restore Edges
        final-edges (sugiyama/apply-edge-points edges placed-nodes)
        final-nodes (remove :dummy? placed-nodes)]

    ;; 10. Post-processing
    (if is-horizontal?
      {:nodes (swap-coords final-nodes)
       :edges (swap-edge-points final-edges)
       :swimlanes strips
       :width total-height
       :height total-width}
      {:nodes final-nodes
       :edges final-edges
       :swimlanes strips
       :width total-width
       :height total-height})))
