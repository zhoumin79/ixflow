(ns xflow.layout.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; --- Constants ---
(def NODE-WIDTH 150)
(def NODE-HEIGHT 80)
(def LAYER-SEP 100)
(def NODE-SEP 50)
(def SWIMLANE-HEADER-SIZE 30)

;; --- 1. Rank Assignment (Layering) ---

(defn- get-roots [nodes edges]
  (let [targets (set (map :to edges))]
    (filter #(not (contains? targets (:id %))) nodes)))

(defn- assign-ranks [nodes edges]
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

;; --- 2. Swimlane Logic ---

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

(defn- group-by-swimlane [nodes pools options]
  (let [direction (:direction options)
        swimlane-mode (:swimlane-mode options) ;; "horizontal" or "vertical"

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

;; --- 3. Coordinate Assignment ---

(defn- calculate-lane-dimensions [lanes mode]
  ;; Calculate required width/height for each lane based on max rank or nodes count
  (map (fn [lane]
         (let [nodes (:nodes lane)
               nodes-by-rank (group-by :rank nodes)
               max-nodes-in-rank (if (seq nodes-by-rank)
                                   (apply max (map count (vals nodes-by-rank)))
                                   0)
               min-size (if (= mode "horizontal") 200 300)

               ;; Calculate dynamic size needed to fit parallel nodes
               node-size (if (= mode "horizontal") NODE-HEIGHT NODE-WIDTH)
               calculated-size (+ (* max-nodes-in-rank node-size)
                                  (* (max 0 (dec max-nodes-in-rank)) NODE-SEP)
                                  100)] ;; Padding
           (assoc lane
                  :size (max min-size calculated-size))))
       lanes))

(defn- assign-coordinates [nodes edges pools options]
  (let [ranked-nodes (assign-ranks nodes edges)
        layout-mode (:layout options) ;; "cluster" or undefined
        ;; Force vertical mode for cluster layout
        mode (if (= layout-mode "cluster") "vertical" (:swimlane-mode options "horizontal"))
        direction (:direction options "lr")

        ;; Group by Swimlane
        lanes-data (group-by-swimlane ranked-nodes pools options)
        lanes (calculate-lane-dimensions lanes-data mode)

        ;; Map lane-id to lane-spec
        lane-map (into {} (map (fn [l] [(:id l) l]) lanes))

        ;; Calculate Lane Offsets
        lane-offsets (reductions + 0 (map :size lanes))
        lane-offset-map (zipmap (map :id lanes) lane-offsets)

        ;; Calculate Max Rank
        max-rank (if (seq ranked-nodes) (apply max (map :rank ranked-nodes)) 0)

        ;; -- Collision avoidance --
        rank-groups (group-by (fn [n] [(:swimlane-id n) (:rank n)]) ranked-nodes)
        ;; REMOVED sorting to respect DSL definition order
        rank-groups (into {} rank-groups)

        ;; Calculate Node Positions (Initial Pass)
        initial-nodes
        (map (fn [n]
               (let [rank (:rank n)
                     lane-id (:swimlane-id n)
                     lane-offset (get lane-offset-map lane-id 0)
                     lane-size (get-in lane-map [lane-id :size] 200)

                     ;; Find position among siblings
                     siblings (get rank-groups [lane-id rank])
                     idx (.indexOf siblings n)
                     count (count siblings)

                     ;; Calculate Cross-Axis Position
                     node-cross-size (if (= mode "horizontal") NODE-HEIGHT NODE-WIDTH)
                     sep NODE-SEP
                     total-span (+ (* count node-cross-size) (* (dec count) sep))
                     start-offset (- (/ total-span 2))
                     relative-pos (+ start-offset
                                     (* idx (+ node-cross-size sep))
                                     (/ node-cross-size 2))

                     lane-center (+ lane-offset (/ lane-size 2))
                     final-cross-pos (+ lane-center relative-pos)

                     ;; Flow Position
                     flow-pos (* rank (+ NODE-WIDTH LAYER-SEP))

                     x (if (= mode "horizontal")
                         (+ flow-pos 50)
                         (- final-cross-pos (/ NODE-WIDTH 2)))

                     y (if (= mode "horizontal")
                         (- final-cross-pos (/ NODE-HEIGHT 2))
                         (+ flow-pos 50))]

                 (assoc n
                        :x x :y y
                        :w NODE-WIDTH :h NODE-HEIGHT)))
             ranked-nodes)

        ;; -- Cluster Layout Post-Processing --
        processed-nodes
        (if (= layout-mode "cluster")
          (let [;; Separate global nodes (empty swimlane-id)
                global-nodes (filter #(empty? (:swimlane-id %)) initial-nodes)
                cluster-nodes (remove #(empty? (:swimlane-id %)) initial-nodes)

                ;; Calculate actual content width of clusters
                cluster-min-x (if (seq cluster-nodes) (apply min (map :x cluster-nodes)) 0)
                cluster-max-x (if (seq cluster-nodes) (apply max (map #(+ (:x %) (:w %)) cluster-nodes)) 0)
                cluster-center (/ (+ cluster-min-x cluster-max-x) 2)

                ;; Rank Split Strategy for Vertical Separation
                ;; Identify ranks that contain BOTH Global and Cluster nodes
                nodes-by-rank (group-by :rank initial-nodes)
                max-r (if (seq initial-nodes) (apply max (map :rank initial-nodes)) 0)

                ;; Calculate Y-shift for each rank
                rank-shifts
                (first
                 (reduce (fn [[shifts current-shift] r]
                           (let [r-nodes (get nodes-by-rank r [])
                                 has-global? (some #(empty? (:swimlane-id %)) r-nodes)
                                 has-cluster? (some #(not (empty? (:swimlane-id %))) r-nodes)

                                 ;; If conflict, we push Global nodes down by adding GAP to current-shift
                                 ;; And subsequent ranks inherit the TOTAL shift
                                 conflict? (and has-global? has-cluster?)
                                 gap 150 ;; Extra vertical space

                                 next-shift (if conflict? (+ current-shift gap) current-shift)]

                             [(assoc shifts r {:base current-shift :conflict? conflict?})
                              next-shift]))
                         [{} 0]
                         (range (inc max-r))))

                ;; Apply rank shifts (Layer 1 Processing)
                spaced-nodes
                (map (fn [n]
                       (let [r (:rank n)
                             {:keys [base conflict?]} (get rank-shifts r)
                             is-global? (empty? (:swimlane-id n))

                             ;; Global nodes in conflict ranks get an EXTRA push
                             y-shift (if (and conflict? is-global?)
                                       (+ base 150)
                                       base)

                             new-y (+ (:y n) y-shift)

                             ;; Center global nodes horizontally
                             new-x (if is-global?
                                     (- cluster-center (/ (:w n) 2))
                                     (:x n))]
                         (assoc n :x new-x :y new-y)))
                     initial-nodes)

                ;; Vertical Centering Strategy (Layer 2 Processing)
                ;; 1. Group by cluster
                clusters (group-by :swimlane-id (filter #(not (empty? (:swimlane-id %))) spaced-nodes))

                ;; 2. Calculate height of each cluster
                cluster-bounds
                (into {}
                      (map (fn [[id nodes]]
                             (let [min-y (apply min (map :y nodes))
                                   max-y (apply max (map #(+ (:y %) (:h %)) nodes))
                                   height (- max-y min-y)]
                               [id {:min-y min-y :height height}]))
                           clusters))

                ;; 3. Find max height
                max-cluster-height (if (seq cluster-bounds)
                                     (apply max (map (comp :height val) cluster-bounds))
                                     0)

                ;; 4. Calculate shifts for centering
                centered-nodes
                (map (fn [n]
                       (if (empty? (:swimlane-id n))
                         n ;; Global node, untouched
                         (let [id (:swimlane-id n)
                               {:keys [height]} (get cluster-bounds id)
                               shift (/ (- max-cluster-height height) 2)]
                           (update n :y + shift))))
                     spaced-nodes)]

            centered-nodes)
          initial-nodes)

        ;; Recalculate Dimensions based on processed nodes
        width (if (= mode "horizontal")
                (apply max (map #(+ (:x %) (:w %) 50) processed-nodes))
                (reduce + (map :size lanes)))

        height (if (= mode "horizontal")
                 (reduce + (map :size lanes))
                 (apply max (map #(+ (:y %) (:h %) 50) processed-nodes)))

        ;; Construct Swimlane Geometry
        swimlane-geoms
        (if (= layout-mode "cluster")
          ;; Cluster Mode: Calculate Bounding Boxes
          (keep (fn [l]
                  (let [l-nodes (filter #(= (:swimlane-id %) (:id l)) processed-nodes)]
                    (if (and (seq l-nodes) (not (empty? (:id l)))) ;; Skip global lane
                      (let [min-x (apply min (map :x l-nodes))
                            max-x (apply max (map #(+ (:x %) (:w %)) l-nodes))
                            min-y (apply min (map :y l-nodes))
                            max-y (apply max (map #(+ (:y %) (:h %)) l-nodes))
                            padding 30]
                        {:x (- min-x padding)
                         :y (- min-y padding)
                         :w (+ (- max-x min-x) (* 2 padding))
                         :h (+ (- max-y min-y) (* 2 padding))
                         :label (:label l)
                         :props (:props l)
                         :index (:index l)})
                      nil)))
                lanes)

          ;; Normal Mode: Strips
          (map-indexed (fn [i l]
                         (let [offset (nth lane-offsets i)]
                           (if (= mode "horizontal")
                             {:x 0 :y offset :w width :h (:size l)
                              :label (:label l) :index (:index l) :vertical? false
                              :props (:props l)}
                             {:x offset :y 0 :w (:size l) :h height
                              :label (:label l) :index (:index l) :vertical? true
                              :props (:props l)})))
                       lanes))]

    {:nodes processed-nodes
     :edges edges
     :swimlanes swimlane-geoms
     :width width
     :height height}))

;; --- 4. Orthogonal Routing (Manhattan) ---

(defn- route-edges [layout options]
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
                                 (let [{:keys [e p1 p2 mid-x mid-y]} geom
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

;; --- Main Entry ---

(defn layout [nodes edges pools options]
  (let [layout-mode (:layout options)
        ;; Force vertical mode for cluster layout to ensure routing matches coordinate assignment
        options (if (= layout-mode "cluster")
                  (assoc options :swimlane-mode "vertical")
                  options)]
    (-> (assign-coordinates nodes edges pools options)
        (route-edges options))))