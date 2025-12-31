(ns xflow.layout.strategy.swimlane
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def NODE-WIDTH 150)
(def NODE-HEIGHT 80)
(def NODE-SEP 50)
(def LAYER-SEP 100)

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

(defn group-by-swimlane [nodes pools options]
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

(defn calculate-lane-dimensions [lanes mode]
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

(defn assign-initial-coordinates [ranked-nodes lanes options]
  (let [mode (:swimlane-mode options "horizontal")
        ;; Map lane-id to lane-spec
        lane-map (into {} (map (fn [l] [(:id l) l]) lanes))

        ;; Calculate Lane Offsets
        lane-offsets (reductions + 0 (map :size lanes))
        lane-offset-map (zipmap (map :id lanes) lane-offsets)

        ;; -- Collision avoidance --
        rank-groups (group-by (fn [n] [(:swimlane-id n) (:rank n)]) ranked-nodes)
        rank-groups (into {} rank-groups) ;; No sorting, respect DSL order

        ;; Calculate Node Positions
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
                        :x (double x) :y (double y)
                        :w (double NODE-WIDTH) :h (double NODE-HEIGHT))))
             ranked-nodes)]
    initial-nodes))

(defn calculate-strip-geometries [lanes width height mode]
  (let [lane-offsets (reductions + 0 (map :size lanes))]
    (map-indexed (fn [i l]
                   (let [offset (nth lane-offsets i)]
                     (if (= mode "horizontal")
                       {:x (double 0) :y (double offset) :w (double width) :h (double (:size l))
                        :label (:label l) :index (:index l) :vertical? false
                        :props (:props l)}
                       {:x (double offset) :y (double 0) :w (double (:size l)) :h (double height)
                        :label (:label l) :index (:index l) :vertical? true
                        :props (:props l)})))
                 lanes)))

(defn layout [nodes edges swimlanes options]
  {:nodes nodes :edges edges :swimlanes swimlanes :width 100 :height 100})
