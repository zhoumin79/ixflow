(ns xflow.layout.routing.architecture
  (:require [xflow.layout.config :as config]
            [clojure.string :as str]))

;; --- Utilities ---

(defn- is-vertical? [direction]
  (let [d (if (keyword? direction) (name direction) (str direction))]
    (or (= d "tb") (= d "vertical") (= d "") (= d "nil") (= d "bt"))))

(defn- get-node-bounds [node padding]
  {:x1 (- (:x node) padding)
   :y1 (- (:y node) padding)
   :x2 (+ (:x node) (:w node) padding)
   :y2 (+ (:y node) (:h node) padding)})

(defn- distribute-points [start length fixed-coord count axis]
  "Returns a sequence of {:x ... :y ...} points distributed along a line.
   axis: :x (vary x, fixed y) or :y (vary y, fixed x)"
  (if (and (number? start) (number? length) (number? fixed-coord) (number? count) (pos? count))
    (let [step (double (/ length (inc count)))]
      (map (fn [i]
             (let [var-coord (double (+ start (* (inc i) step)))]
               (if (= axis :x)
                 {:x var-coord :y (double fixed-coord)}
                 {:x (double fixed-coord) :y var-coord})))
           (range count)))
    []))

;; --- Port Assignment ---

(defn- analyze-columns [nodes]
  ;; 通过节点中心点的 x 分布，估计“左列/右列”的分割线与两列边界。
  ;; 这样可以为：
  ;; 1) 残差连接提供稳定的“总线通道”(bus)；
  ;; 2) 跨列连接提供稳定的“走廊”(corridor)；
  ;; 从而让 Transformer 这类双列架构的连线更接近常见参考图风格。
  ;;
  ;; FIX: Ignore group nodes to avoid skewing the center calculation
  (let [content-nodes (remove #(= (:type %) :group) nodes)
        centers (->> content-nodes
                     (keep (fn [n]
                             (when (and (number? (:x n)) (number? (:w n)))
                               {:id (:id n)
                                :cx (+ (:x n) (/ (:w n) 2.0))})))
                     (sort-by :cx))
        split-x (if (>= (count centers) 2)
                  (let [pairs (partition 2 1 centers)
                        best (apply max-key (fn [[a b]] (- (:cx b) (:cx a))) pairs)
                        [a b] best]
                    (/ (+ (:cx a) (:cx b)) 2.0))
                  (if-let [c (first centers)]
                    (:cx c)
                    0.0))
        node->col (into {}
                        (map (fn [{:keys [id cx]}]
                               [id (if (<= cx split-x) :left :right)])
                             centers))
        init-bounds {:min-x Double/POSITIVE_INFINITY
                     :max-x Double/NEGATIVE_INFINITY
                     :min-y Double/POSITIVE_INFINITY
                     :max-y Double/NEGATIVE_INFINITY}
        col->bounds (->> content-nodes
                         (reduce
                          (fn [m n]
                            (if (and (number? (:x n)) (number? (:y n)) (number? (:w n)) (number? (:h n)))
                              (let [col (get node->col (:id n) :left)
                                    x1 (double (:x n))
                                    x2 (double (+ (:x n) (:w n)))
                                    y1 (double (:y n))
                                    y2 (double (+ (:y n) (:h n)))]
                                (-> m
                                    (update-in [col :min-x] min x1)
                                    (update-in [col :max-x] max x2)
                                    (update-in [col :min-y] min y1)
                                    (update-in [col :max-y] max y2)))
                              m))
                          {:left init-bounds :right init-bounds}))

        ;; New: Compute swimlane bounds for local routing
        swimlane->bounds
        (reduce (fn [acc n]
                  (let [sid (:swimlane-id n)
                        x (:x n) w (:w n)
                        y (:y n) h (:h n)]
                    (if (and sid x w y h (not= (:type n) :group))
                      (update acc sid
                              (fn [b]
                                (let [b (or b init-bounds)]
                                  {:min-x (min (:min-x b) (double x))
                                   :max-x (max (:max-x b) (double (+ x w)))
                                   :min-y (min (:min-y b) (double y))
                                   :max-y (max (:max-y b) (double (+ y h)))})))
                      acc)))
                {}
                nodes)

        global {:min-x (->> content-nodes (keep :x) (map double) (reduce min 0.0))
                :max-x (->> content-nodes (keep (fn [n] (when (and (number? (:x n)) (number? (:w n))) (+ (:x n) (:w n))))) (map double) (reduce max 0.0))}
        norm-bounds (fn [b]
                      (if (or (Double/isInfinite (:min-x b)) (Double/isInfinite (:max-x b)))
                        (merge b global)
                        b))
        left-b (norm-bounds (:left col->bounds))
        right-b (norm-bounds (:right col->bounds))

        ;; Normalize swimlane bounds
        swimlane->bounds (into {} (map (fn [[k v]] [k (norm-bounds v)]) swimlane->bounds))

        corridor-x (if (< (:max-x left-b) (:min-x right-b))
                     (/ (+ (:max-x left-b) (:min-x right-b)) 2.0)
                     split-x)]
    {:split-x split-x
     :node->col node->col
     :col->bounds {:left left-b :right right-b}
     :swimlane->bounds swimlane->bounds
     :corridor-x corridor-x}))

(defn- assign-ports [nodes edges options]
  (let [direction (:direction options :tb)
        vertical? (is-vertical? direction)
        {:keys [node->col]} (analyze-columns nodes)
        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        classify-edge-port
        (fn [edge is-input?]
          (let [edge-type (:type edge)
                relation (:relation edge)
                from-col (get node->col (:from edge) :left)
                to-col (get node->col (:to edge) :right)
                cross-cols? (not= from-col to-col)
                requested-side (some-> (:side edge) keyword)]
            (cond
              ;; 残差连接：优先 obey DSL 的 side，其次按列默认放到外侧总线。
              (or (= edge-type :residual) (= edge-type "residual"))
              (if vertical?
                (or requested-side (if (= from-col :right) :right :left))
                :bottom)

              ;; 跨列连接：优先走两列中间走廊，因此端口朝向“走廊”侧。
              (or (= edge-type :cross) (= edge-type "cross") (= relation :cross-column) cross-cols?)
              (if vertical?
                (cond
                  (and (= from-col :left) (= to-col :right)) (if is-input? :left :right)
                  (and (= from-col :right) (= to-col :left)) (if is-input? :right :left)
                  :else (if is-input? :left :right))
                :bottom)

              :else
              (if vertical?
                (if (= (if (keyword? direction) (name direction) (str direction)) "bt")
                  (if is-input? :bottom :top)
                  (if is-input? :top :bottom))
                (if is-input? :left :right)))))

        node-side-edges
        (reduce
         (fn [acc edge]
           (let [from-id (:from edge)
                 to-id (:to edge)
                 from-side (classify-edge-port edge false)
                 to-side (classify-edge-port edge true)]
             (-> acc
                 (update-in [from-id from-side] (fnil conj []) {:edge edge :is-input? false})
                 (update-in [to-id to-side] (fnil conj []) {:edge edge :is-input? true}))))
         {}
         edges)

        port-updates
        (reduce-kv
         (fn [acc node-id side-map]
           (let [node (get nodes-map node-id)]
             (if (or (nil? node)
                     (not (every? number? [(:x node) (:y node) (:w node) (:h node)])))
               acc
               (reduce-kv
                (fn [acc2 side items]
                  (let [count (count items)
                        pts (case side
                              :top (distribute-points (:x node) (:w node) (:y node) count :x)
                              :bottom (distribute-points (:x node) (:w node) (+ (:y node) (:h node)) count :x)
                              :left (distribute-points (:y node) (:h node) (:x node) count :y)
                              :right (distribute-points (:y node) (:h node) (+ (:x node) (:w node)) count :y))
                        updates (map (fn [item pt]
                                       {(:id (:edge item))
                                        (if (:is-input? item)
                                          {:target (assoc pt :side side :is-input? true)}
                                          {:source (assoc pt :side side :is-input? false)})})
                                     items
                                     pts)]
                    (into acc2 updates)))
                acc
                side-map))))
         []
         node-side-edges)]

    (reduce
     (fn [acc update-map]
       (let [[edge-id data] (first update-map)]
         (update acc edge-id merge data)))
     {}
     port-updates)))

;; --- Routing Logic ---

(defn- clean-points [points]
  ;; 清理走线点集：
  ;; - 去除连续重复点\n  ;; - 去除共线的中间点（水平/垂直）
  ;; 这样可以避免 rounded-path 在拐点处生成“回头”的退化圆角。
  (let [dedup (reduce (fn [acc p]
                        (let [lp (peek acc)]
                          (if (and lp (= (:x lp) (:x p)) (= (:y lp) (:y p)))
                            acc
                            (conj acc p))))
                      []
                      points)]
    (loop [src dedup
           out []]
      (if (<= (count src) 2)
        (into out src)
        (let [[a b c & more] src
              colinear? (or (and (= (:x a) (:x b)) (= (:x b) (:x c)))
                            (and (= (:y a) (:y b)) (= (:y b) (:y c))))]
          (if colinear?
            (recur (cons a (cons c more)) out)
            (recur (cons b (cons c more)) (conj out a))))))))

(defn- route-segment-standard [p1 p2 direction]
  ;; Simple orthogonal routing (Z-shape or straight)
  (let [vertical? (is-vertical? direction)]
    (cond
      ;; Straight
      (or (< (Math/abs (double (- (:x p1) (:x p2)))) 1.0)
          (< (Math/abs (double (- (:y p1) (:y p2)))) 1.0))
      [p1 p2]

      vertical?
      ;; Vertical (TB/BT): Split Y
      (let [mid-y (double (/ (+ (:y p1) (:y p2)) 2))]
        [p1
         {:x (double (:x p1)) :y mid-y}
         {:x (double (:x p2)) :y mid-y}
         p2])

      :else
      ;; Horizontal
      (let [mid-x (double (/ (+ (:x p1) (:x p2)) 2))]
        [p1
         {:x mid-x :y (double (:y p1))}
         {:x mid-x :y (double (:y p2))}
         p2]))))

(defn- route-segment-residual [edge p1 p2 direction env]
  ;; 残差连接走“总线通道”：同一列的残差尽量沿列外侧并行走线，减少穿越节点。
  (let [vertical? (is-vertical? direction)
        stagger (:stagger env 0.0)
        stub (+ 18.0 stagger)
        bus-margin 30.0
        node->col (:node->col env)
        col->bounds (:col->bounds env)
        swimlane->bounds (:swimlane->bounds env)
        nodes-map (:nodes-map env)

        from-id (:from edge)
        to-id (:to edge)
        from-node (get nodes-map from-id)
        to-node (get nodes-map to-id)

        ;; Check if local routing (same swimlane) is possible
        same-swimlane? (and from-node to-node
                            (:swimlane-id from-node)
                            (= (:swimlane-id from-node) (:swimlane-id to-node)))

        ;; Select bounds: Local (Swimlane) or Global (Column)
        local-bounds (when same-swimlane? (get swimlane->bounds (:swimlane-id from-node)))
        b (or local-bounds (get col->bounds (get node->col from-id :left)))

        ;; Determine channel X
        ;; If local, we want to stay inside the group but outside the nodes.
        ;; If group padding is 40, and bus-margin is 30, it fits.
        ;; However, we need to decide left/right side.
        ;; Default to side requested, or determine by column.

        from-col (get node->col from-id :left)
        channel-side (or (some-> (:side p1) keyword)
                         (if (= from-col :right) :right :left))

        channel-x (case channel-side
                    :right (+ (:max-x b) bus-margin stagger)
                    :left (- (:min-x b) bus-margin stagger)
                    ;; Fallback
                    (+ (:max-x b) bus-margin stagger))

        p1-safe (case (:side p1)
                  :left {:x (- (:x p1) stub) :y (:y p1)}
                  :right {:x (+ (:x p1) stub) :y (:y p1)}
                  :top {:x (:x p1) :y (- (:y p1) stub)}
                  :bottom {:x (:x p1) :y (+ (:y p1) stub)}
                  p1)
        p2-safe (case (:side p2)
                  :left {:x (- (:x p2) stub) :y (:y p2)}
                  :right {:x (+ (:x p2) stub) :y (:y p2)}
                  :top {:x (:x p2) :y (- (:y p2) stub)}
                  :bottom {:x (:x p2) :y (+ (:y p2) stub)}
                  p2)]
    (if vertical?
      [p1
       p1-safe
       {:x channel-x :y (:y p1-safe)}
       {:x channel-x :y (:y p2-safe)}
       p2-safe
       p2]
      [p1 p2])))

(defn- route-segment-cross [edge p1 p2 direction env]
  ;; 跨列连接走“中间走廊”：尽量在两列之间垂直穿行，避免切入列内部。
  (let [vertical? (is-vertical? direction)]
    (if vertical?
      (let [corridor-x (double (:corridor-x env))
            stub 18.0
            p1-safe (if (< (:x p1) corridor-x)
                      {:x (+ (:x p1) stub) :y (:y p1)}
                      {:x (- (:x p1) stub) :y (:y p1)})
            p2-safe (if (< (:x p2) corridor-x)
                      {:x (+ (:x p2) stub) :y (:y p2)}
                      {:x (- (:x p2) stub) :y (:y p2)})]
        [p1
         p1-safe
         {:x corridor-x :y (double (:y p1-safe))}
         {:x corridor-x :y (double (:y p2-safe))}
         p2-safe
         p2])
      (route-segment-standard p1 p2 direction))))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        direction (:direction options "tb")
        ;; 环境信息：列划分、列边界与走廊位置，用于统一走线风格
        env0 (analyze-columns nodes)
        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes)) ;; Build nodes map

        ;; 1. Assign Ports
        port-assignments (assign-ports nodes edges options)

        ;; 2. Bounds（兜底）
        valid-nodes (filter (fn [n] (and (number? (:x n)) (number? (:w n))
                                         (number? (:y n)) (number? (:h n))))
                            nodes)
        bounds (if (seq valid-nodes)
                 {:min-x (apply min (map :x valid-nodes))
                  :max-x (apply max (map #(+ (:x %) (:w %)) valid-nodes))
                  :min-y (apply min (map :y valid-nodes))
                  :max-y (apply max (map #(+ (:y %) (:h %) 0) valid-nodes))}
                 {:min-x 0 :max-x 0 :min-y 0 :max-y 0})

        ;; 3. Route Each Edge
        routed-edges
        (map-indexed
         (fn [idx edge]
           (let [ports (get port-assignments (:id edge))]
             (if (or (nil? ports) (nil? (:source ports)) (nil? (:target ports)))
               (do (println "WARNING: Incomplete ports for edge:" (:id edge) "from:" (:from edge) "to:" (:to edge))
                   edge)
               (let [source (assoc (:source ports)
                                   :x (double (:x (:source ports)))
                                   :y (double (:y (:source ports))))
                     target (assoc (:target ports)
                                   :x (double (:x (:target ports)))
                                   :y (double (:y (:target ports))))
                     edge-type (:type edge)
                     stagger (* (mod idx 3) 5.0)
                     ;; Add nodes-map to env
                     env (assoc env0 :bounds bounds :stagger stagger :nodes-map nodes-map)]
                 (let [pts (cond
                             (or (= edge-type :residual) (= edge-type "residual"))
                             (route-segment-residual edge source target direction env)

                             (or (= edge-type :cross) (= edge-type "cross"))
                             (route-segment-cross edge source target direction env)

                             :else
                             (route-segment-standard source target direction))]
                   (assoc edge :points (clean-points pts)))))))
         edges)]

    (assoc layout :edges routed-edges)))
