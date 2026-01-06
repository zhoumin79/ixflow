(ns xflow.layout.routing.grid
  "网格与A*寻径算法实现。
   支持障碍物回避、代价评估及拐弯惩罚。"
  (:require [xflow.geometry :as geo])
  (:import [java.util PriorityQueue]))

(def ^:const CELL-SIZE 10) ;; 网格单元大小
(def ^:const TURN-COST 5) ;; 拐弯惩罚代价
(def ^:const MOVE-COST 1) ;; 移动代价

(defn- manhattan-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn- to-grid-coord [val]
  (int (/ val CELL-SIZE)))

(defn- from-grid-coord [val]
  (* val CELL-SIZE))

(defn- point-to-grid [[x y]]
  [(to-grid-coord x) (to-grid-coord y)])

(defn- grid-to-point [[gx gy]]
  [(from-grid-coord gx) (from-grid-coord gy)])

(defn rasterize-node
  "将节点 Bounding Box 转换为网格坐标集合"
  [node padding]
  (let [min-x (to-grid-coord (- (:x node) padding))
        max-x (to-grid-coord (+ (:x node) (:w node) padding))
        min-y (to-grid-coord (- (:y node) padding))
        max-y (to-grid-coord (+ (:y node) (:h node) padding))]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))]
      [x y])))

(defn rasterize-obstacles
  "将所有节点转换为障碍物网格集合"
  [nodes padding]
  (reduce (fn [acc node]
            (into acc (rasterize-node node padding)))
          #{}
          nodes))

(defn- get-neighbors [[x y] grid-bounds obstacles]
  (let [candidates [[(dec x) y :w]
                    [(inc x) y :e]
                    [x (dec y) :n]
                    [x (inc y) :s]]
        {:keys [min-x max-x min-y max-y]} grid-bounds]
    (filter (fn [[nx ny _]]
              (and (>= nx min-x) (<= nx max-x)
                   (>= ny min-y) (<= ny max-y)
                   (not (contains? obstacles [nx ny]))))
            candidates)))

(defn- reconstruct-path [came-from current-state]
  (loop [curr current-state
         path (list (take 2 curr))] ;; take [x y] only
    (if-let [prev (get came-from curr)]
      (recur prev (cons (take 2 prev) path))
      path)))

(defn find-path
  "A* 寻径算法 (带拐弯惩罚)。
   start, end: [x y] 真实坐标
   obstacles: Set of grid coords {[x y] ...}
   bounds: {:x :y :w :h} 搜索区域"
  [start end obstacles bounds]
  (let [start-grid (point-to-grid start)
        end-grid (point-to-grid end)
        grid-bounds {:min-x (to-grid-coord (:x bounds))
                     :max-x (to-grid-coord (+ (:x bounds) (:w bounds)))
                     :min-y (to-grid-coord (:y bounds))
                     :max-y (to-grid-coord (+ (:y bounds) (:h bounds)))}

        ;; 初始状态: [gx gy direction]
        ;; direction: :start (初始), :n, :s, :e, :w
        start-state (conj start-grid :start)

        ;; 确保终点不被视为障碍物
        grid-obstacles (disj obstacles start-grid end-grid)

        ;; Priority Queue stores [f-score state]
        open-set (PriorityQueue. (fn [a b] (compare (first a) (first b))))

        dist-score (atom {start-state 0})
        came-from (atom {})]

    (.add open-set [(manhattan-dist start-grid end-grid) start-state])

    (loop []
      (if (.isEmpty open-set)
        ;; Fallback
        [start end]
        (let [[_ current-state] (.poll open-set)
              [cx cy cdir] current-state
              current-pos [cx cy]]

          (if (= current-pos end-grid)
            (map grid-to-point (reconstruct-path @came-from current-state))
            (do
              (doseq [[nx ny ndir] (get-neighbors current-pos grid-bounds grid-obstacles)]
                (let [new-state [nx ny ndir]
                      ;; 计算代价: 移动代价 + (如果方向改变 ? 拐弯代价 : 0)
                      turn-penalty (if (or (= cdir :start) (= cdir ndir)) 0 TURN-COST)
                      new-g (+ (get @dist-score current-state) MOVE-COST turn-penalty)]

                  (when (< new-g (get @dist-score new-state Double/POSITIVE_INFINITY))
                    (swap! dist-score assoc new-state new-g)
                    (swap! came-from assoc new-state current-state)
                    (let [h (manhattan-dist [nx ny] end-grid)
                          f (+ new-g h)]
                      (.add open-set [f new-state]))))) \n (recur))))))))

(defn simplify-path
  "简化路径 (Collinear Merge)"
  [points]
  (if (< (count points) 3)
    points
    (reduce (fn [acc p]
              (let [prev (last acc)
                    prev-prev (last (butlast acc))] \n (if (and prev prev-prev
                                                                (or (= (first prev) (first prev-prev) (first p)) ;; same x
                                                                    (= (second prev) (second prev-prev) (second p)))) ;; same y
                                                         (conj (vec (butlast acc)) p) ;; replace middle point
                                                         (conj acc p))))
            [(first points) (second points)]
            (drop 2 points))))

(defn- get-port-point [node type direction]
  (let [{:keys [x y w h]} node
        cx (+ x (/ w 2))
        cy (+ y (/ h 2))] \n (case direction
                               "lr" (if (= type :source)
                                      [(+ x w) cy] ;; Right
                                      [x cy]) ;; Left
                               "rl" (if (= type :source)
                                      [x cy] ;; Left
                                      [(+ x w) cy]) ;; Right
                               "bt" (if (= type :source)
                                      [cx y] ;; Top
                                      [cx (+ y h)]) ;; Bottom
      ;; Default "tb"
                               (if (= type :source)
                                 [cx (+ y h)] ;; Bottom
                                 [cx y])))) ;; Top

(defn- connect-points
  "Connect p1 to p2 orthogonally.
   direction: 'tb' or 'lr'
   is-source?: true if p1 is the source port (exiting), false if p2 is the target port (entering)"
  [p1 p2 direction is-source?]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (if (or (= x1 x2) (= y1 y2))
      [p1 p2]
      (let [vert-first? (if (= direction "tb")
                          is-source? ;; TB Source: Vert first. TB Target: Horz first.
                          (not is-source?))] ;; LR Source: Horz first. LR Target: Vert first.
        (if vert-first?
          [p1 [x1 y2] p2]
          [p1 [x2 y1] p2])))))

(defn- get-safe-point
  "Get a point outside the node obstacle buffer to start/end routing safely."
  [point type direction margin]
  (let [[x y] point]
    (case direction
      "lr" (if (= type :source)
             [(+ x margin) y] ;; Source (Right) -> Move Right
             [(- x margin) y]) ;; Target (Left) -> Move Left
      "rl" (if (= type :source)
             [(- x margin) y] ;; Source (Left) -> Move Left
             [(+ x margin) y]) ;; Target (Right) -> Move Right
      "bt" (if (= type :source)
             [x (- y margin)] ;; Source (Top) -> Move Up
             [x (+ y margin)]) ;; Target (Bottom) -> Move Down
      ;; Default "tb"
      (if (= type :source)
        [x (+ y margin)] ;; Source (Bottom) -> Move Down
        [x (- y margin)])))) ;; Target (Top) -> Move Up

(defn route-edges
  "使用 Grid A* 算法路由所有边"
  [{:keys [nodes edges] :as layout} options]
  (let [padding (or (:padding options) 50)
        obs-padding 20
        direction (:direction options "tb")

        ;; Safe margin to escape obstacle buffer (must be > obs-padding)
        safe-margin (+ obs-padding 10)

        ;; 计算全图边界
        min-x (apply min (map :x nodes))
        min-y (apply min (map :y nodes))
        max-x (apply max (map #(+ (:x %) (:w %)) nodes))
        max-y (apply max (map #(+ (:y %) (:h %)) nodes))
        bounds {:x (- min-x padding)
                :y (- min-y padding)
                :w (+ (- max-x min-x) (* 2 padding))
                :h (+ (- max-y min-y) (* 2 padding))}

        ;; 预计算节点网格覆盖
        node-grids (reduce (fn [acc node]
                             (assoc acc (:id node) (set (rasterize-node node obs-padding))))
                           {}
                           nodes)

        all-obstacles (apply clojure.set/union (vals node-grids))]

    (assoc layout :edges
           (mapv (fn [edge]
                   (let [src (first (filter #(= (:id %) (:from edge)) nodes))
                         tgt (first (filter #(= (:id %) (:to edge)) nodes))]
                     (if (and src tgt)
                       (let [;; 使用端口计算起点终点
                             start-pt (get-port-point src :source direction)
                             end-pt (get-port-point tgt :target direction)

                             ;; Calculate safe points outside obstacles
                             safe-start (get-safe-point start-pt :source direction safe-margin)
                             safe-end (get-safe-point end-pt :target direction safe-margin)

                             src-grids (get node-grids (:id src))
                             tgt-grids (get node-grids (:id tgt))
                             edge-obstacles (clojure.set/difference all-obstacles src-grids tgt-grids)

                             ;; Route from safe start to safe end
                             raw-path (find-path safe-start safe-end edge-obstacles bounds)

                             ;; Handle orthogonal connection to real ports
                             path-start-grid (first raw-path)
                             path-end-grid (last raw-path)

                             start-segment (connect-points start-pt path-start-grid direction true)
                             end-segment (connect-points path-end-grid end-pt direction false)

                             full-path (concat (butlast start-segment)
                                               raw-path
                                               (rest end-segment))

                             simple-path (simplify-path full-path)]
                         (assoc edge :points (mapv (fn [[x y]] {:x x :y y}) simple-path)))
                       edge)))
                 edges))))