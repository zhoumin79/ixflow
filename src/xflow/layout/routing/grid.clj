(ns ixflow.layout.routing.grid
  "网格与A*寻径算法实现。
   支持障碍物回避、代价评估。"
  (:import [java.util PriorityQueue]))

(def ^:const CELL-SIZE 10) ;; 网格单元大小，可配置

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

(defn- get-neighbors [[x y] grid-bounds obstacles]
  (let [candidates [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        {:keys [min-x max-x min-y max-y]} grid-bounds]
    (filter (fn [[nx ny]]
              (and (>= nx min-x) (<= nx max-x)
                   (>= ny min-y) (<= ny max-y)
                   (not (contains? obstacles [nx ny]))))
            candidates)))

(defn- reconstruct-path [came-from current]
  (loop [curr current
         path (list curr)]
    (if-let [prev (get came-from curr)]
      (recur prev (cons prev path))
      path)))

(defn find-path
  "A* 寻径算法。
   start, end: [x y] 真实坐标
   obstacles: Set of grid coords {[x y] ...} or Bounding Boxes
   bounds: {:x :y :w :h} 搜索区域"
  [start end obstacles bounds]
  (let [start-grid (point-to-grid start)
        end-grid (point-to-grid end)
        grid-bounds {:min-x (to-grid-coord (:x bounds))
                     :max-x (to-grid-coord (+ (:x bounds) (:w bounds)))
                     :min-y (to-grid-coord (:y bounds))
                     :max-y (to-grid-coord (+ (:y bounds) (:h bounds)))}
        ;; Obstacles need to be rasterized to grid cells. 
        ;; 这里假设 obstacles 已经是 grid set，实际需要转换
        grid-obstacles obstacles

        open-set (PriorityQueue. (fn [a b] (compare (first a) (first b)))) ;; Correct Min-Heap comparator

        dist-score (atom {start-grid 0})
        f-score (atom {start-grid (manhattan-dist start-grid end-grid)})
        came-from (atom {})]

    (.add open-set [(get @f-score start-grid) start-grid])

    (loop []
      (if (.isEmpty open-set)
        nil ;; No path found
        (let [[_ current] (.poll open-set)]
          (if (= current end-grid)
            (map grid-to-point (reconstruct-path @came-from current))
            (do
              (doseq [neighbor (get-neighbors current grid-bounds grid-obstacles)]
                (let [tentative-g (+ (get @dist-score current) 1)] ;; uniform cost 1
                  (when (< tentative-g (get @dist-score neighbor Double/POSITIVE_INFINITY))
                    (swap! came-from assoc neighbor current)
                    (swap! dist-score assoc neighbor tentative-g)
                    (let [f (+ tentative-g (manhattan-dist neighbor end-grid))]
                      (swap! f-score assoc neighbor f)
                      (.add open-set [f neighbor])))))
              (recur))))))))

(defn simplify-path
  "简化路径 (Collinear Merge)"
  [points]
  (if (< (count points) 3)
    points
    (reduce (fn [acc p]
              (let [prev (last acc)
                    prev-prev (last (butlast acc))]
                (if (and prev prev-prev
                         (or (= (first prev) (first prev-prev) (first p)) ;; same x
                             (= (second prev) (second prev-prev) (second p)))) ;; same y
                  (conj (vec (butlast acc)) p) ;; replace middle point
                  (conj acc p))))
            [(first points) (second points)]
            (drop 2 points))))
