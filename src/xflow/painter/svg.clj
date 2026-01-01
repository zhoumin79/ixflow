(ns xflow.painter.svg
  (:require [clojure.string :as str]))

(defn escape-html [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))))

(def icon-map
  {"file-text" "📝"
   "bug" "🐛"
   "copy" "📄"
   "repeat" "🔁"
   "zap" "⚡"
   "check-square" "☑️"
   "package" "📦"
   "send" "📤"
   "log-in" "🔐"
   "check-circle" "✅"
   "file-plus" "➕"
   "upload" "📤"
   "dollar-sign" "💲"
   "x-circle" "❌"
   "calendar" "📅"
   "filter" "🌪️"
   "check" "✔️"
   "thumbs-up" "👍"
   "credit-card" "💳"
   "eye" "👁️"
   "list" "📋"
   "map" "🗺️"
   "cpu" "⚙️"
   "code" "💻"})

(declare curve-path)

(defn- rounded-path [points radius]
  (if (< (count points) 3)
    ;; Fallback for simple lines
    (str "M" (:x (first points)) "," (:y (first points)) " "
         (str/join " " (map (fn [p] (str "L" (:x p) "," (:y p))) (rest points))))

    ;; Generate path with curves
    (let [start (first points)
          path-cmds (atom [(str "M" (:x start) "," (:y start))])]
      (dotimes [i (- (count points) 2)]
        (let [curr (nth points (inc i))
              prev (nth points i)
              next (nth points (+ i 2))

              ;; Vectors
              dx1 (- (:x curr) (:x prev))
              dy1 (- (:y curr) (:y prev))
              len1 (Math/sqrt (+ (* dx1 dx1) (* dy1 dy1)))

              dx2 (- (:x next) (:x curr))
              dy2 (- (:y next) (:y curr))
              len2 (Math/sqrt (+ (* dx2 dx2) (* dy2 dy2)))

              ;; Effective radius (limit by segment length)
              r (min radius (/ len1 2) (/ len2 2))

              ;; Start of curve (backed off from corner)
              p1-x (- (:x curr) (* r (/ dx1 len1)))
              p1-y (- (:y curr) (* r (/ dy1 len1)))

              ;; End of curve (forward from corner)
              p2-x (+ (:x curr) (* r (/ dx2 len2)))
              p2-y (+ (:y curr) (* r (/ dy2 len2)))]

          (swap! path-cmds conj (str "L" p1-x "," p1-y))
          (swap! path-cmds conj (str "Q" (:x curr) "," (:y curr) " " p2-x "," p2-y))))

      ;; Final segment
      (let [last-pt (last points)]
        (swap! path-cmds conj (str "L" (:x last-pt) "," (:y last-pt))))

      (str/join " " @path-cmds))))

(defn- basis-spline-path [points]
  (if (< (count points) 3)
    (curve-path points) ;; Fallback to simple curve for short paths

    (let [;; Duplicate start and end points to clamp the curve
          pts (vec (concat [(first points)] points [(last points)]))
          n (count pts)

          ;; Start path at first point
          start (nth pts 1) ;; Original first point
          path-cmds (atom [(str "M" (:x start) "," (:y start))])]

      ;; Iterate through windows of 4 points to generate segments
      ;; B-Spline segment uses points i, i+1, i+2, i+3
      ;; We iterate from 0 to n-4
      (dotimes [i (- n 3)]
        (let [p0 (nth pts i)
              p1 (nth pts (inc i))
              p2 (nth pts (+ i 2))
              p3 (nth pts (+ i 3))

              ;; B-Spline Basis Matrix to Bezier Control Points
              ;; S = (P0 + 4P1 + P2) / 6
              ;; C1 = (2P1 + P2) / 3   <- Wait, this is relative to segment
              ;; Actually, the segment defined by P0..P3 goes from S to E
              ;; where S is near P1 and E is near P2.
              ;; 
              ;; Correct Bezier Control Points for segment i:
              ;; Start point S: (1/6)P0 + (2/3)P1 + (1/6)P2
              ;; End point E:   (1/6)P1 + (2/3)P2 + (1/6)P3
              ;; Ctrl 1:        (2/3)P1 + (1/3)P2
              ;; Ctrl 2:        (1/3)P1 + (2/3)P2

              x1 (+ (* (/ 1.0 6.0) (:x p0)) (* (/ 2.0 3.0) (:x p1)) (* (/ 1.0 6.0) (:x p2)))
              y1 (+ (* (/ 1.0 6.0) (:y p0)) (* (/ 2.0 3.0) (:y p1)) (* (/ 1.0 6.0) (:y p2)))

              x2 (+ (* (/ 1.0 6.0) (:x p1)) (* (/ 2.0 3.0) (:x p2)) (* (/ 1.0 6.0) (:x p3)))
              y2 (+ (* (/ 1.0 6.0) (:y p1)) (* (/ 2.0 3.0) (:y p2)) (* (/ 1.0 6.0) (:y p3)))

              cp1-x (+ (* (/ 2.0 3.0) (:x p1)) (* (/ 1.0 3.0) (:x p2)))
              cp1-y (+ (* (/ 2.0 3.0) (:y p1)) (* (/ 1.0 3.0) (:y p2)))

              cp2-x (+ (* (/ 1.0 3.0) (:x p1)) (* (/ 2.0 3.0) (:x p2)))
              cp2-y (+ (* (/ 1.0 3.0) (:y p1)) (* (/ 2.0 3.0) (:y p2)))]

          ;; For the first segment (i=0), the start point x1,y1 should be exactly P1 (original start)
          ;; Because we duplicated P0=P1, calculation:
          ;; S = 1/6 P1 + 2/3 P1 + 1/6 P2 = 5/6 P1 + 1/6 P2. Not exactly P1.
          ;; Standard Basis Spline doesn't hit control points.
          ;; But if we triple the end points, it does.
          ;; Let's try tripling start/end in `pts` definition.

          ;; If we just use L for start/end segments?
          ;; Or just accept the standard Basis Spline approximation.
          ;; Let's use the calculated points.

          ;; Note: We need to Move to start of first segment if not already there?
          ;; The SVG path is continuous.
          (if (zero? i)
            (swap! path-cmds conj (str "L" x1 "," y1))) ;; Connect from M(original) to S(spline start)

          (swap! path-cmds conj (str "C" cp1-x "," cp1-y " " cp2-x "," cp2-y " " x2 "," y2))))

      ;; Connect to final point
      (let [end (last points)]
        (swap! path-cmds conj (str "L" (:x end) "," (:y end))))

      (str/join " " @path-cmds))))

(defn- curve-path [points]
  (let [pts (vec points)
        n (count pts)]
    (cond
      (< n 2) ""
      (= n 2)
      ;; Use optimized S-curve for direct connections
      (let [p0 (nth pts 0)
            p1 (nth pts 1)
            dx (- (:x p1) (:x p0))
            dy (- (:y p1) (:y p0))
            vertical? (>= (Math/abs (double dy)) (Math/abs (double dx)))
            pull (double (min 80.0 (* 0.4 (if vertical? (Math/abs (double dy)) (Math/abs (double dx))))))
            sgn (fn [v] (if (neg? v) -1.0 1.0))
            [cp1-x cp1-y cp2-x cp2-y]
            (if vertical?
              [(:x p0) (+ (:y p0) (* pull (sgn dy)))
               (:x p1) (- (:y p1) (* pull (sgn dy)))]
              [(+ (:x p0) (* pull (sgn dx))) (:y p0)
               (- (:x p1) (* pull (sgn dx))) (:y p1)])]
        (str "M" (:x p0) "," (:y p0) " "
             "C" cp1-x "," cp1-y " " cp2-x "," cp2-y " " (:x p1) "," (:y p1)))

      :else
      ;; Use Basis Spline for multi-point paths (Mermaid style)
      ;; But first check if it's an Orthogonal path (Manhattan)
      ;; Orthogonal paths look better with Rounded Corners.
      ;; Non-orthogonal (Sugiyama) paths look better with Basis Spline.
      (let [is-ortho? (every? (fn [i]
                                (let [p1 (nth pts i)
                                      p2 (nth pts (inc i))]
                                  (or (= (:x p1) (:x p2)) (= (:y p1) (:y p2)))))
                              (range (dec n)))]
        (if is-ortho?
          (rounded-path points 20)
          (basis-spline-path points))))))

(defn- strip-quotes [s]
  (if (string? s)
    (-> s
        str/trim
        (str/replace #"^\"+|\"+$" ""))
    s))

(defn- calculate-label-pos [points]
  (let [cnt (count points)]
    (if (= cnt 2)
      ;; Midpoint of segment
      (let [p0 (first points)
            p1 (second points)
            ;; Initial guess at 0.4
            t 0.4
            pos-x (+ (:x p0) (* (- (:x p1) (:x p0)) t))
            pos-y (+ (:y p0) (* (- (:y p1) (:y p0)) t))

            ;; Heuristic to avoid overlap with target (p1)
            dx (- (:x p1) (:x p0))
            dy (- (:y p1) (:y p0))
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))]

        ;; If distance is small, ensure we are far enough from p1
        (if (> (Math/abs dy) (Math/abs dx))
          ;; Vertical layout: check Y distance
          (let [safe-y (- (:y p1) 35)] {:x pos-x :y (min pos-y safe-y)})
          ;; Horizontal layout: check X distance (assuming L->R)
          (if (> dx 0)
            (let [safe-x (- (:x p1) 40)] {:x (min pos-x safe-x) :y pos-y})
            {:x pos-x :y pos-y}))) ;; Backwards/other cases, leave as is

      ;; Multi-point path (Spline or Manhattan)
      (let [mid-idx (int (/ cnt 2))]
        (if (odd? cnt)
          (nth points mid-idx)
          (let [p1 (nth points (dec mid-idx))
                p2 (nth points mid-idx)]
            {:x (/ (+ (:x p1) (:x p2)) 2)
             :y (/ (+ (:y p1) (:y p2)) 2)}))))))

(defn render-svg [layout]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        swimlanes (:swimlanes layout)
        width (:width layout 800)
        height (:height layout 600)
        config (:config layout)
        title (:title config)
        layout-mode (:layout config)
        group-node? (fn [n] (= (:type n) :group))
        group-nodes (filter group-node? nodes)
        atomic-nodes (remove group-node? nodes)
        render-node
        (fn [n {:keys [shadow?]}]
          (when (and (:x n) (:y n))
            (let [shape (or (-> n :props :shape) "rect")
                  bg-color (or (-> n :props :fill) (-> n :props :color) "white")
                  stroke-color (or (-> n :props :stroke) "#333")
                  w (or (:w n) 100)
                  h (or (:h n) 50)
                  cx (+ (:x n) (/ w 2))
                  cy (+ (:y n) (/ h 2))
                  icon-key (-> n :props :icon)
                  icon-char (get icon-map icon-key)
                  g-attrs (cond-> {}
                            shadow? (assoc :filter "url(#drop-shadow)"))]
              [:g g-attrs
               (case shape
                 "diamond" [:polygon {:points (str cx "," (:y n) " " (+ (:x n) w) "," cy " " cx "," (+ (:y n) h) " " (:x n) "," cy)
                                      :fill bg-color :stroke stroke-color :stroke-width 1.5}]
                 "oval" [:rect {:x (:x n) :y (:y n) :width w :height h :rx (/ h 2) :ry (/ h 2)
                                :fill bg-color :stroke stroke-color :stroke-width 1.5}]
                 [:rect {:x (:x n) :y (:y n) :width w :height h
                         :fill bg-color
                         :stroke stroke-color :rx 6 :stroke-width 1.5}])

               [:text {:x cx :y cy :text-anchor "middle" :dominant-baseline "middle" :font-size 12 :font-weight "bold" :fill "#222"}
                (escape-html (strip-quotes (or (-> n :props :label) (:id n))))]

               (when icon-char
                 [:g
                  [:circle {:cx (:x n) :cy (:y n) :r 12 :fill "white" :stroke "#333" :stroke-width 1}]
                  [:text {:x (:x n) :y (:y n) :text-anchor "middle" :dominant-baseline "middle" :font-size 14} icon-char]])])))]

    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width width :height (+ height (if title 50 0))
           :viewBox (str "0 0 " width " " (+ height (if title 50 0)))
           :style "font-family: sans-serif;"}

     [:defs
      [:marker {:id "arrow" :markerWidth 10 :markerHeight 10 :refX 8 :refY 3 :orient "auto" :markerUnits "strokeWidth"}
       [:path {:d "M0,0 L0,6 L9,3 z" :fill "#555"}]]

      [:filter {:id "drop-shadow" :x "-20%" :y "-20%" :width "150%" :height "150%"}
       [:feGaussianBlur {:in "SourceAlpha" :stdDeviation "2" :result "blur"}]
       [:feOffset {:in "blur" :dx "2" :dy "2" :result "offsetBlur"}]
       [:feComponentTransfer
        [:feFuncA {:type "linear" :slope "0.3"}]]
       [:feMerge
        [:feMergeNode]
        [:feMergeNode {:in "SourceGraphic"}]]]]

     (when title
       [:text {:x (/ width 2) :y 30 :text-anchor "middle" :font-size 20 :font-weight "bold" :fill "#333"} (escape-html title)])

     [:g {:transform (if title "translate(0, 50)" "translate(0, 0)")}
      (if (= layout-mode "cluster")
        (for [lane swimlanes]
          (let [bg-color (or (-> lane :props :color) "#f8f9fa")]
            [:g
             [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
                     :fill bg-color :fill-opacity 0.3
                     :stroke (or (-> lane :props :color) "#dee2e6") :stroke-width 2
                     :rx 10 :ry 10}]
             [:rect {:x (:x lane) :y (:y lane) :width 100 :height 25 :rx 5 :ry 5 :fill "white" :stroke "#dee2e6"}]
             [:text {:x (+ (:x lane) 10) :y (+ (:y lane) 17) :font-weight "bold" :fill "#495057" :font-size 12} (escape-html (:label lane))]]))

        (for [lane swimlanes]
          (let [header-color (or (-> lane :props :color) "#e9ecef")]
            [:g
             [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
                     :fill (if (even? (:index lane)) "#f8f9fa" "#ffffff")
                     :stroke "#dee2e6" :stroke-width 1}]
             (if (:vertical? lane)
               [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height 30 :fill header-color :stroke "#dee2e6"}]
               [:rect {:x (:x lane) :y (:y lane) :width 30 :height (:h lane) :fill header-color :stroke "#dee2e6"}])
             (if (:vertical? lane)
               [:text {:x (+ (:x lane) (/ (:w lane) 2)) :y (+ (:y lane) 20) :text-anchor "middle" :font-weight "bold" :fill "#495057"} (escape-html (:label lane))]
               [:text {:x (+ (:x lane) 15) :y (+ (:y lane) (/ (:h lane) 2))
                       :text-anchor "middle" :dominant-baseline "middle"
                       :transform (str "rotate(-90, " (+ (:x lane) 15) ", " (+ (:y lane) (/ (:h lane) 2)) ")")
                       :font-weight "bold" :fill "#495057"} (escape-html (:label lane))])])))

      ;; 分组节点（容器）需要先绘制，否则其填充会遮挡内部连线
      (for [n group-nodes]
        (render-node n {:shadow? false}))

      (for [e edges]
        (let [points (:points e)]
          (when points
            (let [d (if (= (:routing-type e) :spline)
                      (curve-path points)
                      (rounded-path points 10))]
              [:g
               (let [edge-type (:type e)
                     dashed? (or (= edge-type :dashed) (= edge-type :cross) (= edge-type "cross"))
                     default-stroke (cond
                                      (or (= edge-type :residual) (= edge-type "residual")) "#666"
                                      dashed? "#777"
                                      :else "#555")
                     default-width (cond
                                     (or (= edge-type :residual) (= edge-type "residual")) 1.6
                                     dashed? 1.6
                                     :else 2)
                     stroke (or (:stroke e) default-stroke)
                     stroke-width (let [v (:stroke-width e)]
                                    (cond
                                      (number? v) v
                                      (string? v) (try (Double/parseDouble v) (catch Exception _ default-width))
                                      :else default-width))
                     arrow? (not (or (= edge-type :residual) (= edge-type "residual")))
                     marker-end (when arrow? "url(#arrow)")]
                 [:path {:d d
                         :fill "none"
                         :stroke stroke
                         :stroke-width stroke-width
                         :stroke-linecap "round"
                         :stroke-linejoin "round"
                         :stroke-miterlimit 10
                         :stroke-dasharray (if dashed? "6,6" "none")
                         :marker-end marker-end}])
               (when (:label e)
                 (let [pos (calculate-label-pos points)]
                   [:g
                    [:rect {:x (- (:x pos) 20) :y (- (:y pos) 10) :width 40 :height 20 :fill "white" :opacity 0.9 :rx 3}]
                    [:text {:x (:x pos) :y (:y pos) :fill "#333" :font-size 11 :text-anchor "middle" :dominant-baseline "middle"} (escape-html (strip-quotes (:label e)))]]))]))))

      (for [n atomic-nodes]
        (render-node n {:shadow? true}))]]))