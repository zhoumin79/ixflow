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

(defn- curve-path [points]
  (if (< (count points) 2)
    ""
    (let [start (first points)
          cmds (atom [(str "M" (:x start) "," (:y start))])]
      (dotimes [i (dec (count points))]
        (let [p0 (nth points i)
              p1 (nth points (inc i))

              ;; Vertical layout heuristic (assuming TB for now, logic can be adapted)
              ;; If mainly vertical movement:
              dy (- (:y p1) (:y p0))

              ;; Control points for a sigmoid S-curve
              ;; CP1 is vertically below P0
              cp1-x (:x p0)
              cp1-y (+ (:y p0) (* dy 0.5))

              ;; CP2 is vertically above P1
              cp2-x (:x p1)
              cp2-y (- (:y p1) (* dy 0.5))]

          (swap! cmds conj (str "C" cp1-x "," cp1-y " " cp2-x "," cp2-y " " (:x p1) "," (:y p1)))))

      (str/join " " @cmds))))

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
        layout-mode (:layout config)] ;; Check layout mode

    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width width :height (+ height (if title 50 0))
           :viewBox (str "0 0 " width " " (+ height (if title 50 0)))
           :style "font-family: sans-serif;"}

     ;; Definitions
     [:defs
      [:marker {:id "arrow" :markerWidth 10 :markerHeight 10 :refX 8 :refY 3 :orient "auto" :markerUnits "strokeWidth"}
       [:path {:d "M0,0 L0,6 L9,3 z" :fill "#555"}]]

      ;; Drop Shadow Filter
      [:filter {:id "drop-shadow" :x "-20%" :y "-20%" :width "150%" :height "150%"}
       [:feGaussianBlur {:in "SourceAlpha" :stdDeviation "2" :result "blur"}]
       [:feOffset {:in "blur" :dx "2" :dy "2" :result "offsetBlur"}]
       [:feComponentTransfer
        [:feFuncA {:type "linear" :slope "0.3"}]] ;; Opacity of shadow
       [:feMerge
        [:feMergeNode] ;; Shadow
        [:feMergeNode {:in "SourceGraphic"}]]]] ;; Original

     ;; Title
     (when title
       [:text {:x (/ width 2) :y 30 :text-anchor "middle" :font-size 20 :font-weight "bold" :fill "#333"} (escape-html title)])

     [:g {:transform (if title "translate(0, 50)" "translate(0, 0)")}
      ;; Draw Swimlanes (Backgrounds) or Clusters
      (if (= layout-mode "cluster")
        ;; Draw Cluster Boxes
        (for [lane swimlanes]
          (let [bg-color (or (-> lane :props :color) "#f8f9fa")]
            [:g
             [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
                     :fill bg-color :fill-opacity 0.3
                     :stroke (or (-> lane :props :color) "#dee2e6") :stroke-width 2
                     :rx 10 :ry 10}]
             ;; Cluster Label (top-left inside box)
             [:rect {:x (:x lane) :y (:y lane) :width 100 :height 25 :rx 5 :ry 5 :fill "white" :stroke "#dee2e6"}]
             [:text {:x (+ (:x lane) 10) :y (+ (:y lane) 17) :font-weight "bold" :fill "#495057" :font-size 12} (escape-html (:label lane))]]))

        ;; Draw Standard Swimlanes
        (for [lane swimlanes]
          (let [header-color (or (-> lane :props :color) "#e9ecef")]
            [:g
             [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
                     :fill (if (even? (:index lane)) "#f8f9fa" "#ffffff")
                     :stroke "#dee2e6" :stroke-width 1}]
             ;; Header
             (if (:vertical? lane)
               [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height 30 :fill header-color :stroke "#dee2e6"}]
               [:rect {:x (:x lane) :y (:y lane) :width 30 :height (:h lane) :fill header-color :stroke "#dee2e6"}])
             ;; Label
             (if (:vertical? lane)
               [:text {:x (+ (:x lane) (/ (:w lane) 2)) :y (+ (:y lane) 20) :text-anchor "middle" :font-weight "bold" :fill "#495057"} (escape-html (:label lane))]
               [:text {:x (+ (:x lane) 15) :y (+ (:y lane) (/ (:h lane) 2))
                       :text-anchor "middle" :dominant-baseline "middle"
                       :transform (str "rotate(-90, " (+ (:x lane) 15) ", " (+ (:y lane) (/ (:h lane) 2)) ")")
                       :font-weight "bold" :fill "#495057"} (escape-html (:label lane))])])))

      ;; Draw Edges
      (for [e edges]
        (let [points (:points e)] ;; Expecting pre-calculated points from layout
          (when points
            (let [d (if (= (:routing-type e) :spline)
                      (curve-path points)
                      (rounded-path points 10))] ;; Use rounded path with radius 10
              [:g
               [:path {:d d
                       :fill "none"
                       :stroke "#555" :stroke-width 2
                       :stroke-dasharray (if (= (:type e) :dashed) "5,5" "none")
                       :marker-end "url(#arrow)"}]
               (when (:label e)
                 ;; Draw label at calculated position
                 (let [pos (calculate-label-pos points)]
                   [:g
                    [:rect {:x (- (:x pos) 20) :y (- (:y pos) 10) :width 40 :height 20 :fill "white" :opacity 0.9 :rx 3}]
                    [:text {:x (:x pos) :y (:y pos) :fill "#333" :font-size 11 :text-anchor "middle" :dominant-baseline "middle"} (escape-html (strip-quotes (:label e)))]]))]))))

      ;; Draw Nodes
      (for [n nodes]
        (when (and (:x n) (:y n))
          (let [shape (or (-> n :props :shape) "rect")
                bg-color (or (-> n :props :color) "white")
                w (:w n) h (:h n)
                cx (+ (:x n) (/ w 2))
                cy (+ (:y n) (/ h 2))
                icon-key (-> n :props :icon)
                icon-char (get icon-map icon-key)]
            [:g {:filter "url(#drop-shadow)"} ;; Apply shadow
             (case shape
               "diamond" [:polygon {:points (str cx "," (:y n) " " (+ (:x n) w) "," cy " " cx "," (+ (:y n) h) " " (:x n) "," cy)
                                    :fill bg-color :stroke "#333" :stroke-width 1.5}]
               "oval" [:rect {:x (:x n) :y (:y n) :width w :height h :rx (/ h 2) :ry (/ h 2)
                              :fill bg-color :stroke "#333" :stroke-width 1.5}]
               ;; Default rect
               [:rect {:x (:x n) :y (:y n) :width w :height h
                       :fill bg-color
                       :stroke "#333" :rx 6 :stroke-width 1.5}])

             ;; Node Label
             [:text {:x cx :y cy :text-anchor "middle" :dominant-baseline "middle" :font-size 12 :font-weight "bold" :fill "#222"}
              (escape-html (strip-quotes (or (-> n :props :label) (:id n))))]

             ;; Icon Badge
             (when icon-char
               [:g
                [:circle {:cx (:x n) :cy (:y n) :r 12 :fill "white" :stroke "#333" :stroke-width 1}]
                [:text {:x (:x n) :y (:y n) :text-anchor "middle" :dominant-baseline "middle" :font-size 14} icon-char]])])))]]))
