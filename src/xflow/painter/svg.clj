(ns xflow.painter.svg
  (:require [clojure.string :as str]
            [xflow.geometry :as geo]
            [xcommon.geometry :as xgeo]))

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

(defn- strip-quotes [s]
  (if (string? s)
    (-> s
        str/trim
        (str/replace #"^\"+|\"+$" ""))
    s))

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
                      (xgeo/smooth-curve-path points)
                      (xgeo/polyline-path points :radius 10))]
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
                 (when-let [pos (or (:label-pos e) (geo/calculate-label-pos points))]
                   [:g
                    [:rect {:x (- (:x pos) 20) :y (- (:y pos) 10) :width 40 :height 20 :fill "white" :opacity 0.9 :rx 3}]
                    [:text {:x (:x pos) :y (:y pos) :fill "#333" :font-size 11 :text-anchor "middle" :dominant-baseline "middle"} (escape-html (strip-quotes (:label e)))]]))]))))

      (for [n atomic-nodes]
        (render-node n {:shadow? true}))]]))