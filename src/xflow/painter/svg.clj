(ns xflow.painter.svg
  (:require [clojure.string :as str]
            [xflow.geometry :as geo]
            [xcommon.geometry :as xgeo]
            [xflow.painter.style :as style]
            [xflow.painter.defs :as defs]))

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

(defmulti render-shape (fn [node style] (keyword (or (:shape style) (:shape node) "rect"))))

(defn- render-rect-impl [node style & [override-rx override-ry]]
  (let [x (:x node) y (:y node) w (or (:w node) 100) h (or (:h node) 50)
        rx (or override-rx (:rx style) 6)
        ry (or override-ry (:ry style) 6)]
    [:rect (merge {:x x :y y :width w :height h :rx rx :ry ry}
                  (select-keys style [:fill :stroke :stroke-width :filter]))]))

(defmethod render-shape :default [node style]
  (render-rect-impl node style))

(defmethod render-shape :rect [node style]
  (render-rect-impl node style 0 0))

(defmethod render-shape :round-rect [node style]
  (render-rect-impl node style 6 6))

(defmethod render-shape :oval [node style]
  (let [x (:x node) y (:y node) w (or (:w node) 100) h (or (:h node) 50)]
    [:rect (merge {:x x :y y :width w :height h :rx (/ h 2) :ry (/ h 2)}
                  (select-keys style [:fill :stroke :stroke-width :filter]))]))

(defmethod render-shape :diamond [node style]
  (let [x (:x node) y (:y node) w (or (:w node) 100) h (or (:h node) 50)
        cx (+ x (/ w 2))
        cy (+ y (/ h 2))]
    [:polygon (merge {:points (str cx "," y " " (+ x w) "," cy " " cx "," (+ y h) " " x "," cy)}
                     (select-keys style [:fill :stroke :stroke-width :filter]))]))

(defn- render-node [node config context]
  (let [style (style/resolve-node-style node config context)
        _ (when (= (:id node) "start") (println "DEBUG: Style for start node:" (keys style) "Fill:" (:fill style)))
        shadow? (not (= (:type node) :group))
        style (cond-> style
                shadow? (assoc :filter "url(#drop-shadow)"))

        x (:x node) y (:y node) w (or (:w node) 100) h (or (:h node) 50)
        cx (+ x (/ w 2))
        cy (+ y (/ h 2))
        icon-key (:icon style)
        icon-char (get icon-map (or icon-key (-> node :props :icon)))]

    [:g
     (render-shape node style)

     ;; Label
     [:text {:x cx :y cy :text-anchor "middle" :dominant-baseline "middle"
             :font-size 12 :font-weight "bold"
             :fill (or (:font-color style) "#222")}
      (escape-html (strip-quotes (or (-> node :props :label) (:id node))))]

     ;; Icon
     (when icon-char
       [:g
        [:circle {:cx x :cy y :r 12 :fill "white" :stroke "#333" :stroke-width 1}]
        [:text {:x x :y y :text-anchor "middle" :dominant-baseline "middle" :font-size 14} icon-char]])]))

(defn- render-edge [edge config]
  (let [style (style/resolve-edge-style edge config)
        points (:points edge)]
    (when points
      (let [d (if (= (:routing-type edge) :spline)
                (xgeo/smooth-curve-path points)
                (xgeo/polyline-path points :radius 10))
            marker-end (when (not= (:type edge) :residual) "url(#arrow)")]
        [:g
         [:path (merge {:d d
                        :fill "none"
                        :stroke-linecap "round"
                        :stroke-linejoin "round"
                        :stroke-miterlimit 10
                        :marker-end marker-end}
                       (select-keys style [:stroke :stroke-width :stroke-dasharray]))]

         (when (:label edge)
           (when-let [pos (or (:label-pos edge) (geo/calculate-label-pos points))]
             [:g
              [:rect {:x (- (:x pos) 20) :y (- (:y pos) 10) :width 40 :height 20 :fill "white" :opacity 0.9 :rx 3}]
              [:text {:x (:x pos) :y (:y pos) :fill (or (:font-color style) "#333")
                      :font-size 11 :text-anchor "middle" :dominant-baseline "middle"}
               (escape-html (strip-quotes (:label edge)))]]))]))))

(defn- render-swimlane [lane config context]
  (let [style (style/resolve-swimlane-style lane config context)
        vertical? (:vertical? lane)
        layout-mode (:layout config)]
    (if (= layout-mode "cluster")
      [:g
       [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
               :fill (or (:fill style) "#f8f9fa") :fill-opacity 0.3
               :stroke (or (:stroke style) "#dee2e6") :stroke-width 2
               :rx 10 :ry 10}]
       [:rect {:x (:x lane) :y (:y lane) :width 100 :height 25 :rx 5 :ry 5 :fill "white" :stroke "#dee2e6"}]
       [:text {:x (+ (:x lane) 10) :y (+ (:y lane) 17) :font-weight "bold" :fill "#495057" :font-size 12}
        (escape-html (:label lane))]]

      [:g
       ;; Background
       [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height (:h lane)
               :fill (or (:fill style) "#ffffff")
               :stroke (or (:stroke style) "#dee2e6") :stroke-width 1}]

       ;; Header
       (if vertical?
         [:rect {:x (:x lane) :y (:y lane) :width (:w lane) :height 30
                 :fill (or (:header-fill style) "#e9ecef") :stroke "#dee2e6"}]
         [:rect {:x (:x lane) :y (:y lane) :width 30 :height (:h lane)
                 :fill (or (:header-fill style) "#e9ecef") :stroke "#dee2e6"}])

       ;; Title
       (if vertical?
         [:text {:x (+ (:x lane) (/ (:w lane) 2)) :y (+ (:y lane) 20)
                 :text-anchor "middle" :font-weight "bold" :fill (or (:font-color style) "#495057")}
          (escape-html (:label lane))]
         [:text {:x (+ (:x lane) 15) :y (+ (:y lane) (/ (:h lane) 2))
                 :text-anchor "middle" :dominant-baseline "middle"
                 :transform (str "rotate(-90, " (+ (:x lane) 15) ", " (+ (:y lane) (/ (:h lane) 2)) ")")
                 :font-weight "bold" :fill (or (:font-color style) "#495057")}
          (escape-html (:label lane))])])))

(defn render-svg [layout]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        swimlanes (:swimlanes layout)
        width (:width layout 800)
        height (:height layout 600)
        config (:config layout)
        title (get-in config [:settings :title :text] (:title config))

        group-node? (fn [n] (= (:type n) :group))
        group-nodes (filter group-node? nodes)
        atomic-nodes (remove group-node? nodes)

        ;; Build Context for rules
        lane-map (zipmap (map :id swimlanes) swimlanes)
        get-context (fn [node]
                      (let [lane (get lane-map (:lane-id node))
                            pool (get lane-map (:parent-id lane))]
                        {:lane-idx (:index lane)
                         :pool-idx (:index pool)
                         :completed (-> node :props :completed)}))]

    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width width :height (+ height (if title 50 0))
           :viewBox (str "0 0 " width " " (+ height (if title 50 0)))
           :style "font-family: sans-serif;"}

     (defs/gen-defs config)

     (when title
       [:text {:x (/ width 2) :y 30 :text-anchor "middle" :font-size 20 :font-weight "bold" :fill "#333"} (escape-html title)])

     [:g {:transform (if title "translate(0, 50)" "translate(0, 0)")}

      ;; Swimlanes
      (for [lane swimlanes]
        (render-swimlane lane config (get-context lane))) ;; Pass context to swimlane too? lane context is simpler

      ;; Groups
      (for [n group-nodes]
        (render-node n config (get-context n)))

      ;; Edges
      (for [e edges]
        (render-edge e config))

      ;; Nodes
      (for [n atomic-nodes]
        (render-node n config (get-context n)))]]))
