(ns xflow.painter.defs
  (:require [clojure.string :as str]))

(defn- gen-linear-gradient [id {:keys [colors direction] :as config}]
  (let [x1 (if (= direction :vertical) "0%" "0%")
        y1 (if (= direction :horizontal) "0%" "0%")
        x2 (case direction
             :horizontal "100%"
             :diagonal "100%"
             "0%")
        y2 (case direction
             :vertical "100%"
             :diagonal "100%"
             "0%")]
    [:linearGradient {:id id :x1 x1 :y1 y1 :x2 x2 :y2 y2}
     (map-indexed (fn [i color]
                    [:stop {:offset (str (int (* (/ i (dec (count colors))) 100)) "%")
                            :stop-color color}])
                  colors)]))

(defn- gen-radial-gradient [id {:keys [colors] :as config}]
  [:radialGradient {:id id :cx "50%" :cy "50%" :r "50%" :fx "50%" :fy "50%"}
   (map-indexed (fn [i color]
                  [:stop {:offset (str (int (* (/ i (dec (count colors))) 100)) "%")
                          :stop-color color}])
                colors)])

(defn gen-gradient [id config]
  (case (:type config)
    :linear (gen-linear-gradient id config)
    :radial (gen-radial-gradient id config)
    nil))

(defn gen-defs [config]
  (let [gradients (:gradients config)
        arrow-color (get-in config [:settings :connector :connector-arrow-colour] "#555")]
    [:defs
     ;; Arrow Marker
     [:marker {:id "arrow" :markerWidth 10 :markerHeight 10 :refX 8 :refY 3 :orient "auto" :markerUnits "strokeWidth"}
      [:path {:d "M0,0 L0,6 L9,3 z" :fill arrow-color}]]

     ;; Drop Shadow Filter
     [:filter {:id "drop-shadow" :x "-20%" :y "-20%" :width "150%" :height "150%"}
      [:feGaussianBlur {:in "SourceAlpha" :stdDeviation "2" :result "blur"}]
      [:feOffset {:in "blur" :dx "2" :dy "2" :result "offsetBlur"}]
      [:feComponentTransfer
       [:feFuncA {:type "linear" :slope "0.3"}]]
      [:feMerge
       [:feMergeNode]
       [:feMergeNode {:in "SourceGraphic"}]]]

     ;; Inner Glow (example)
     [:filter {:id "inner-glow"}
      [:feFlood {:flood-color "#ffffff" :flood-opacity "0.5" :result "flood"}]
      [:feComposite {:in "flood" :in2 "SourceAlpha" :operator "out" :result "mask"}]
      [:feGaussianBlur {:in "mask" :stdDeviation "3" :result "blurred"}]
      [:feComposite {:in "blurred" :in2 "SourceAlpha" :operator "arithmetic" :k2 "1" :k3 "-1" :result "shadow"}]
      [:feMerge
       [:feMergeNode {:in "SourceGraphic"}]
       [:feMergeNode {:in "shadow"}]]]

     ;; Generate Gradients
     (when gradients
       (if (:type gradients)
         (gen-gradient "theme-gradient" gradients)
         (for [[k v] gradients]
           (gen-gradient (name k) v))))]))

