(ns xflow.layout.config)

;; --- Default Dimensions ---
(def ^:const DEFAULT-NODE-WIDTH 150)
(def ^:const DEFAULT-NODE-HEIGHT 80)
(def ^:const DUMMY-NODE-SIZE 0)

;; --- Default Spacing ---
(def ^:const DEFAULT-NODE-SEP 50) ;; Horizontal gap between nodes in same rank
(def ^:const DEFAULT-RANK-SEP 80) ;; Vertical gap between ranks
(def ^:const DEFAULT-EDGE-SEP 10) ;; Separation between parallel edges
(def ^:const SWIMLANE-PADDING 50)

;; --- Routing Constants ---
(def ^:const ROUTING-GRID-SIZE 10) ;; Grid resolution for routing
(def ^:const PORT-OFFSET 10) ;; Offset for ports from node center/corner

;; --- Theme Presets ---
(def compact-theme
  {:node-sep 30
   :rank-sep 50
   :edge-sep 5})

(def spacious-theme
  {:node-sep 80
   :rank-sep 120
   :edge-sep 20})

(defn resolve-config [options]
  (let [base-theme (case (:theme options)
                     :compact compact-theme
                     :spacious spacious-theme
                     {})]
    (merge {:node-sep DEFAULT-NODE-SEP
            :rank-sep DEFAULT-RANK-SEP
            :edge-sep DEFAULT-EDGE-SEP}
           base-theme
           (select-keys options [:node-sep :rank-sep :edge-sep]))))
