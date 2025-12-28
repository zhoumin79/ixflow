(ns xflow.text2flow
  (:require [clojure.string :as str]
            [hiccup.core :as hiccup]
            [hiccup.util :as h-util]
            [xflow.theme.core :as theme]
            [xflow.layout.core :as layout]
            [xcommon.io :as xio]))

(def icon-map
  {"file-text" "📝"
   "bug" "🐛"
   "copy" "📄"
   "repeat" "🔁"
   "zap" "⚡"
   "check-square" "☑️"
   "package" "📦"
   "send" "📤"})

;; --- Parser ---

(defn- parse-props [props-str]
  (if (empty? props-str)
    {}
    (->> (str/split props-str #",")
         (map (fn [p]
                (let [[k v] (str/split p #":" 2)]
                  [(keyword (str/trim k)) (some-> v str/trim)])))
         (into {}))))

;; --- Parser ---

(defn- parse-props [props-str]
  (if (empty? props-str)
    {}
    (->> (str/split props-str #",")
         (map (fn [p]
                (let [[k v] (str/split p #":" 2)]
                  [(keyword (str/trim k)) (some-> v str/trim)])))
         (into {}))))

(defn- parse-line [line context]
  (let [line (str/trim line)]
    (cond
      (empty? line) context
      (str/starts-with? line "//") context ;; Comment

      ;; Title: title ...
      (str/starts-with? line "title ")
      (assoc-in context [:config :title] (str/trim (subs line 6)))

      ;; Config: key: value
      (re-matches #"^([a-zA-Z0-9-]+)\s*:\s*(.+)$" line)
      (let [[_ k v] (re-matches #"^([a-zA-Z0-9-]+)\s*:\s*(.+)$" line)]
        ;; Only treat as config if we are NOT inside a pool/block
        (if (empty? (:stack context))
          (assoc-in context [:config (keyword k)] (str/trim v))
          context))

      ;; Pool Start: Name [props] {
      ;; FIX: Non-greedy match for name to allow props capturing
      (re-matches #"(?i)^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?\s*\{$" line)
      (let [[_ name props-str] (re-matches #"(?i)^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?\s*\{$" line)
            pool {:name name
                  :props (parse-props props-str)
                  :nodes []
                  :type :pool}]
        (-> context
            (update :stack conj pool)))

      ;; Pool End: }
      (= line "}")
      (let [pool (peek (:stack context))
            stack (pop (:stack context))]
        (if (empty? stack)
          (-> context
              (update :pools conj pool) ;; Top level pool
              (assoc :stack stack)) ;; FIX: Remove from stack!
          (let [parent (peek stack)
                parent (update parent :nodes conj pool) ;; Nested pool
                stack (pop stack)
                stack (conj stack parent)]
            (assoc context :stack stack))))

      ;; Edge: A > B, A -> B, A --> B, A -- B
      (re-find #"(.+?)\s+((?:-*>)|(?:--))\s+(.+?)(?:\s*:\s*(.*))?$" line)
      (let [[_ from type-str to label] (re-find #"(.+?)\s+((?:-*>)|(?:--))\s+(.+?)(?:\s*:\s*(.*))?$" line)
            type (if (or (str/includes? type-str "--") (= type-str "--")) :dashed :solid)
            edge {:from (str/trim (str/replace from #"\"" ""))
                  :to (str/trim (str/replace to #"\"" ""))
                  :type type
                  :label (some-> label str/trim)}]
        (update context :edges conj edge))

      ;; Node: Name [props]
      :else
      (let [[_ name props-str] (re-matches #"^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?$" line)]
        (if name
          (let [node {:id name
                      :props (parse-props props-str)}]
            (if (empty? (:stack context))
              (update context :nodes conj node) ;; Top level node
              (let [pool (peek (:stack context))
                    pool (update pool :nodes conj node)
                    stack (pop (:stack context))
                    stack (conj stack pool)]
                (assoc context :stack stack))))
          ;; Log ignored lines for debugging
          (do
            (when (not (empty? line))
              (println "Ignored line:" line))
            context)))))) ;; Fallback/Error

(defn parse-dsl [dsl-str]
  (let [lines (str/split-lines dsl-str)
        initial-ctx {:config {} :pools [] :nodes [] :edges [] :stack []}]
    (reduce (fn [ctx line] (parse-line line ctx)) initial-ctx lines)))

;; --- Pre-Layout Processing ---

(defn- flatten-nodes [pools top-level-nodes]
  (let [all-nodes (atom top-level-nodes)]
    (letfn [(process-pool [pool path]
              (let [current-path (conj path pool)
                    nodes (:nodes pool)]
                (doseq [item nodes]
                  (if (= (:type item) :pool)
                    (process-pool item current-path)
                    (swap! all-nodes conj (assoc item
                                                 :swimlane-id (str/join " / " (map :name current-path))
                                                 :swimlane-path current-path))))))]
      (doseq [pool pools]
        (process-pool pool [])))
    @all-nodes))

;; --- Renderer ---

(defn render-svg [layout output-file]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        swimlanes (:swimlanes layout)
        width (:width layout 800)
        height (:height layout 600)
        config (:config layout)
        title (:title config)
        layout-mode (:layout config)] ;; Check layout mode

    (let [svg [:svg {:xmlns "http://www.w3.org/2000/svg"
                     :width width :height (+ height (if title 50 0))
                     :viewBox (str "0 0 " width " " (+ height (if title 50 0)))
                     :style "font-family: sans-serif;"}

               ;; Title
               (when title
                 [:text {:x (/ width 2) :y 30 :text-anchor "middle" :font-size 20 :font-weight "bold"} (h-util/escape-html title)])

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
                       [:text {:x (+ (:x lane) 10) :y (+ (:y lane) 17) :font-weight "bold" :fill "#495057" :font-size 12} (h-util/escape-html (:label lane))]]))

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
                         [:text {:x (+ (:x lane) (/ (:w lane) 2)) :y (+ (:y lane) 20) :text-anchor "middle" :font-weight "bold" :fill "#495057"} (h-util/escape-html (:label lane))]
                         [:text {:x (+ (:x lane) 15) :y (+ (:y lane) (/ (:h lane) 2))
                                 :text-anchor "middle" :dominant-baseline "middle"
                                 :transform (str "rotate(-90, " (+ (:x lane) 15) ", " (+ (:y lane) (/ (:h lane) 2)) ")")
                                 :font-weight "bold" :fill "#495057"} (h-util/escape-html (:label lane))])])))

                 ;; Draw Edges
                (for [e edges]
                  (let [points (:points e)] ;; Expecting pre-calculated points from layout
                    (when points
                      (let [d (str "M" (:x (first points)) "," (:y (first points)) " "
                                   (str/join " " (map (fn [p] (str "L" (:x p) "," (:y p))) (rest points))))]
                        [:g
                         [:path {:d d
                                 :fill "none"
                                 :stroke "black" :stroke-width 2
                                 :stroke-dasharray (if (= (:type e) :dashed) "5,5" "none")
                                 :marker-end "url(#arrow)"}]
                         (when (:label e)
                            ;; Draw label at midpoint
                           (let [mid-idx (int (/ (count points) 2))
                                 mid-p (nth points mid-idx)]
                             [:g
                              [:rect {:x (- (:x mid-p) 20) :y (- (:y mid-p) 10) :width 40 :height 20 :fill "white" :opacity 0.8}]
                              [:text {:x (:x mid-p) :y (:y mid-p) :fill "black" :font-size 10 :text-anchor "middle" :dominant-baseline "middle"} (h-util/escape-html (:label e))]]))]))))

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
                      [:g
                       (case shape
                         "diamond" [:polygon {:points (str cx "," (:y n) " " (+ (:x n) w) "," cy " " cx "," (+ (:y n) h) " " (:x n) "," cy)
                                              :fill bg-color :stroke "black" :stroke-width 1.5}]
                         "oval" [:rect {:x (:x n) :y (:y n) :width w :height h :rx (/ h 2) :ry (/ h 2)
                                        :fill bg-color :stroke "black" :stroke-width 1.5}]
                         ;; Default rect
                         [:rect {:x (:x n) :y (:y n) :width w :height h
                                 :fill bg-color
                                 :stroke "black" :rx 5 :stroke-width 1.5}])

                       ;; Node Label
                       [:text {:x cx :y cy :text-anchor "middle" :dominant-baseline "middle" :font-size 12 :font-weight "bold"}
                        (h-util/escape-html (:id n))]

                       ;; Icon Badge
                       (when icon-char
                         [:g
                          [:circle {:cx (:x n) :cy (:y n) :r 12 :fill "white" :stroke "black" :stroke-width 1}]
                          [:text {:x (:x n) :y (:y n) :text-anchor "middle" :dominant-baseline "middle" :font-size 14} icon-char]])])))

                 ;; Definitions
                [:defs
                 [:marker {:id "arrow" :markerWidth 10 :markerHeight 10 :refX 10 :refY 3 :orient "auto" :markerUnits "strokeWidth"}
                  [:path {:d "M0,0 L0,6 L9,3 z" :fill "black"}]]]]]]

      (xio/write-file output-file (hiccup/html svg)))))

(defn render [dsl-str output-file]
  (println "Starting render...")
  (let [model (parse-dsl dsl-str)
        _ (println "Parsed DSL. Nodes:" (count (:nodes model)) "Pools:" (count (:pools model)))
        nodes (flatten-nodes (:pools model) (:nodes model))
        _ (println "Flattened nodes:" (count nodes))

        ;; Determine layout options from config
        config (:config model)
        options {:direction (or (:direction config) "lr") ;; lr or tb
                 :swimlane-mode (or (:swimlane-mode config) "horizontal")
                 :layout (:layout config)} ;; Pass layout mode

        _ (println "Layout options:" options)
        ;; Use new layout engine
        layout-result (layout/layout nodes (:edges model) (:pools model) options)
        _ (println "Layout completed. Nodes:" (count (:nodes layout-result)))

        ;; Add config to layout result for title rendering
        layout-result (assoc layout-result :config config)]

    (println "Rendering SVG...")
    (render-svg layout-result output-file)
    (println "Rendered to" output-file)))