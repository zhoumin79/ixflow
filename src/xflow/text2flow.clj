(ns xflow.text2flow
  (:require [clojure.string :as str]
            [hiccup.core :as hiccup]
            [xflow.dsl.parser :as parser]
            [xflow.layout.core :as layout]
            [xflow.painter.svg :as painter]
            [xcommon.io :as xio]
            [xflow.theme.core :as theme]))

(defn render-model [model output-file]
  (println "Starting render-model...")
  (let [_ (println "Model stats - Nodes:" (count (:nodes model)) "Pools:" (count (:pools model)))
        nodes (parser/flatten-nodes (:pools model) (:nodes model))
        _ (println "Flattened nodes:" (count nodes))

        ;; Determine layout options from config
        dsl-config (:config model)

        ;; Resolve Theme
        theme-name (or (:theme dsl-config) "default")
        _ (println "Resolving theme:" theme-name)
        theme-config (theme/get-theme-config theme-name)

        ;; Merge configs: Theme provides base styles/settings, DSL config provides overrides (layout, title, etc)
        final-config (merge theme-config dsl-config)

        direction (or (:direction final-config) "lr")
        swimlane-mode (or (:swimlane-mode final-config)
                          (if (or (= direction "tb") (= direction "vertical"))
                            "vertical"
                            "horizontal"))
        options {:direction direction
                 :swimlane-mode swimlane-mode
                 :layout (:layout final-config) ;; Pass layout mode
                 :routing (:routing final-config)} ;; Pass routing mode (spline or manhattan)

        _ (println "Layout options:" options)
        ;; Use new layout engine
        layout-result (layout/layout nodes (:edges model) (:pools model) options)
        _ (println "Layout completed. Nodes:" (count (:nodes layout-result)))

        ;; Add config to layout result for title rendering
        layout-result (assoc layout-result :config final-config)

        ;; Render to SVG Hiccup
        svg-hiccup (painter/render-svg layout-result)]

    (println "Rendering SVG...")
    (xio/write-file output-file (hiccup/html svg-hiccup))
    (println "Rendered to" output-file)))

(defn render [dsl-str output-file]
  (println "Parsing DSL...")
  (let [model (parser/parse-dsl dsl-str)]
    (render-model model output-file)))
