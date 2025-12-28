(ns xflow.text2flow
  (:require [clojure.string :as str]
            [hiccup.core :as hiccup]
            [xflow.dsl.parser :as parser]
            [xflow.layout.core :as layout]
            [xflow.painter.svg :as painter]
            [xcommon.io :as xio]))

(defn render [dsl-str output-file]
  (println "Starting render...")
  (let [model (parser/parse-dsl dsl-str)
        _ (println "Parsed DSL. Nodes:" (count (:nodes model)) "Pools:" (count (:pools model)))
        nodes (parser/flatten-nodes (:pools model) (:nodes model))
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
        layout-result (assoc layout-result :config config)

        ;; Render to SVG Hiccup
        svg-hiccup (painter/render-svg layout-result)]

    (println "Rendering SVG...")
    (xio/write-file output-file (hiccup/html svg-hiccup))
    (println "Rendered to" output-file)))
