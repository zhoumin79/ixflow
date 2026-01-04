(ns xflow.template
  (:require [xcommon.io :as xio]
            [xflow.text2flow :as text2flow]
            [clojure.string :as str]))

(defn load-template
  "Load a workflow template EDN from resources/templates."
  [template-name]
  (let [resource-path (str "templates/" (name template-name) ".edn")]
    (xio/load-edn-from-resource resource-path)))

(defn render-template
  "Render a workflow diagram from a template EDN.

  template-name: keyword or string identifying the template file (without extension)
  output-file: output path, e.g. \"output/template_workflow_demo.svg\""
  [template-name output-file]
  (let [template (load-template template-name)
        data (:data template)
        dsl (:pool-dsl data)
        config (:config data)

        ;; Extract theme name if present
        theme-name (or (-> template :theme :name) "default")

        ;; Merge theme into config to be injected into DSL
        config (assoc config :theme theme-name)

        ;; Inject config from EDN into DSL string so parser picks it up
        config-lines (map (fn [[k v]] (str (name k) ": " v)) config)
        full-dsl (str (str/join "\n" config-lines) "\n" dsl)

        cleaned-output-file (str/replace output-file #"\+" "/")]
    (text2flow/render full-dsl cleaned-output-file)))
