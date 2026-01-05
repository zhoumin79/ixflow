(ns xflow.theme.core
  (:require [xcommon.io :as xio]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [xflow.theme.rule :as rule]))

(def ^:private presets-cache (atom nil))

(defn- load-presets []
  (if @presets-cache
    @presets-cache
    (let [themes (xio/load-edn-from-resource "presets/themes.edn")
          color-themes (xio/load-edn-from-resource "presets/color_themes.edn")
          colors (xio/load-edn-from-resource "presets/colors.edn")
          fonts (xio/load-edn-from-resource "presets/fonts.edn")
          layouts (xio/load-edn-from-resource "presets/layouts.edn")
          rules (xio/load-edn-from-resource "presets/rules.edn")
          gradients (xio/load-edn-from-resource "presets/gradients.edn")
          shapes (xio/load-edn-from-resource "presets/shapes.edn")]
      (reset! presets-cache (merge themes color-themes colors fonts layouts rules gradients shapes)))))

(defn- resolve-ref [val context]
  (if (and (vector? val) (keyword? (first val)))
    (let [res (get-in context val)]
      (if res
        (resolve-ref res context)
        val))
    val))

(defn- rgb-to-hex [[r g b]]
  (format "#%02x%02x%02x" r g b))

(defn- resolve-color [val context]
  (let [resolved (if (and (vector? val) (keyword? (first val)))
                   (resolve-ref val context)
                   val)]
    (cond
      (vector? resolved) (if (and (number? (first resolved)) (<= (count resolved) 4))
                           (rgb-to-hex (take 3 resolved))
                           resolved)
      (keyword? resolved) (let [pool-color (get-in context [:colors :color-pool resolved])
                                direct-color (get-in context [:colors resolved])]
                            (cond
                              pool-color (resolve-color pool-color context)
                              direct-color (resolve-color direct-color context)
                              :else (name resolved)))
      :else resolved)))

(defn get-pool-lane-colors [preset-name context]
  (let [fallback (get-in context [:colors :palette-presets preset-name :pool-lanes :fallback])]
    (if fallback
      (mapv #(resolve-color % context) fallback)
      ["#0B5394" "#9FC5E8" "#FFFFFF"]))) ;; Default fallback ;; Default fallback

(defn theme->painter-config [theme-map context]
  (let [pool-preset-name (get-in theme-map [:pool-lane-fill-colors :preset])
        pool-colors (when pool-preset-name (get-pool-lane-colors pool-preset-name context))
        ;; Prefer explicit keys, fall back to preset list
        pool-fill (or (:pool-fill-color theme-map) (first pool-colors) "#0B5394")
        lane-fill (or (:lane-fill-color theme-map) (second pool-colors) "#9FC5E8")

        ;; Calculate lists for rules
        pool-colors-list (or pool-colors [pool-fill])

        ;; Extract lane-colors from theme-map if present, otherwise fallback
        lane-colors-from-theme (:lane-colors theme-map)
        lane-colors-list (cond
                           lane-colors-from-theme lane-colors-from-theme
                           pool-preset-name [lane-fill]
                           :else [lane-fill])

        ;; Extract lane-nodes colors if present
        lane-nodes (get theme-map :lane-nodes)
        lane-nodes-list (if lane-nodes
                          (mapv #(resolve-color % context) lane-nodes)
                          [])

        ;; Resolve pool-lanes map for dynamic lane coloring based on pool color
        pool-lanes-raw (when pool-preset-name
                         (get-in context [:colors :palette-presets pool-preset-name :pool-lanes]))
        pool-lane-map (when pool-lanes-raw
                        (reduce-kv (fn [m k v]
                                     (if (and (keyword? k) (not= k :fallback) (vector? v))
                                       (let [k-color (resolve-color k context)
                                             v-colors (mapv #(resolve-color % context) v)]
                                         (assoc m k-color v-colors))
                                       m))
                                   {}
                                   pool-lanes-raw))

        resolved-lane-colors (mapv #(resolve-color % context) lane-colors-list)]
    {:theme "CUSTOM" ;; or pass name
     :settings
     {:background
      {:background-fill-colour (resolve-color (:background-fill theme-map "white") context)}

      :title
      {:title-font "Arial"
       :title-font-size 26
       :title-font-colour (resolve-color (or (:font-color theme-map) "black") context)}

      :pool
      {:pool-font "Arial"
       :pool-font-size 18
       :pool-font-colour (resolve-color (or (:pool-title-font-color theme-map) "white") context)
       :pool-fill-colour (resolve-color pool-fill context)
       :pool-text-alignment "center"}

      :lane
      {:lane-font "Arial"
       :lane-font-size 18
       :lane-font-colour (resolve-color (or (:lane-title-font-color theme-map) "black") context)
       :lane-fill-colour (resolve-color lane-fill context)
       :lane-text-alignment "center"
       :lane-background-fill-colour (resolve-color (or (:lane-body-fill-color theme-map) "white") context)}

      :element
      {:element-font "Arial"
       :element-font-size 15
       :element-font-colour (resolve-color (or (:font-color theme-map) "black") context)
       :element-fill-colour (resolve-color (or (:task-fill theme-map) "white") context)
       :element-outline-colour (resolve-color (or (:element-outline-colour theme-map)
                                                  (:task-stroke theme-map)
                                                  "black")
                                              context)
       :element-outline-width (or (:element-outline-width theme-map) 1)
       :element-text-alignment "center"}

      :connector
      {:connector-font "Arial"
       :connector-font-size 15
       :connector-font-colour "#000000"
       :connector-line-width 1
       :connector-line-colour (resolve-color (or (:connector-stroke theme-map) "black") context)
       :connector-arrow-colour (resolve-color (or (:connector-stroke theme-map) "black") context)
       :connector-arrow-size 13}

      :footer
      {:footer-font "Arial"
       :footer-font-size 18
       :footer-font-colour (resolve-color (or (:font-color theme-map) "gray") context)}}

     ;; Add raw theme map for extended access if needed
     :raw-theme theme-map
     ;; Add colors for rule resolution
     :colors (merge (:colors context)
                    {:pool-colors (mapv #(resolve-color % context) pool-colors-list)
                     :lane-colors resolved-lane-colors
                     ;; Ensure :pool-lane-dynamic is available for rules that use it
                     :pool-lane-dynamic resolved-lane-colors
                     :lane-nodes lane-nodes-list
                     :pool-lane-map pool-lane-map})}))

(defn- resolve-composite-rule [val context]
  (if (and (vector? val)
           (keyword? (first val))
           (= (first val) :rules))
    (let [path-2 (take 2 val)
          keys-to-select (drop 2 val)]
      (if (and (seq path-2) (get-in context path-2))
        (let [base-rules (get-in context path-2)]
          (if (seq keys-to-select)
            (select-keys base-rules keys-to-select)
            base-rules))
        (do
          (println "Warning: Could not resolve rule path:" val)
          {})))
    (let [resolved (resolve-ref val context)]
      (if (vector? resolved)
        (do
          (println "Warning: Could not resolve rule ref:" val)
          {})
        resolved))))

(defn- resolve-rules [rules context]
  (reduce-kv (fn [m k v]
               (assoc m k (resolve-composite-rule v context)))
             {}
             rules))

(defn- resolve-map-refs [m context]
  (reduce-kv (fn [acc k v]
               (assoc acc k (if (map? v)
                              (resolve-map-refs v context)
                              (resolve-ref v context))))
             {}
             m))

(defn get-theme-config [theme-name]
  (let [context (load-presets)
        theme-key (if (string? theme-name)
                    (keyword (str/replace (str/lower-case theme-name) "_" "-"))
                    theme-name)
        theme-def (get-in context [:themes theme-key])
        color-theme-def (get-in context [:color-themes theme-key])]
    (cond
      theme-def
      (let [colors-ref (:colors theme-def)
            color-theme (resolve-ref colors-ref context)
            config (theme->painter-config color-theme context)
            resolved-rules (resolve-rules (:rules theme-def) context)
            ;; Resolve gradients if present
            gradients-ref (:gradients theme-def)
            resolved-gradients (when gradients-ref (resolve-ref gradients-ref context))
            ;; Resolve shapes if present
            shapes-ref (:shapes theme-def)
            resolved-shapes (when shapes-ref (resolve-map-refs shapes-ref context))]
        ;; Merge rules, gradients, and shapes from the theme definition into the config
        (assoc config
               :rules resolved-rules
               :gradients resolved-gradients
               :shapes resolved-shapes))

      color-theme-def
      (theme->painter-config color-theme-def context)

      :else
      (do
        (println "Theme not found:" theme-name "Using default.")
        (if (not= theme-key :default)
          (get-theme-config :default)
          nil)))))

