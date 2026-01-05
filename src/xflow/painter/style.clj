(ns xflow.painter.style
  (:require [xflow.theme.rule :as rule]
            [clojure.string :as str]))

(defn- get-element-type [element]
  (cond
    (contains? element :points) :edge
    (:vertical? element) :pool ;; Simple heuristic for now
    (:nodes element) :pool
    :else :task))

(defn- resolve-style-values [style config context]
  (reduce-kv (fn [m k v]
               (let [res (if (map? v)
                           (resolve-style-values v config context) ;; RECURSE!
                           (rule/resolve-rule-value v config context))]
                 (assoc m k res)))
             {}
             style))

(defn- map-theme-keys [style]
  (cond-> style
    (:element-fill-colour style) (assoc :fill (:element-fill-colour style))
    (:element-outline-colour style) (assoc :stroke (:element-outline-colour style))
    (:element-outline-width style) (assoc :stroke-width (:element-outline-width style))
    (:element-font-colour style) (assoc :font-color (:element-font-colour style))

    (:pool-fill-colour style) (assoc :fill (:pool-fill-colour style))
    (:pool-font-colour style) (assoc :font-color (:pool-font-colour style))
    (:lane-fill-colour style) (assoc :fill (:lane-fill-colour style))
    (:lane-font-colour style) (assoc :font-color (:lane-font-colour style))
    (:fill-color style) (assoc :fill (:fill-color style)) ;; Added support for :fill-color
    (:color style) (assoc :fill (:color style)) ;; Added support for :color (from DSL props)

    ;; Map nested rule styles to renderer keys
    (get-in style [:header :fill-color]) (assoc :header-fill (get-in style [:header :fill-color]))
    (get-in style [:header :stroke-color]) (assoc :header-stroke (get-in style [:header :stroke-color]))
    (get-in style [:header :font-color]) (assoc :header-font-color (get-in style [:header :font-color]))

    (:connector-line-colour style) (assoc :stroke (:connector-line-colour style))
    (:connector-line-width style) (assoc :stroke-width (:connector-line-width style))
    (:connector-font-colour style) (assoc :font-color (:connector-font-colour style))))

(defn resolve-node-style [node config context]
  (let [base-style (get-in config [:settings :element])
        rules (get-in config [:rules :task])
        matched-rule (when rules
                       (rule/get-matching-rule rules context node))

        ;; Determine shape key to lookup shape properties
        ;; Priority: Rule > Node Prop > Base Style > Default
        shape-key (keyword (or (:shape matched-rule)
                               (:shape (:props node))
                               (:shape base-style)
                               :default))

        ;; Resolve shape properties from config
        ;; Currently assumes all shapes are under :task category in shapes config
        ;; or falls back to looking up in config[:shapes shape-key] if structure differs
        shape-props (or (get-in config [:shapes :task shape-key])
                        (get-in config [:shapes shape-key]))

        ;; Merge styles: Base < Shape < Rule < Node Props
        style (merge base-style shape-props matched-rule)

        ;; Resolve props that might be directly on the node
        style (merge style (:props node))

        ;; Resolve dynamic values (vectors)
        style (resolve-style-values style config context)

        ;; Map to SVG keys
        style (map-theme-keys style)]

    ;; Special handling for gradients if configured in the style
    (if-let [grad (:gradient style)]
      (assoc style :fill (str "url(#" (name grad) ")"))
      style)))

(defn resolve-edge-style [edge config]
  (let [base-style (get-in config [:settings :connector])
        style (merge base-style (:props edge))]
    (map-theme-keys style)))

(defn resolve-swimlane-style [lane config context]
  (let [lane-id (:id lane)
        is-pool? (and lane-id (not (str/includes? lane-id " / ")))
        element-type (if is-pool? :pool :lane)
        lane-with-type (assoc lane :type element-type)

        base-key (if is-pool? :pool :lane)
        base-style (get-in config [:settings base-key])

        rule-category [:rules :swimlane]
        rules (get-in config rule-category)

        matched-rule (when rules
                       (rule/get-matching-rule rules context lane-with-type))

        style (merge base-style matched-rule (:props lane))
        style (resolve-style-values style config context)]
    (map-theme-keys style)))
