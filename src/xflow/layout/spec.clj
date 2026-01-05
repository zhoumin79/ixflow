(ns xflow.layout.spec
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]))

;; --- Core Primitives ---
(def Point
  [:map
   [:x number?]
   [:y number?]])

(def Dimension
  [:map
   [:w number?]
   [:h number?]])

(def Box
  (mu/merge Point Dimension))

;; --- Style Spec ---
;; 用于视觉呈现的样式属性，与布局算法解耦
(def Style
  [:map {:closed false}
   [:fill {:optional true} :string]
   [:stroke {:optional true} :string]
   [:stroke-width {:optional true} number?]
   [:stroke-dasharray {:optional true} :string]
   [:opacity {:optional true} number?]
   [:font-family {:optional true} :string]
   [:font-size {:optional true} number?]
   [:font-weight {:optional true} [:or :string :int]]])

;; --- Port Spec ---
;; 节点上的连接点，用于高级路由
(def Port
  [:map
   [:id :string]
   [:x number?]
   [:y number?]
   [:side {:optional true} [:enum :top :bottom :left :right]]])

;; --- Node Spec ---
(def Node
  [:map
   [:id :string]
   [:type [:enum :start :end :process :decision :pool :dummy :group]]
   [:label {:optional true} :string]

   ;; 视觉样式
   [:style {:optional true} Style]
   [:icon {:optional true} :string]
   [:shape {:optional true} :string] ;; e.g., "rect", "diamond"

   ;; 嵌套结构 (Compound Graph)
   [:children {:optional true} [:vector [:ref :xflow.layout.spec/node]]]
   [:parent-id {:optional true} :string]
   [:collapsed? {:optional true} :boolean]

   ;; 布局属性 (算法计算过程中会不断更新)
   [:x {:optional true} number?]
   [:y {:optional true} number?]
   [:w {:optional true} number?]
   [:h {:optional true} number?]
   [:rank {:optional true} int?] ;; Layer assignment
   [:order {:optional true} int?] ;; Order within layer
   [:swimlane-id {:optional true} [:or :string :nil]]

   ;; 路由属性
   [:ports {:optional true} [:vector Port]]

   ;; 内部标志位
   [:dummy? {:optional true} :boolean]
   [:parent-edge {:optional true} :any]]) ;; Reference to original edge for dummy nodes

;; --- Edge Spec ---
(def Edge
  [:map
   [:from :string]
   [:to :string]
   [:id {:optional true} :string]
   [:label {:optional true} :string]
   [:type {:optional true} [:enum :default :association :dependency]]

   ;; 布局约束
   [:weight {:optional true} int?] ;; 权重，影响布局紧凑度
   [:min-len {:optional true} int?] ;; 最小跨层数

   ;; 视觉样式
   [:style {:optional true} Style]
   [:arrow-head {:optional true} [:enum :none :standard :diamond :circle]]
   [:arrow-tail {:optional true} [:enum :none :standard :diamond :circle]]

   ;; 路由结果
   [:points {:optional true} [:vector Point]]
   [:label-pos {:optional true} Point]])

;; --- Swimlane Spec ---
(def Swimlane
  [:map
   [:id :string]
   [:label :string]
   [:index int?]
   [:nodes {:optional true} [:vector [:ref :xflow.layout.spec/node]]] ;; 可以引用节点对象
   [:node-ids {:optional true} [:set :string]] ;; 或者只存ID引用

   ;; 布局属性
   [:size {:optional true} number?] ;; Computed width/height
   [:x {:optional true} number?]
   [:y {:optional true} number?]
   [:w {:optional true} number?]
   [:h {:optional true} number?]
   [:vertical? {:optional true} :boolean]
   [:style {:optional true} Style]])

;; --- Configuration Spec ---
(def LayoutOptions
  [:map
   [:direction {:optional true} [:enum "tb" "lr" "bt" "rl"]]
   [:layout {:optional true} [:enum "simple" "cluster" "swimlane" "compound"]]
   [:routing {:optional true} [:enum "manhattan" "spline" "ortho" "direct"]]
   [:swimlane-mode {:optional true} [:enum "horizontal" "vertical"]]

   ;; Sugiyama 算法参数
   [:align {:optional true} [:enum "ul" "ur" "dl" "dr" "center"]]
   [:ranker {:optional true} [:enum "network-simplex" "longest-path" "tight-tree"]]

   ;; 间距设置
   [:node-sep {:optional true} number?] ;; 同层节点间距
   [:rank-sep {:optional true} number?] ;; 层间距
   [:edge-sep {:optional true} number?] ;; 边间距
   [:padding {:optional true} number?] ;; 画布或容器内边距

   ;; 调试选项
   [:debug? {:optional true} :boolean]])

;; --- Registry ---
(def registry
  {:xflow.layout.spec/node Node
   :xflow.layout.spec/edge Edge
   :xflow.layout.spec/swimlane Swimlane})

;; --- Context Spec (State passed through pipeline) ---
(def LayoutContext
  [:schema {:registry registry}
   [:map
    [:nodes [:vector [:ref :xflow.layout.spec/node]]]
    [:edges [:vector [:ref :xflow.layout.spec/edge]]]
    [:swimlanes {:optional true} [:vector [:ref :xflow.layout.spec/swimlane]]]
    [:options LayoutOptions]
    [:width {:optional true} number?]
    [:height {:optional true} number?]
    [:scale {:optional true} number?]]])

;; --- Validation Helpers ---

(defn validate-context!
  "Validates the layout context against the schema. Throws ex-info if invalid."
  [ctx]
  (when-not (m/validate LayoutContext ctx)
    (let [explanation (m/explain LayoutContext ctx)]
      (throw (ex-info "Invalid Layout Context"
                      {:errors (me/humanize explanation)})))))

(defn valid? [ctx]
  (m/validate LayoutContext ctx))
