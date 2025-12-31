(ns xflow.layout.spec
  (:require [malli.core :as m]))

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
  (m/merge Point Dimension))

;; --- Node Spec ---
(def Node
  [:map
   [:id :string]
   [:type [:enum :start :end :process :decision :pool :dummy]] ;; Added :dummy for algorithm internal use
   [:label {:optional true} :string]
   ;; Layout properties (mutable during algorithm steps)
   [:x {:optional true} number?]
   [:y {:optional true} number?]
   [:w {:optional true} number?]
   [:h {:optional true} number?]
   [:rank {:optional true} int?] ;; Layer assignment
   [:order {:optional true} int?] ;; Order within layer
   [:swimlane-id {:optional true} [:or :string :nil]]
   ;; Internal flags
   [:dummy? {:optional true} :boolean]
   [:parent-edge {:optional true} :any]]) ;; Reference to original edge for dummy nodes

;; --- Edge Spec ---
(def Edge
  [:map
   [:from :string]
   [:to :string]
   [:id {:optional true} :string]
   [:label {:optional true} :string]
   [:type {:optional true} :string]
   ;; Routing results
   [:points {:optional true} [:vector Point]]])

;; --- Swimlane Spec ---
(def Swimlane
  [:map
   [:id :string]
   [:label :string]
   [:index int?]
   [:nodes [:vector Node]]
   [:size {:optional true} number?] ;; Computed width/height
   [:x {:optional true} number?]
   [:y {:optional true} number?]
   [:w {:optional true} number?]
   [:h {:optional true} number?]
   [:vertical? {:optional true} :boolean]])

;; --- Configuration Spec ---
(def LayoutOptions
  [:map
   [:direction {:optional true} [:enum "tb" "lr"]]
   [:layout {:optional true} [:enum "simple" "cluster" "swimlane"]]
   [:routing {:optional true} [:enum "manhattan" "spline"]]
   [:swimlane-mode {:optional true} [:enum "horizontal" "vertical"]]
   [:align {:optional true} [:enum "ul" "ur" "dl" "dr"]] ;; Alignment within ranking
   [:node-sep {:optional true} number?]
   [:rank-sep {:optional true} number?]
   [:edge-sep {:optional true} number?]])

;; --- Context Spec (State passed through pipeline) ---
(def LayoutContext
  [:map
   [:nodes [:vector Node]]
   [:edges [:vector Edge]]
   [:swimlanes {:optional true} [:vector Swimlane]]
   [:options LayoutOptions]
   [:width {:optional true} number?]
   [:height {:optional true} number?]])
