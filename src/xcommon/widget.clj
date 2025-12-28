(ns xcommon.widget
  (:import [org.apache.poi.xslf.usermodel XMLSlideShow XSLFPictureData XSLFSlide XSLFPictureShape XSLFPictureData XSLFFreeformShape
            XSLFTextParagraph XSLFTextRun XSLFTextShape XSLFAutoShape XSLFSheet XSLFGroupShape XSLFConnectorShape]
           [org.apache.poi.sl.usermodel TextParagraph$TextAlign TextShape$TextDirection TextShape$TextAutofit TextParagraph$TextAlign
            TextParagraph TextParagraph$FontAlign ShapeType StrokeStyle$LineDash VerticalAlignment LineDecoration LineDecoration$DecorationShape LineDecoration$DecorationSize]
           [org.openxmlformats.schemas.drawingml.x2006.main STTextFontAlignType CTTextParagraphProperties CTGeomGuideList CTGeomGuide CTNonVisualConnectorProperties CTConnection CTGradientFillProperties CTGradientStop CTGradientStopList]
           [org.openxmlformats.schemas.presentationml.x2006.main CTConnector CTShape]
           [java.awt Rectangle Color Dimension]
           [org.apache.poi.common.usermodel.fonts FontGroup]
           [java.awt.geom Rectangle2D Rectangle2D$Double AffineTransform])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [xcommon.gradient :refer [gradient-x-fill gradient-x-line gradient-x-text]]
            [xcommon.path :as path]
            [xcommon.color :refer [make-color get-random-color favor-colors gradient-colors]]
            [xcommon.macro :refer [whens]]
            [xcommon.text :refer [add-node-text]]))

(defn text-para-align [k]
  (case k
    :left TextParagraph$TextAlign/LEFT
    :center TextParagraph$TextAlign/CENTER
    :right TextParagraph$TextAlign/RIGHT
    TextParagraph$TextAlign/CENTER))

(defn verti-align
  "Returns vertical alignment"
  [k]
  (case k
    :top VerticalAlignment/TOP
    :bottom VerticalAlignment/BOTTOM
    :distributed VerticalAlignment/DISTRIBUTED
    :middle VerticalAlignment/MIDDLE
    :justified VerticalAlignment/JUSTIFIED
    VerticalAlignment/MIDDLE))

(defn text-shape-auto-fit [k]
  (case k
    :normal TextShape$TextAutofit/NORMAL
    :shape TextShape$TextAutofit/SHAPE
    :none TextShape$TextAutofit/NONE
    TextShape$TextAutofit/NORMAL))

(defn shape-type [k]
  (case k
    :rect ShapeType/RECT
    :arc ShapeType/ARC
    :text-box ShapeType/TEXT_BOX
    :line ShapeType/LINE
    :star-5 ShapeType/STAR_5
    :star-8 ShapeType/STAR_8
    :home-plate ShapeType/HOME_PLATE
    :round-rect ShapeType/ROUND_RECT
    :round-1-rect ShapeType/ROUND_1_RECT
    :round-2-diag-rect ShapeType/ROUND_2_DIAG_RECT
    :chevron ShapeType/CHEVRON
    :diamond ShapeType/DIAMOND
    :can ShapeType/CAN
    :oval ShapeType/ELLIPSE
    :ellipse ShapeType/ELLIPSE
    :teardrop ShapeType/TEARDROP
    :left-right-arrow ShapeType/LEFT_RIGHT_ARROW
    :straight-connector-1 ShapeType/STRAIGHT_CONNECTOR_1
    :heart ShapeType/HEART
    :triangle ShapeType/TRIANGLE
    :flow-chart-terminator ShapeType/FLOW_CHART_TERMINATOR
    :flow-chart-decision ShapeType/FLOW_CHART_DECISION
    :flow-chart-io ShapeType/FLOW_CHART_INPUT_OUTPUT
    :flow-chart-doc ShapeType/FLOW_CHART_DOCUMENT
    :flow-chart-multidoc ShapeType/FLOW_CHART_MULTIDOCUMENT
    :flow-chart-preparation ShapeType/FLOW_CHART_PREPARATION
    :flow-chart-process ShapeType/FLOW_CHART_PROCESS
    :sword [[:move [157.04070866141737 338.74102362204724]] [:cubic [155.14070866141736 337.84259842519685 152.64070866141736 335.7462992125984 151.54070866141737 334.2489763779528]] [:cubic [149.44062992125987 331.45385826771656 149.44062992125987 330.5554330708661 149.44062992125987 270.6611023622047]] [:cubic [149.44062992125987 210.76669291338584 149.44062992125987 209.86834645669293 151.54070866141737 207.0732283464567]] [:cubic [152.64070866141736 205.57590551181104 155.14070866141736 203.4796062992126 157.04070866141737 202.58118110236222]] [:cubic [160.24070866141736 200.88417322834647 175.94070866141737 200.78433070866143 441.9407086614174 200.78433070866143]] [:cubic [625.6407086614174 200.78433070866143 725.8407086614174 201.1836220472441 730.4407086614174 201.78259842519685]] [:cubic [762.3407086614174 206.47433070866143 787.9407086614174 229.13433070866142 797.1407086614173 261.17779527559054]] [:cubic [799.3407086614174 268.7644094488189 799.4407086614174 270.76094488188977 799.4407086614174 300.3088188976378]] [:cubic [799.4407086614174 330.15614173228346 799.3407086614174 331.55370078740157 797.3407086614174 334.2489763779528]] [:cubic [796.2407086614173 335.7462992125984 793.7407086614173 337.84259842519685 791.8407086614174 338.74102362204724]] [:cubic [788.6407086614173 340.438031496063 771.4407086614174 340.537874015748 474.4407086614173 340.537874015748]] [:cubic [177.44070866141737 340.537874015748 160.24070866141736 340.438031496063 157.04070866141737 338.74102362204724]] [:close nil]]
    ShapeType/RECT))

(defn dash-type [k]
  (case k
    :dash StrokeStyle$LineDash/DASH
    :dot StrokeStyle$LineDash/DOT
    :dash-dot StrokeStyle$LineDash/DASH_DOT
    :solid StrokeStyle$LineDash/SOLID
    StrokeStyle$LineDash/DASH))

(defn line-type [k]
  (case k
    :arrow LineDecoration$DecorationShape/ARROW
    :diamond LineDecoration$DecorationShape/DIAMOND
    :triangle LineDecoration$DecorationShape/TRIANGLE
    :oval LineDecoration$DecorationShape/OVAL
    :stealth LineDecoration$DecorationShape/STEALTH
    LineDecoration$DecorationShape/ARROW))

(defn set-color [^XSLFAutoShape shape color mode]
  (if (= :fill mode)
    (.setFillColor shape color)
    (.setLineColor shape color)))

(defn- apply-opacity [^Color c opacity]
  (if (and c opacity)
    (let [r (.getRed c)
          g (.getGreen c)
          b (.getBlue c)
          a (int (* 255 opacity))]
      (Color. r g b a))
    c))

(defn x-color [shape color mode & {:keys [gradient-angel opacity]
                                   :or {gradient-angel 180}}]
  (cond
    (= color :rand) (set-color shape (apply-opacity (get-random-color) opacity) mode)
    (= color :favor) (set-color shape (apply-opacity (make-color (rand-nth favor-colors)) opacity) mode)
    (= color :gradient) (let [colors (rand-nth (vals gradient-colors))]
                          (gradient-x-fill shape :colors colors :angle gradient-angel))
    (= color :gradient-rainbow) (let [colors (get gradient-colors :rainbow)]
                                  (gradient-x-fill shape :colors colors :angle gradient-angel))
    (= color [:rand :rand]) (gradient-x-fill shape :colors [(get-random-color) (get-random-color)]
                                             :angle gradient-angel)
    (= color [:favor :favor]) (gradient-x-fill shape :colors [(rand-nth favor-colors) (rand-nth favor-colors)])
    (and (vector? color) (= 2 (count color)) (= (first color) :rand) (not= (second color) :rand))
    (gradient-x-fill shape :colors [(get-random-color) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :rand) (= (second color) :rand))
    (gradient-x-fill shape :colors [(make-color (first color)) (get-random-color)]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (= (first color) :favor) (not= (second color) :favor))
    (gradient-x-fill shape :colors [(rand-nth favor-colors) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :favor) (= (second color) :favor))
    (gradient-x-fill shape :colors [(first color) (rand-nth favor-colors)]
                     :angle gradient-angel)
    (and (vector? color)
         (= 2 (count color))
         (or (every? string? color) (every? keyword? color)))
    (gradient-x-fill shape :colors color :angle gradient-angel)
    (and (vector? color) (< 2 (count color)) (every? string? color))
    (gradient-x-fill shape :colors color :angle gradient-angel)
    (and (vector? color)
         (<= 2 (count color))
         (every? (fn [c] (some? (make-color c))) color))
    (gradient-x-fill shape :colors color :angle gradient-angel)
    :else (set-color shape (apply-opacity (make-color color) opacity) mode)))

(defn x-line-color [shape color mode & {:keys [gradient-angel]
                                        :or {gradient-angel 90}}]
  (cond
    (= color :rand) (set-color shape (get-random-color) mode)
    (= color :favor) (set-color shape (make-color (rand-nth favor-colors)) mode)
    (= color :gradient) (let [colors (rand-nth (vals gradient-colors))]
                          (gradient-x-line shape :colors colors :angle gradient-angel))
    (= color :gradient-rainbow) (let [colors (get gradient-colors :rainbow)]
                                  (gradient-x-line shape :colors colors :angle gradient-angel))
    (= color [:rand :rand]) (gradient-x-line shape :colors [(get-random-color) (get-random-color)]
                                             :angle gradient-angel)
    (= color [:favor :favor]) (gradient-x-line shape :colors [(rand-nth favor-colors) (rand-nth favor-colors)])
    (and (vector? color) (= 2 (count color)) (= (first color) :rand) (not= (second color) :rand))
    (gradient-x-line shape :colors [(get-random-color) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :rand) (= (second color) :rand))
    (gradient-x-line shape :colors [(make-color (first color)) (get-random-color)]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (= (first color) :favor) (not= (second color) :favor))
    (gradient-x-line shape :colors [(rand-nth favor-colors) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :favor) (= (second color) :favor))
    (gradient-x-line shape :colors [(first color) (rand-nth favor-colors)]
                     :angle gradient-angel)
    (and (vector? color)
         (= 2 (count color))
         (or (every? string? color) (every? keyword? color)))
    (gradient-x-line shape :colors color :angle gradient-angel)
    (and (vector? color) (< 2 (count color)) (every? string? color))
    (gradient-x-line shape :colors color :angle gradient-angel)
    (and (vector? color)
         (<= 2 (count color))
         (every? (fn [c] (some? (make-color c))) color))
    (gradient-x-line shape :colors color :angle gradient-angel)
    :else (set-color shape (make-color color) mode)))

(defn x-text-color [^XSLFTextRun shape color & {:keys [gradient-angel]
                                                :or {gradient-angel 90}}]
  (cond
    (= color :rand) (.setFontColor shape ^Color (get-random-color))
    (= color :favor) (.setFontColor shape ^Color (make-color (rand-nth favor-colors)))
    (= color :gradient) (let [colors (rand-nth (vals gradient-colors))]
                          (gradient-x-text shape :colors colors :angle gradient-angel))
    (= color :gradient-rainbow) (let [colors (get gradient-colors :rainbow)]
                                  (gradient-x-text shape :colors colors :angle gradient-angel))
    (= color [:rand :rand]) (gradient-x-text shape :colors [(get-random-color) (get-random-color)]
                                             :angle gradient-angel)
    (= color [:favor :favor]) (gradient-x-text shape :colors [(rand-nth favor-colors) (rand-nth favor-colors)])
    (and (vector? color) (= 2 (count color)) (= (first color) :rand) (not= (second color) :rand))
    (gradient-x-text shape :colors [(get-random-color) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :rand) (= (second color) :rand))
    (gradient-x-text shape :colors [(make-color (first color)) (get-random-color)]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (= (first color) :favor) (not= (second color) :favor))
    (gradient-x-text shape :colors [(rand-nth favor-colors) (make-color (second color))]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (not= (first color) :favor) (= (second color) :favor))
    (gradient-x-text shape :colors [(first color) (rand-nth favor-colors)]
                     :angle gradient-angel)
    (and (vector? color) (= 2 (count color)) (or (every? string? color) (every? keyword? color))) (gradient-x-text shape :colors color
                                                                                                                   :angle gradient-angel)
    (and (vector? color) (< 2 (count color)) (every? string? color)) (gradient-x-text shape :colors color
                                                                                      :angle gradient-angel)
    :else (.setFontColor shape ^Color (make-color color))))

(defn x-widget [slide text & {:keys [x y w h type font-color font font-size bold italic
                                     text-align text-auto-fit v-align fill-color line-color angle
                                     line-head line-end line-dash line-width word-wrap h-center
                                     flip-vertical rotation gradient-angel opacity]
                              :or {x 27.18 y (* 2.38 28.35)
                                   w (* 4.23 28.35)
                                   h (* 1.06 28.35)
                                   type :rect
                                   font-color Color/BLACK
                                   font "微软雅黑"
                                   font-size 18.0
                                   bold false
                                   flip-vertical false
                                   text-align :center
                                   v-align :middle
                                   word-wrap false
                                   gradient-angel 90}}]

  (let [^XSLFSlide my-slide (if (map? slide) (:slide slide) slide)
        is-path-def? (and (vector? type)
                          (seq type)
                          (every? vector? type)
                          (every? #(keyword? (first %)) type))
        x-type (if is-path-def? type (shape-type type))
        ^XSLFAutoShape shape (if-not (vector? x-type)
                               (.createAutoShape ^XSLFSlide my-slide)
                               (.createFreeform ^XSLFSlide my-slide))
        normalized-text (cond
                          (nil? text) nil
                          (string? text) (when-not (str/blank? text) text)
                          :else (str text))]

    (if (vector? x-type)
      (let [free-path (path/path-def->shape x-type)
            bounds (.getBounds2D free-path)
            tx (- (.getX bounds))
            ty (- (.getY bounds))]
        (.transform free-path (AffineTransform/getTranslateInstance tx ty))
        (.setPath ^XSLFFreeformShape shape free-path))
      (.setShapeType shape x-type))

    (.setAnchor shape (Rectangle2D$Double. (double x) (double y) (double w) (double h)))

    (whens
     fill-color (x-color shape fill-color :fill :gradient-angel gradient-angel :opacity opacity)
     line-color (x-line-color shape line-color :line :gradient-angel gradient-angel)
     angle (.setRotation shape (double angle))
     v-align (.setVerticalAlignment shape (verti-align v-align))
     text-auto-fit (.setTextAutofit shape (text-shape-auto-fit text-auto-fit))
     line-head (.setLineHeadDecoration shape (line-type line-head))
     line-end (.setLineTailDecoration shape (line-type line-end))
     line-dash (.setLineDash shape (dash-type line-dash))
     line-width (.setLineWidth shape (double line-width))
     flip-vertical (.setFlipVertical shape flip-vertical)
     rotation (.setRotation shape (double rotation))
     word-wrap (.setWordWrap shape word-wrap)
     h-center (.setHorizontalCentered shape h-center))

    (when normalized-text
      (let [^XSLFTextParagraph text-p (.addNewTextParagraph shape)
            ^XSLFTextRun tr (.addNewTextRun text-p)]
        (.setText tr normalized-text)
        (whens
         font-color (x-text-color tr font-color :gradient-angel gradient-angel)
         font-size (.setFontSize tr (double font-size))
         font (do (.setFontFamily tr font FontGroup/EAST_ASIAN)
                  (.setFontFamily tr font FontGroup/LATIN))
         bold (.setBold tr bold)
         text-align (.setTextAlign text-p (text-para-align text-align))
         italic (.setItalic tr italic))))

    (if (map? slide)
      (assoc slide :shape shape)
      slide)))

(defn diagram [^XSLFSlide slide text & {:as args}]

  (x-widget slide text (merge args {:type :rect
                                    :v-align :top
                                    :text-align :center})))

(defn app [^XSLFSlide slide text {:keys [x y style]
                                  :or {x 27.18 y (* 2.38 28.35)
                                       style {:w (* 4.23 28.35) :height (* 1.06 28.35)
                                              :font-color Color/WHITE :font "微软雅黑" :font-size 18.0
                                              :fill-color (Color. 129 211 26)}}}]

  (x-widget slide text :type :rect {:x x :y y :style style}))

(defn db
  [slide text & {:as args}]
  (x-widget slide text (merge args {:type :can})))

(defn freeshape
  ([slide path-def x y w h fill-color]
   (let [^XSLFSlide slide (if (map? slide) (:slide slide) slide)
         shape ^XSLFFreeformShape (.createFreeform slide)
         free-path (path/path-def->shape path-def)]
     (doto shape
       (.setPath free-path)
       (.setAnchor (Rectangle2D$Double. (double x) (double y) (double w) (double h)))
       (.setLineWidth 3)
       ;; (.setLineColor line-color)
       (.setFillColor fill-color)))
   slide)
  ([slide path-def fill-color rotation]
   (let [^XSLFSlide slide (if (map? slide) (:slide slide) slide)
         shape ^XSLFFreeformShape (.createFreeform slide)
         free-path (path/path-def->shape path-def)]
     (doto shape
       (.setPath free-path)
       (.setLineWidth 3)
       ;; (.setLineColor line-color)
       (.setFillColor fill-color)
       (.setRotation rotation)))
   slide))

(defn freeshapes
  ([slide path-defs fill-colors rotations]
   (doseq [[[path-def fill-color] rotation] (zipmap (zipmap path-defs fill-colors) rotations)]
     (freeshape slide path-def fill-color rotation))
   slide)
  ([slide pptx n fill-colors rotations]
   (let [free-shapes (->> ^XSLFSlide (nth (.getSlides (XMLSlideShow. (io/input-stream (io/resource pptx)))) n)
                          .getShapes
                          (filter #(instance? XSLFFreeformShape %)))
         path-defs (->> free-shapes
                        (map #(.getPath ^XSLFFreeformShape %))
                        (mapv #(mapv drop-last (path/shape->path-def %))))
         fill-colors (if (nil? fill-colors) (map #(.getFillColor ^XSLFFreeformShape %) free-shapes) fill-colors)
         rotations (if (nil? rotations) (map #(.getRotation ^XSLFFreeformShape %) free-shapes) rotations)]
     (freeshapes slide path-defs fill-colors rotations))
   slide))

(defn make-widgets [slide m-widgets]
  (doseq [{x :x y :y w :w h :h text :text type :type font :font font-size :font-size
           font-color :font-color fill-color :fill-color line-color :line-color line-width :line-width
           rotation :rotation text-align :text-align style :style flip-vertical :flip-vertical gradient-angel :gradient-angel} m-widgets]
    (x-widget slide (or text "") :x x :y y :w w :h h :type (or (get style :type) type :rect) :fill-color (or fill-color (get style :fill-color))
              :font (or font "微软雅黑") :font-size (or (get style :font-size) font-size 18.0) :font-color (or font-color (get style :font-color))
              :line-color (or line-color (get style :line-color))
              :line-width (or (get style :line-width) line-width 1.0)
              :rotation (or rotation 0)
              :flip-vertical flip-vertical
              :text-align (or text-align :center)
              :gradient-angel gradient-angel))
  slide)

(defn org-widget [slide shape-opts]
  (let [{x :x y :y w :w h :h text :text type :type font :font font-size :font-size
         font-color :font-color fill-color :fill-color line-color :line-color line-width :line-width
         rotation :rotation text-align :text-align style :style flip-vertical :flip-vertical gradient-angel :gradient-angel} shape-opts]
    (x-widget slide (or text "") :x x :y y :w w :h h :type (or (get style :type) type :rect) :fill-color (or fill-color (get style :fill-color))
              :font (or font "微软雅黑") :font-size (or (get style :font-size) font-size 18.0) :font-color (or font-color (get style :font-color))
              :line-color (or line-color (get style :line-color))
              :line-width (or (get style :line-width) line-width 1.0)
              :rotation (or rotation 0)
              :flip-vertical flip-vertical
              :text-align (or text-align :center)
              :gradient-angel gradient-angel)))

(defn draw-between-two-points [slide x1 x2 y1 y2 line-color line-width]
  (let [^XSLFSlide slide (if (map? slide) (:slide slide) slide)
        shape (.createAutoShape ^XSLFSlide slide)
        x1 (double x1)
        x2 (double x2)
        y1 (double y1)
        y2 (double y2)]
    (doto shape
      (.setShapeType ShapeType/LINE)
      (.setLineColor (make-color line-color))
      (.setLineWidth line-width)
      (.setAnchor (Rectangle2D$Double. (if (<= x1 x2) x1 x2) (if (<= y1 y2) y1 y2)
                                       (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))
      (.setFlipHorizontal (> x1 x2)))
    ;; (when (> x1 x2) (.setFlipVertical shape true))
    ))

#_(defn draw-lines [slide v-lines line-color line-width]
    (doseq [[_ x1 y1 x2 y2] v-lines]
      (draw-between-two-points slide x1 x2 y1 y2 line-color line-width))
    slide)

(defn draw-lines [slide v-lines line-color line-width]
  (doseq [{x1 :x1 y1 :y1 x2 :x2 y2 :y2} v-lines]
    (draw-between-two-points slide x1 x2 y1 y2 line-color line-width))
  slide)
