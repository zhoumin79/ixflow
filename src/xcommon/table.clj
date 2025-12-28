(ns xcommon.table
  (:import [org.apache.poi.xslf.usermodel XMLSlideShow XSLFPictureData XSLFSlide XSLFPictureShape XSLFPictureData
            XSLFTable XSLFTableRow XSLFTableCell XSLFTextParagraph XSLFTextRun XSLFTextShape XSLFAutoShape XSLFSheet XSLFGroupShape XSLFConnectorShape]
           [org.apache.poi.sl.usermodel TableCell TableCell$BorderEdge  TextParagraph TextParagraph$TextAlign TextParagraph$FontAlign ShapeType VerticalAlignment TextShape$TextDirection]
           [org.openxmlformats.schemas.drawingml.x2006.main STTextFontAlignType STTextAlignType CTTextParagraphProperties CTGeomGuideList CTGeomGuide CTNonVisualConnectorProperties CTConnection STLineEndLength STLineEndType STLineEndWidth STShapeType]
           [org.apache.poi.common.usermodel.fonts FontGroup]
           [org.apache.poi.util Units]
           [java.awt Rectangle Color])
  (:require [xcommon.color :refer [make-color]]
            [xcommon.slide :refer [ppt-width]]))

(def default-table-style {:x                   20 :y 50
                          :col-fill-color      [254 134 55]
                          :row-fill-color      [255 175 126]
                          :odd-row-fill-color  [254 230 215]
                          :even-row-fill-color [255 243 235]
                          :col-font-color      :white
                          :row-font-color      :black
                          :border-color        :white
                          :font-size           30.0})

(defn null-to-default [goal-str default-str]
  (if (or (nil? goal-str) (empty? goal-str))
    default-str
    goal-str))

(defn get-ppr [ct-text-paragraph]
  (let [p-pr (.getPPr (.getXmlObject ct-text-paragraph))]
    (if (nil? p-pr) (.addNewPPr (.getXmlObject ct-text-paragraph)) p-pr)))

(defn set-ct-text-paragraph-vertical-align [ct-text-paragraph vertical-str]
  (let [p-pr (get-ppr ct-text-paragraph)]
    (case vertical-str
      "top" (.setFontAlgn p-pr STTextFontAlignType/T)
      "baseline" (.setFontAlgn p-pr STTextFontAlignType/BASE)
      "bottom" (.setFontAlgn p-pr STTextFontAlignType/B)
      "center" (.setFontAlgn p-pr STTextFontAlignType/CTR)
      (.setFontAlgn p-pr STTextFontAlignType/AUTO))))

(defn set-paragraph-vertical-align [paragraph vertical]
  (let [vertical (null-to-default vertical "auto")] (set-ct-text-paragraph-vertical-align paragraph (clojure.string/lower-case vertical))))

(defn set-ct-text-paragraph-horizontal-align [ct-text-paragraph horizontal-str]
  (let [p-pr (get-ppr ct-text-paragraph)]
    (case horizontal-str
      "left" (.setAlgn p-pr STTextAlignType/L)
      "right" (.setAlgn p-pr STTextAlignType/R)
      "center" (.setAlgn p-pr STTextAlignType/CTR)
      "disperse" (.setAlgn p-pr STTextAlignType/DIST)
      (.setAlgn p-pr STTextAlignType/JUST))))

(defn set-paragraph-horizontal-align [paragraph horizontal]
  (let [horizontal (null-to-default horizontal "auto")]
    (set-ct-text-paragraph-horizontal-align paragraph (clojure.string/lower-case horizontal))))

(defn set-cell-horizontal-align [cell horizontal-align]
  (let [text-paragraphs (.getTextParagraphs cell)]
    (doseq [text-paragraph text-paragraphs]
      (set-paragraph-horizontal-align text-paragraph horizontal-align))))

(defn set-cell-vertical-align [cell vertical-align]
  (let [text-paragraphs (.getTextParagraphs cell)]
    (doseq [text-paragraph text-paragraphs]
      (set-paragraph-vertical-align text-paragraph vertical-align))))

(defn set-table-cell-borders [^XSLFTableCell cell color & {:as args}]
  (doto cell
    (.setBorderColor TableCell$BorderEdge/top (or (make-color (:top-color args)) (make-color color)))
    (.setBorderColor TableCell$BorderEdge/right (or (make-color (:right-color args)) (make-color color)))
    (.setBorderColor TableCell$BorderEdge/bottom (or (make-color (:bottom-color args)) (make-color color)))
    (.setBorderColor TableCell$BorderEdge/left (or (make-color (:left-color args)) (make-color color)))))

(defn table
  "Add text to the slideshow."
  [^XSLFTable table & {:as args}]
  (let [;argmap (apply hash-map args)
        argmap (merge default-table-style args)
        table-anchor (Rectangle. (or (:x argmap) 50) (or (:y argmap) 200)
                                 0 0)
        ^XSLFTableRow header-row (.addRow table)]
    (.setHeight header-row (or (:col-h argmap) (:h argmap) 50))
    (.setAnchor table table-anchor)
    (doseq [col (:cols argmap)]
      (let [cell (.addCell header-row)
            ^XSLFTextParagraph p (.addNewTextParagraph cell)
            ;; ppr (-> p .getXmlObject .addNewPPr)
            ^XSLFTextRun r (.addNewTextRun p)]
        (do
          (.setText r col)
          ;; (.setSpaceAfter p 0)
          ;; (.setFontAlgn ppr STTextFontAlignType/CTR)
          ;; (.setAlgn ppr STTextFontAlignType/CTR)
          (.setTextAlign p TextParagraph$TextAlign/CENTER)
          (set-table-cell-borders cell (or (:border-color argmap) :white)
                                  :top-color (:top-color argmap) :right-color (:right-color argmap)
                                  :bottom-color (:bottom-color argmap) :left-color (:left-color argmap))
          ;(.setTextDirection cell TextShape$TextDirection/VERTICAL) ;文本方向
          (.setVerticalAlignment cell VerticalAlignment/MIDDLE)
          (set-cell-horizontal-align cell "center")
          (set-cell-vertical-align cell "center")
          (.setFillColor cell (or (make-color (:col-fill-color argmap)) nil))
          (.setFontSize r (or (:font-size argmap) nil))
          (.setFontColor  r  (or ^Color (make-color (:col-font-color argmap)) nil))
          (.setFontFamily r (or (:font-name argmap) "微软雅黑") FontGroup/EAST_ASIAN)
          (.setFontFamily r (or (:font-name argmap) "微软雅黑") FontGroup/LATIN))))

    (when (:cols-width-ratio argmap)
      (doseq [[j col-w] (zipmap (range 0 (count (:cols-width-ratio argmap))) (:cols-width-ratio argmap))]
        (.setColumnWidth table j (* col-w (/ (- (:ppt-width argmap) (* 2 (:x argmap))) (reduce + (:cols-width-ratio argmap)))))))

    (when (:cols-width argmap)
      (doseq [[j col-w] (zipmap (range 0 (count (:cols-width argmap))) (:cols-width argmap))]
        (.setColumnWidth table j col-w)))

    (doseq [[row i] (zipmap (:rows argmap) (range 0 (count (:rows argmap))))]
      (let [data-row  (.addRow table)]
        (.setHeight data-row (or (:row-h argmap) (:h argmap) 50))
        (doseq [cell-v row]
          (let [cell (.addCell data-row)
                p (.addNewTextParagraph cell)
                r (.addNewTextRun p)]
            (do
              (.setText r (str cell-v))
              (.setFillColor cell (or (make-color (:row-fill-color argmap)) nil))
              (if (odd? i) (.setFillColor cell (or (make-color (:odd-row-fill-color argmap)) nil))
                  (.setFillColor cell (or (make-color (:even-row-fill-color argmap)) nil)))
              (.setFontSize r (or (:font-size argmap) nil))
              (.setFontColor r (or ^Color (make-color (:row-font-color argmap)) nil))
              (.setFontFamily r (or (:font-name argmap) "微软雅黑") FontGroup/EAST_ASIAN)
              (.setFontFamily r (or (:font-name argmap) "微软雅黑") FontGroup/LATIN)
              (if (and (string? cell-v) (> (count cell-v) 10))
                (.setTextAlign p TextParagraph$TextAlign/LEFT)
                (.setTextAlign p TextParagraph$TextAlign/CENTER))
              ;; (set-table-cell-borders cell (or (:border-color argmap) Color/BLACK))
              (set-table-cell-borders cell (or (:border-color argmap) :white)
                                      :top-color (:top-color argmap) :right-color (:right-color argmap)
                                      :bottom-color (:bottom-color argmap) :left-color (:left-color argmap))
              (.setVerticalAlignment cell VerticalAlignment/MIDDLE)
              (.setFontAlign p TextParagraph$FontAlign/CENTER)
              (.setHorizontalCentered cell true)
              (set-cell-horizontal-align cell "center")
              (set-cell-vertical-align cell "center"))))))))

(defn add-table [slideshow & {:as args}]
  (let [curr-table (.createTable ^XSLFSlide (:slide slideshow))
        ppt-width (ppt-width (:ppt slideshow))
        args (merge {:ppt-width ppt-width} args)]
    (table curr-table args))
  slideshow)

(defn remove-row [^XSLFTable table i]
  (.remove (-> table  .getCTTable .getTrList)
           (dec (.getNumberOfRows table))); //Remove the last row from table.
  )

(comment

  (zipmap '(1 2 3 4 8) '(2 3 4 6 8))

  ;;注意下面会输出怪异的问题，导致表格各列宽不一致，zipmap去掉了重复，需将key不重复
  (zipmap [100 100 200 100 200] [0 1 2 3 4])
  ;; => {100 3, 200 4}

  (int (Math/round (float (/ (* 60 9) 256))))

  (int (* 72 (/ 200 (/ 914400  96))))

  Units/DEFAULT_CHARACTER_WIDTH
  (Math/round (/ (+ (* 60 Units/DEFAULT_CHARACTER_WIDTH)  5.0) (* Units/DEFAULT_CHARACTER_WIDTH 256.0)))

  (int (Math/round (float (*  200 (/ 96 72)))))
  (Math/round (float (*  200 (/ 96 72)))))
