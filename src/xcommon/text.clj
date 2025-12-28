(ns xcommon.text
  (:import [org.apache.poi.sl.usermodel TextParagraph$TextAlign TextParagraph$FontAlign ShapeType VerticalAlignment]
           [org.apache.poi.xslf.usermodel XMLSlideShow XSLFPictureData XSLFSlide XSLFPictureShape XSLFPictureData XSLFFreeformShape
            XSLFTextParagraph XSLFTextRun XSLFTextShape XSLFAutoShape XSLFSheet XSLFGroupShape XSLFConnectorShape]
           [org.apache.poi.common.usermodel.fonts FontGroup]
           [java.awt Rectangle Color]))

(defn add-text
  "Add text to the slideshow."
  [slideshow text & args]
  (let [argmap     (apply hash-map args)
        txt-anchor (Rectangle. (or (:x argmap) 0) (or (:y argmap) 0)
                               (or (:width argmap) 100) (or (:height argmap) 100))
        text-box   (.createAutoShape ^XSLFSlide (:slide slideshow))
        text-p     (do
                 (.clearText text-box)
                 (.setShapeType text-box (or (:shape-type argmap) ShapeType/RECT))
                 (.setWordWrap text-box (or (:word-wrap argmap) false))
                 (.setLineColor text-box (or (:line-color argmap) Color/WHITE))
                 ;; (.setLineWidth text-box 3.0)
                 (.addNewTextParagraph text-box))
        tr         (.addNewTextRun text-p)]
    ;; (.clearText text-box)
    (.setText tr text)
    ;; (.setFontAlign text-p TextParagraph$FontAlign/CENTER)
    (.setTextAlign text-p TextParagraph$TextAlign/CENTER)
    (.setFontColor tr (or ^Color (:font-color argmap) Color/black))
    (.setFontSize tr (or (:font-size argmap) 28.0))
    ;; (.setFontFamily tr (or (:font-name argmap) "微软雅黑"))
    (.setFontFamily tr (or (:font-name argmap) "微软雅黑") FontGroup/EAST_ASIAN)
    (.setFontFamily tr (or (:font-name argmap) "微软雅黑") FontGroup/LATIN)
    (.setBold tr (or (:bold argmap) false))
    (.setFillColor text-box (or (:fill-color argmap) nil))
    (.setHorizontalCentered text-box true)
    (.setAnchor text-box txt-anchor)
    (.setVerticalAlignment text-box VerticalAlignment/MIDDLE)
    slideshow))

(defn add-texts [slide texts]
  (let [cnt (count texts)
        width (/ 900.0 cnt)]
    (doseq [[text x] (zipmap texts (take cnt (iterate (partial + (+ 10 width)) 20)))]
      (add-text slide text :x x :y 100 :width width :height 36 :font-size 14.0 :font-color Color/WHITE :fill-color Color/RED)))
  slide)

(defn add-node-text
  [^XSLFAutoShape autoshape text & args]
  (let [argmap (apply hash-map args)
        text-p (.addNewTextParagraph autoshape)
        tr     (.addNewTextRun text-p)]
    (.setText tr text)
    ;; (.setHorizontalCentered autoshape true)
    ;; (.setFontAlign text-p TextParagraph$FontAlign/CENTER)
    (when (true? (:bullet argmap)) (do (.setBullet text-p true) (.setLineSpacing text-p 150.0)))
    ;; (.setVerticalAlignment autoshape VerticalAlignment/MIDDLE)
    (when (not (:left argmap)) (.setVerticalAlignment autoshape (if (= (:pos argmap) "top") VerticalAlignment/TOP VerticalAlignment/MIDDLE)))
    (when-not (or (:bullet argmap) (:left argmap)) (.setTextAlign text-p TextParagraph$TextAlign/CENTER))
    (when  (= (:pos argmap) "right") (.setTextAlign text-p TextParagraph$TextAlign/RIGHT))
    (.setFontColor tr (or ^Color (:font-color argmap) Color/BLACK))
    (.setFontSize tr (or (:font-size argmap) 10.0))
    (.setFontFamily tr (or (:font-name argmap) "微软雅黑") FontGroup/EAST_ASIAN)
    (.setFontFamily tr (or (:font-name argmap) "微软雅黑") FontGroup/LATIN)
    (.setBold tr (or (:bold argmap) false))))
