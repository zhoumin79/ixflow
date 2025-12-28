(ns xcommon.path
  (:import [org.apache.poi.xslf.usermodel XSLFFreeformShape XMLSlideShow XSLFPictureData XSLFSlide
            XSLFTextParagraph XSLFTextRun XSLFTextShape XSLFAutoShape XSLFGroupShape XSLFConnectorShape]
           [org.apache.poi.sl.usermodel AutoShape LineDecoration LineDecoration$DecorationShape LineDecoration$DecorationSize TableCell TableCell$BorderEdge TextParagraph TextParagraph$TextAlign TextParagraph$FontAlign ShapeType VerticalAlignment]
           [org.apache.poi.sl.usermodel TextShape$TextDirection TextShape$TextAutofit TextParagraph$TextAlign]
           [org.openxmlformats.schemas.drawingml.x2006.main STTextFontAlignType CTTextParagraphProperties CTGeomGuideList CTGeomGuide CTNonVisualConnectorProperties CTConnection STLineEndLength STLineEndType STLineEndWidth STShapeType]
           [org.openxmlformats.schemas.presentationml.x2006.main CTConnector CTShape]
           [java.awt Rectangle Color Dimension Shape]
           [java.awt.geom PathIterator Ellipse2D Ellipse2D$Double Line2D Line2D$Double Path2D Path2D$Double Rectangle2D Rectangle2D$Double Point2D Point2D$Double Arc2D Arc2D$Double])
  (:require
   [xcommon.slide :refer [ppt-width ppt-height]]
   [xcommon.svg-path :as svg-path]))

;; shape - path
(def ^:private segment-types->keywords {PathIterator/SEG_CLOSE :close
                                        PathIterator/SEG_LINETO :line
                                        PathIterator/SEG_MOVETO :move
                                        PathIterator/SEG_QUADTO :quad
                                        PathIterator/SEG_CUBICTO :cubic})

(def ^:private winding-rules->keywords {PathIterator/WIND_EVEN_ODD :even-odd
                                        PathIterator/WIND_NON_ZERO :non-zero})

(defn- shape->path-seq
  ([iterator] (shape->path-seq iterator (double-array 6)))
  ([^PathIterator iterator ^doubles buff]
   (lazy-seq
    (when-not (.isDone iterator)
      (let [t (segment-types->keywords (.currentSegment iterator buff) :unknown)
            w (winding-rules->keywords (.getWindingRule iterator) :unknown)
            triplet [t (case t
                         :close nil
                         :line (vec (take 2 buff))
                         :move (vec (take 2 buff))
                         :quad (vec (take 4 buff))
                         :cubic (vec buff)
                         :unknown) w]]
        (.next iterator)
        (cons triplet (shape->path-seq iterator buff)))))))

(defn shape->path-def
  "Create path definition from a shape
  
  Returns sequence of triplets of [command coords winding-rule]:

  * `[:move [x y]]` - move to a position
  * `[:line [x y]]` - line to a position
  * `[:quad [x1 y1 x2 y2]` - curved line to a position
  * `[:cubic [x1 y1 x2 y2 x3 y3]]` - bezier line to a position
  * `[:close]` - close path
  
  Winding rule is one of `:even-odd` and `:non-zero` (default)  

  See [[path-def->shape]]"
  [^Path2D path2d]
  (shape->path-seq (.getPathIterator path2d nil)))

(defn- seg-moveto [^Path2D p [x1 y1]] (.moveTo p x1 y1))
(defn- seg-lineto [^Path2D p [x1 y1]] (.lineTo p x1 y1))
(defn- seg-quadto [^Path2D p [x1 y1 x2 y2]] (.quadTo p x1 y1 x2 y2))
(defn- seg-cubicto [^Path2D p [x1 y1 x2 y2 x3 y3]] (.curveTo p x1 y1 x2 y2 x3 y3))

(defn path-def->shape
  "Create a shape from path definition, see [[shape->path-def]].

  Path entry is a tripplet [command arguments [winding-rule]]

  * `[:move [x y]]` - move to a position
  * `[:line [x y]]` - line to a position
  * `[:quad [x1 y1 x2 y2]` - curved line to a position
  * `[:cubic [x1 y1 x2 y2 x3 y3]]` - bezier line to a position
  * `[:close]` - close path
  * `[:shape [shape-object connect?]]` - append given shape

  Winding rule can be one of `:even-odd` and `:non-zero` (default) and is optional. "
  [path-def]
  (let [path-def (cond
                   (string? path-def) (svg-path/svg-path->path-def path-def)
                   (and (seqable? path-def) (char? (first path-def))) (svg-path/svg-path->path-def (apply str path-def))
                   :else path-def)
        ^Path2D p (Path2D$Double.)]
    (doseq [[t v w] path-def]
      (when w (.setWindingRule p (case t
                                   :even-odd Path2D/WIND_EVEN_ODD
                                   :non-zero Path2D/WIND_NON_ZERO)))
      (case t
        :close (.closePath p)
        :line (seg-lineto p v)
        :quad (seg-quadto p v)
        :cubic (seg-cubicto p v)
        :move (seg-moveto p v)
        :shape (.append p ^Shape (first v) (if (second v) true false))))
    p))

#_(defn drawBetweenTwoPoints [shape ^double x1 ^double x2 ^double y1 ^double y2]
    (doto shape
      (.setAnchor (Rectangle2D$Double. (if (<= x1 x2) x1 x2) (if (<= y1 y2) y1 y2)
                                       (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))
      (.setFlipVertical (< y2 y1))))

(defn draw-between-two-points [^AutoShape shape x1 x2 y1 y2]
  (let [x1 (double x1)
        x2 (double x2)
        y1 (double y1)
        y2 (double y2)]
    (doto shape
      (.setAnchor (Rectangle2D$Double. (if (<= x1 x2) x1 x2) (if (<= y1 y2) y1 y2)
                                       (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))
      (.setFlipVertical (< y2 y1)))))

(defn space [slide x y]
  (let [width (- (ppt-width (:ppt slide)) (* 2 x))
        height (- (ppt-height (:ppt slide)) (* 2 y))]
    {:w width
     :h height}))

(defn set-radius [^XSLFAutoShape autoshape percent]
  (when (= (.getShapeType autoshape) ShapeType/ROUND_RECT)
    (let [^CTShape ctShape (.getXmlObject autoshape)
          ctGeomGuideList (-> ctShape .getSpPr .getPrstGeom .getAvLst)
          ctGeomGuide (.addNewGd ctGeomGuideList)]

      (doto ctGeomGuide
        (.setName "adj")
        (.setFmla (str "val " (* 50000 (/ percent 100))))))))

#_(defn drawRectWithText [^XSLFSlide slide x y width height text]
    (let [^AutoShape shape (.createAutoShape slide)
          rect (java.awt.Rectangle. x y width height)]
      (.setAnchor shape (.getBounds2D rect))
      (.setShapeType shape ShapeType/RECT)
      (.setText shape text)
      (.setTextDirection shape TextShape$TextDirection/HORIZONTAL)
      (.setVerticalAlignment shape VerticalAlignment/MIDDLE)
      (.setTextAutofit ^AutoShape shape TextShape$TextAutofit/NORMAL)
      (let [rect2D (.resizeToFitText shape)
            upscaledHeight (.getHeight rect2D)
            hScale (if (> upscaledHeight height) (/ height upscaledHeight) 1.0)]
        (println hScale)
        (.setFontScale (.getNormAutofit (.getBodyPr (.getXmlObject (.getTextBody shape)))) (* hScale 100000)))
      (.setAnchor shape (.getBounds2D rect))
      (let [xslfTextParagraph (.get (.getTextParagraphs shape) 0)]
        (.setTextAlign xslfTextParagraph TextParagraph$TextAlign/CENTER))))
