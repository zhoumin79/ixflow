(ns xcommon.gradient
  (:import [java.awt.geom Rectangle2D Rectangle2D$Double]
           [org.apache.poi.xslf.usermodel XSLFAutoShape XSLFSlide XSLFTextRun]
           [org.openxmlformats.schemas.drawingml.x2006.main CTRegularTextRun CTGradientFillProperties CTLineProperties CTGradientStop CTGradientStopList CTTextCharacterProperties]
           [org.openxmlformats.schemas.presentationml.x2006.main CTShape CTBackgroundProperties CTBackground])
  (:require [xcommon.color :refer [make-color color->rgb gradient-colors]]
            ;; [idoc.slide :refer [make-ppt save-ppt]]
            ;; [idoc.ppt.widget :refer [make-widgets]]
            ))

(defn fill [slide shape-type base-color]
  (let [shape (.createAutoShape ^XSLFSlide (:slide slide))
        anchor (Rectangle2D$Double. 100 100 100 100)
        ^CTShape ctshape (.getXmlObject shape)
        ^CTGradientFillProperties gfill (-> ctshape .getSpPr .addNewGradFill)
        ^CTGradientStopList list (.addNewGsLst gfill)
        start-stop (.addNewGs list)
        end-stop (.addNewGs list)
        srgbclr1 (.addNewSrgbClr start-stop)
        srgbclr2 (.addNewSrgbClr end-stop)]
    (doto shape
      (.setShapeType shape-type)
      (.setAnchor anchor))
    (.setAng (.addNewLin gfill) 1800000)
    (doto start-stop
      (.setPos 0))
    (.setVal srgbclr1 (byte-array base-color))
    (doto end-stop
      (.setPos 100000))
    (.setVal srgbclr2 (byte-array [247 150 70])))
  slide)

(defn gradient-fill
  ([^XSLFAutoShape shape & {:keys [from-color middle-color to-color
                                   angle]
                            :or {angle 180}}]
   (let [^CTShape ctshape (.getXmlObject shape)
         ^CTGradientFillProperties gfill (-> ctshape .getSpPr .addNewGradFill)
         ^CTGradientStopList list (.addNewGsLst gfill)
         start-stop (.addNewGs list)
         middle-stop (.addNewGs list)
         end-stop (.addNewGs list)
         srgbclr1 (.addNewSrgbClr start-stop)
         srgbclr2 (.addNewSrgbClr middle-stop)
         srgbclr3 (.addNewSrgbClr end-stop)]
     (.setAng (.addNewLin gfill) (* angle 6 10000))
     (doto start-stop
       (.setPos 0))
     (.setVal srgbclr1 (byte-array (color->rgb from-color)))
     (when middle-color
       (doto middle-stop
         (.setPos 50000))
       (.setVal srgbclr2 (byte-array (color->rgb middle-color))))
     (doto end-stop
       (.setPos 100000) ;set the end pos (100000 = 100%)
       )
     (.setVal srgbclr3 (byte-array (color->rgb to-color))))))

(defn gradient-x-fill
  ([^XSLFAutoShape shape & {:keys [colors angle opacity]
                            :or {angle 180}}]
   (let [^CTShape ctshape (.getXmlObject shape)
         ^CTGradientFillProperties gfill (-> ctshape .getSpPr .addNewGradFill)
         ^CTGradientStopList list (.addNewGsLst gfill)
         cnt (count colors)]
     (.setAng (.addNewLin gfill) (* angle 6 10000))

     (doseq [[idx color] (zipmap (range cnt) colors)]
       (let [srgb (.addNewSrgbClr (doto (.addNewGs list)
                                    (.setPos (* idx (int (/ 100 (dec cnt))) 1000))))]
         (.setVal srgb (byte-array (color->rgb color)))
         (when opacity
           (.setVal (.addNewAlpha srgb) (int (* opacity 100000)))))))))

(defn gradient-x-text
  ([^XSLFTextRun r & {:keys [colors angle]
                      :or {angle 180}}]
   (let [^CTRegularTextRun run (.getXmlObject r)
         ^CTTextCharacterProperties cttext (-> run .getRPr)
         ^CTGradientFillProperties gfill (.addNewGradFill cttext)
         ^CTGradientStopList list (.addNewGsLst gfill)
         cnt (count colors)]
     (.setAng (.addNewLin gfill) (* angle 6 10000))

     (doseq [[idx color] (zipmap (range cnt) colors)]
       (doto (.addNewSrgbClr (doto (.addNewGs list)
                               (.setPos (* idx (int (/ 100 (dec cnt))) 1000))))
         (.setVal (byte-array (color->rgb color))))))))

(defn gradient-x-line
  ([^XSLFAutoShape shape & {:keys [colors angle]
                            :or {angle 180}}]
   (let [^CTShape ctshape (.getXmlObject shape)
         ^CTLineProperties xline (-> ctshape .getSpPr .addNewLn)
         ^CTGradientFillProperties gfill (.addNewGradFill xline)
         ^CTGradientStopList list (.addNewGsLst gfill)
         cnt (count colors)]
     (.setAng (.addNewLin gfill) (* angle 6 10000))

     (doseq [[idx color] (zipmap (range cnt) colors)]
       (doto (.addNewSrgbClr (doto (.addNewGs list)
                               (.setPos (* idx (int (/ 100 (dec cnt))) 1000))))
         (.setVal (byte-array (color->rgb color))))))))

(comment

  (defn gradient-shapes [colors]
    (for [color colors]
      {:x (rand-int 900)
       :y (rand-int 500)
       :w 100
       :h 100
       :fill-color color
       :type :rect
       :gradient-angel 180}))

  (gradient-shapes [[:black :blue] [:purple600 :sky200]])

  (def bg-gradient-colors
    {:blue-purple ["#003C71" "#5C4AC7" "#B46CBB"]
     :rainbow ["#FF0000" "#FF7F00" "#FFFF00" "#00FF00" "#00FFFF" "#0000FF" "#4B0082" "#9400D3"] ;漂亮
     :starsky ["#2A0845" "#3D2A91" "#A86DD8" "#FFC3D1"]
     :spring-garden ["#FFB3BA" "#FFDAC1" "#FFABAB" "#FFC3A0" "#FFD93D"]
     :dream-rainbow ["#FF5C00" "#FFA900" "#FFEC40" "#A0D95E" "#3B8BF0"] ;漂亮
     :green-nature ["#E8F5E9" "#B2EBC5" "#8BC34A" "#4CAF50" "#2E7D32"] ;绿色自然,漂亮
     :forest ["#D4EDDA" "#A8D5BA" "#6FDDA9" "#3A9A5A" "#2E5E3D"] ;清爽森林
     :shentaihx ["#F1F8E9" "#C8E6C9" "#81C784" "#388E3C" "#005A38"] ;生态和谐，漂亮
     :morning ["#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA" "#00ACC1"] ;晨露之美,青蓝色
     :future-tech ["#e3f2fd" "#bbdefb" "#90caf9" "#64b5f6" "#42a5f5"]
     :deep-sea-tech ["#eef4f8" "#cfd8e2" "#90a4ae" "#546e7a" "#263238"]
     :dream-blue-purple ["#D1C4E9" "#B39DDB" "#7E57C2" "#5C6BC0" "#2196F3"] ;漂亮 梦幻蓝紫渐变
     :tech-sunrise ["#FFEBEE" "#FFCDD2" "#EF9A9A" "#F48FB1" "#F06292"] ;科技日出渐变
     :ice-blue ["#E0F7FA" "#B2EBF2" "#80DEEA" "#26C6DA" "#00BFD4"] ;冰蓝迷雾渐变
     :frost ["#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"]
     :rainbow-starsky ["#FFCCFF" "#FF99CC" "#FF66B2" "#FF3399" "#FF0066"] ;幻彩星空渐变,粉红
     :x-dream1 ["#A500B5" "#7A1EA8" "#4E2BB9" "#3E5CDA" "#1A85E0"]
     :x-dream2 ["#A500B5" "#7A1EA8" "#3920B9" "#2636C7" "#1A85E0"]
     :x-dream3 ["#F4D9E4" "#D6A0E9" "#FFA07A" "#F5E1D0" "#A0D3FF"]
     :x-dream8 ["#FFF0E6" "#A77BCE" "#E8D7E5" "#4DC7FF" "#D5C9FF"]
     :x-dream9 ["#FFEBD8" "#A78BCC" "#F4EFF6" "#8EDBF5" "#E4D8FF"]
     :x-rainbow-dream ["#FFEBD8" ;/* 左上 */
                       "#F7E2DD" ;/* 左中 */
                       "#BB9AF1" ;/* 左下 */
                       "#F4F3F1" ;/* 中上 */
                       "#F8F2F6" ;/* 中心 */
                       "#ECE2FB" ;/* 中下 */
                       "#46C2FE" ;/* 右上 */
                       "#9AD0FF" ;/* 右中 */
                       "#DDD4FF" ;/* 右下 */
                       ]
     :x ["#FFEBD8" ;/* 左上 */  
         "#BB9AF1" ;/* 左下 */  
         "#F4F3F1" ;/* 中间 */  
         "#46C2FE" ;/* 右上 */  
         "#DDD4FF" ; /* 右下 */
         ]})

  (def bg-colors {:gradient-background
                  {"科技与现代" [["#00C6FF" "#0072FF" "#6A82FB"] ; 清新动感，富有科技感  
                            ["#24C6DC" "#514A9D" "#4CA1AF"]] ; 冷暖交融，前卫设计  

                   "艺术与创意" [["#FF5F6D" "#FFC371" "#FF8C00"] ; 温暖丰富，激发灵感  
                            ["#FF7E5F" "#FEB47B" "#FF6F61"]] ; 柔和梦幻，充满情感  

                   "商业与专业" [["#3F2A80" "#A45D99" "#B9FBC0"] ; 现代感，干练且专业  
                            ["#6A3093" "#A044FF" "#D5AAFF"]] ; 简约而优雅  

                   "自然与环境" [["#00b09b" "#96c93d" "#AEEEEE"] ; 生态清新，生机勃勃  
                            ["#FF5F6D" "#FAD0C4" "#FFD7A0"]] ; 亲切自然，柔和温暖  

                   "浪漫与温馨" [["#FBD3C1" "#FF6B6B" "#FFB6B9"] ; 浪漫与甜美的混合  
                            ["#FF0080" "#FBD3C1" "#FFDD94"]] ; 温暖柔和，包容感强  

                   "梦幻与幻想" [["#6190E8" "#A7BFE8" "#FFC3A0"] ; 清新轻盈，如梦幻般的色调  
                            ["#FF9D00" "#FCE77D" "#F5D02B"]] ; 明亮与富有想象力  

                   "青春与活力" [["#FFB75E" "#ED8F18" "#FFE69A"] ; 活力四射，阳光明媚  
                            ["#6F8FDC" "#D5E1FF" "#A6CEEF"]] ; 清新动感，积极向上  

                   "优雅与奢华" [["#F7AB8A" "#C38CDA" "#E3AAFF"] ; 奢华魅力，优雅感强  
                            ["#00BFFF" "#1E90FF" "#ADD8E6"]] ; 清新高档，现代感  

                   "科技未来" [["#2B32B2" "#1488CC" "#D5E1EF"] ; 深邃未来的氛围  
                           ["#1D2B64" "#F8CDDA" "#FFC3A0"]] ; 神秘与梦幻的结合  

                   "神秘与幻想" [["#F3A683" "#F7B731" "#F6D6B2"] ; 温柔与神秘结合  
                            ["#6F1D99" "#7A44D7" "#A4B0B1"]] ; 迷幻与创意，深邃而引人入胜  

                   "冰冷与清新" [["#3a6073" "#55aacc" "#B2E6E9"] ; 冰冷清新，宁静清澈  
                            ["#74ebd5" "#ACB6E5" "#D6E9C6"]]} ; 平静安详，多样纹理  
                  })

  (-> (make-ppt (str "gradient" ".pptx") :bg-color {:colors ["#FF0080" "#FF7F50" "#FFD700" "#87CEEB" "#6A5ACD"]
                                                    :angel 225}
                #_["#FFEBD8" ;/* 左上 */
                   "#BB9AF1" ;/* 左下 */  
                   "#F4F3F1" ;/* 中间 */  
                   "#46C2FE" ;/* 右上 */  
                   "#DDD4FF" ; /* 右下 */
                   ])
      ;; (make-widgets (gradient-shapes (vals gradient-colors)))
      (save-ppt)))

(defn gradient-line
  ([^XSLFAutoShape shape & {:keys [from-color middle-color to-color
                                   angle]
                            :or {angle 180}}]
   (let [^CTShape ctshape (.getXmlObject shape)
         ^CTLineProperties xline (-> ctshape .getSpPr .addNewLn)
         ^CTGradientFillProperties gfill (.addNewGradFill xline)
         ^CTGradientStopList list (.addNewGsLst gfill)
         start-stop (.addNewGs list)
         middle-stop (.addNewGs list)
         end-stop (.addNewGs list)
         srgbclr1 (.addNewSrgbClr start-stop)
         srgbclr2 (.addNewSrgbClr middle-stop)
         srgbclr3 (.addNewSrgbClr end-stop)]
     (.setAng (.addNewLin gfill) (* angle 6 10000))
     (doto start-stop
       (.setPos 0))
     (.setVal srgbclr1 (byte-array (color->rgb from-color)))
     (when middle-color
       (doto middle-stop
         (.setPos 50000))
       (.setVal srgbclr2 (byte-array (color->rgb middle-color))))
     (doto end-stop
       (.setPos 100000) ;set the end pos (100000 = 100%)
       )
     (.setVal srgbclr3 (byte-array (color->rgb to-color))))))

;; (byte-array [247 150 70])
;linear-gradient(90deg,#da30ff,#242bff,#61abff)

(defn gradient-slide
  ([^XSLFSlide slide & {:keys [from-color middle-color to-color angle]
                        :or {angle 180}}]
   (let [ctslide (.getXmlObject slide)
         csld (.getCSld ctslide)
         ^CTBackground ctbackground (if (.isSetBg csld)
                                      (.getBg csld)
                                      (.addNewBg csld))]
     (when (.isSetBgRef ctbackground)
       (.unsetBgRef ctbackground))
     (when (.isSetBgPr ctbackground)
       (.unsetBgPr ctbackground))
     (let [^CTBackgroundProperties bg (.addNewBgPr ctbackground)
           ^CTGradientFillProperties gfill (.addNewGradFill bg)
           ^CTGradientStopList list (.addNewGsLst gfill)]
       (.setAng (.addNewLin gfill) (* angle 60000))
       (let [start-stop (.addNewGs list)
             srgbclr1 (.addNewSrgbClr start-stop)]
         (.setPos start-stop 0)
         (.setVal srgbclr1 (byte-array (color->rgb from-color))))
       (when middle-color
         (let [middle-stop (.addNewGs list)
               srgbclr2 (.addNewSrgbClr middle-stop)]
           (.setPos middle-stop 50000)
           (.setVal srgbclr2 (byte-array (color->rgb middle-color)))))
       (let [end-stop (.addNewGs list)
             srgbclr3 (.addNewSrgbClr end-stop)]
         (.setPos end-stop 100000)
         (.setVal srgbclr3 (byte-array (color->rgb to-color))))))
   slide))

(defn gradient-x-slide
  ([^XSLFSlide slide & {:keys [colors angle]
                        :or {angle 180}}]
   (let [ctslide (.getXmlObject slide)
         csld (.getCSld ctslide)
         ^CTBackground ctbackground (if (.isSetBg csld)
                                      (.getBg csld)
                                      (.addNewBg csld))]
     (when (.isSetBgRef ctbackground)
       (.unsetBgRef ctbackground))
     (when (.isSetBgPr ctbackground)
       (.unsetBgPr ctbackground))
     (let [^CTBackgroundProperties bg (.addNewBgPr ctbackground)
           ^CTGradientFillProperties gfill (.addNewGradFill bg)
           ^CTGradientStopList list (.addNewGsLst gfill)
           cnt (count colors)
           step (if (> cnt 1)
                  (/ 100000.0 (dec cnt))
                  0.0)]
       (.setAng (.addNewLin gfill) (* angle 60000))
       (doseq [[idx color] (zipmap (range cnt) colors)]
         (let [stop (.addNewGs list)
               srgb (.addNewSrgbClr stop)
               pos (int (Math/round (* idx step)))]
           (.setPos stop pos)
           (.setVal srgb (byte-array (color->rgb color)))))))
   slide))
