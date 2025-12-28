(ns xcommon.slide
  (:require [clojure.java.io :as io]
            [xcommon.color :refer [make-color]]
            [xcommon.gradient :refer [gradient-x-slide]])
  (:import [org.apache.poi.xslf.usermodel XMLSlideShow XSLFSlideMaster SlideLayout XSLFSlideLayout
            SlideLayout XSLFSlide]
           [java.io FileOutputStream]
           (org.apache.poi.openxml4j.util ZipSecureFile)
           [java.awt Dimension]))

(defn get-slide-masters [^XMLSlideShow ppt]
  (vec (.getSlideMasters ppt)))

;; Wrapper function for getLayout method
;; Takes a XSLFSlideMaster and optional type (int) and returns a XSLFSlideLayout
(defn get-layout ^XSLFSlideLayout [^XSLFSlideMaster slide-master & type]
  (let [type (if type (first type) SlideLayout/TITLE_AND_CONTENT)]
    (.getLayout slide-master ^SlideLayout type)))

(defn ppt-width ^double [^XMLSlideShow ppt]
  (-> ppt .getPageSize .getWidth))

(defn ppt-height ^double [^XMLSlideShow ppt]
  (-> ppt .getPageSize .getHeight))

(defn make-ppt
  "Make a slideshow PPT and directory."
  [filename & {:keys [w h bg-color template]
               :or {w 960
                    h 540}}]
  (let [^XMLSlideShow ppt (if template (XMLSlideShow. (io/input-stream template)) (XMLSlideShow.))
        slide-master (when template (first (get-slide-masters ppt)))
        layout (when template (get-layout slide-master SlideLayout/TITLE_AND_CONTENT))
        ^XSLFSlide slide (if template (.createSlide ppt layout) (.createSlide ppt))]
    (when-not template (.setPageSize ppt (Dimension. w h)))
    {:ppt ppt
     :slide (if (and (vector? (:colors bg-color)) (every? string? (:colors bg-color)))
              (gradient-x-slide slide :colors (:colors bg-color) :angle (or (:angle bg-color) (:angel bg-color)))
              (doto slide (-> .getBackground (.setFillColor (make-color (:colors bg-color))))))
     :filename filename}))

(defn next-slide
  "Make a new slide."
  [slideshow]
  (assoc slideshow
         :slide ^XSLFSlide (.createSlide ^XMLSlideShow (:ppt slideshow))))

(defn save-ppt
  "Write the slideshow's ppt."
  [slideshow]
  (.write ^XMLSlideShow (:ppt slideshow)
          (FileOutputStream. ^String (:filename slideshow)))
  slideshow)

(defn load-ppt [filename]
  (ZipSecureFile/setMinInflateRatio 0)
  (ZipSecureFile/setMaxFileCount 10000)
  (let [^XMLSlideShow ppt (XMLSlideShow. (io/input-stream filename))]
    {:ppt ppt
     :filename filename}))

