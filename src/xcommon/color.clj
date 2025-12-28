(ns xcommon.color
  (:require [xcommon.macro :as macro])
  (:import [java.awt Color]))

(def colors {})

(def gradient-colors {})
(def gradient-theme-colors {})

(def theme-colors [])

(defn get-theme-colors
  ([] (:colors (rand-nth theme-colors)))
  ([theme-name]
   (->> (filter #(= (keyword (:name %)) (keyword theme-name)) theme-colors)
        first
        :colors)))

(defn non-repeat-rand [coll n]
  (if (<= (count coll) n)
    coll
    (take n (shuffle coll))))

(defn get-gradient-colors
  ([] (get-gradient-colors 15))
  ([n]
   (vec (non-repeat-rand (rand-nth (vals (group-by count (vals gradient-colors)))) n)))
  ([n theme-colors]
   (->> (map (fn [{name :name colors :colors}] [(keyword name) (mapv (fn [color] ["#ffffffff" color]) colors)]) theme-colors)
        (into {})
        (merge gradient-theme-colors) ;合并渐变主题色集合
        vals
        rand-nth
        (take n)
        vec)
   #_(->> (vals gradient-theme-colors)
          rand-nth
          (take n)
          vec))) ;考虑二色与三色渐变

;; (get-gradient-colors 12 theme-colors)

(defn get-gradient-color
  ([]
   (rand-nth (vals gradient-colors)))
  ([theme-name]
   (get gradient-colors theme-name)))

;; (get-theme-colors )

(def favor-colors [[60 123 199]
                   [35 115 243]
                   [151 189 70]
                   [53 173 206]
                   [60 123 199]
                   [123 88 126]
                   [228 108 10]
                   [0 162 233] ;beauty blue
                   [254 134 55] ;pa orange
                   [255 105 1] ;aliyun
                   :red
                   :green500
                   :black
                   ;; (Color. 2 172 191) ;深青--引起注意
                   ;; (Color. 102 58 118) ;深紫--诱发兴趣
                   ;; (Color. 254 81 81) ;红色--刺激欲望
                   ;; (Color. 255 183 80) ;橙色--促成购买
                   ;; (Color. 38 134 167) ;青绿
                   ;; (Color. 84 190 113) ;绿色
                   ;; (Color. 139 194 72) ;酸橙色
                   ;; (Color. 239 149 39) ;橙色
                   ;; (Color. 237 66 61)  ;红色
                   ;; (Color. 32 47 62)
                   ])

(defn get-one-color []
  (rand-nth favor-colors))

(defn get-favor-colors
  ([] (get-favor-colors 8))
  ([n]
   (vec (non-repeat-rand favor-colors n))))

(defn rand-colors
  ([cnt]
   (repeatedly cnt #(rand-nth (vals colors))))
  ([cnt favor-colors]
   (repeatedly cnt #(rand-nth favor-colors))))

(def chosen-colors (atom #{}))

(defn get-random-color []
  (let [remaining-colors (vec (remove #(contains? @chosen-colors %) favor-colors))
        rand-color (if (empty? remaining-colors) (first (rand-colors 1)) (rand-nth remaining-colors))]
    (swap! chosen-colors conj rand-color)
    rand-color))

(defn colour
  "an unchecked-int or r g b should be between 0 and 255"
  ([uint] (unchecked-int uint))
  ([r g b]
   (unchecked-int
    (bit-or
     (unchecked-int 0xFF000000)
     (bit-shift-left r 16)
     (bit-shift-left g 8)
     (bit-shift-left b 0)))))

(defn colour-noise
  "turns noise into an r, g, or b value for use with colour"
  [noise]
  (if noise (int (quot (* (+ noise 1) 255) 2)) 0))

(defn- int->hex
  "Convert integer to hex string"
  [v]
  (Integer/toHexString v))

(defn hex-to-rgb
  "将十六进制颜色字符串转换为RGB值向量，或处理颜色名称字符串，或处理RGB格式字符串"
  [hex]
  (cond
    ;; 处理RGB格式字符串，如 "rgb(249,250,251)"
    (and (string? hex) (.startsWith hex "rgb("))
    (let [rgb-str (-> hex
                      (clojure.string/replace "rgb(" "")
                      (clojure.string/replace ")" ""))
          [r g b] (map #(Integer/parseInt (clojure.string/trim %))
                       (clojure.string/split rgb-str #","))]
      [r g b])

    ;; 处理非十六进制字符串
    (and (string? hex) (not (.startsWith hex "#")))
    ;; 检查是否是十六进制字符串（6位数字和字母）
    (if (and (= 6 (count hex)) (re-matches #"[0-9a-fA-F]{6}" hex))
      ;; 是十六进制字符串，直接解析
      [(Integer/parseInt (subs hex 0 2) 16) ; 红色
       (Integer/parseInt (subs hex 2 4) 16) ; 绿色
       (Integer/parseInt (subs hex 4 6) 16)] ; 蓝色
      ;; 否则尝试作为颜色名称从colors映射中获取
      (if-let [color-obj (get colors (keyword hex))]
        [(.getRed color-obj) (.getGreen color-obj) (.getBlue color-obj)]
        (throw (IllegalArgumentException. (str "Unknown color name: " hex)))))

    ;; 处理十六进制格式
    :else
    (let [cnt (count hex)
          hex-val (if (= 9 cnt) (subs hex 3) (subs hex 1)) ; 移除#或透明度前缀
          len (count hex-val)]
      (if (= len 3)
        (let [r (subs hex-val 0 1)
              g (subs hex-val 1 2)
              b (subs hex-val 2 3)]
          [(Integer/parseInt (str r r) 16)
           (Integer/parseInt (str g g) 16)
           (Integer/parseInt (str b b) 16)])
        [(Integer/parseInt (subs hex-val 0 2) 16) ; 红色
         (Integer/parseInt (subs hex-val 2 4) 16) ; 绿色
         (Integer/parseInt (subs hex-val 4 6) 16)])))) ; 蓝色 ; 蓝色 ; 蓝色

(defn rgb->hex
  [[r g b]]
  (let [r (int r)
        g (int g)
        b (int b)]
    (if (or (not= r (bit-and r 255))
            (not= g (bit-and g 255))
            (not= b (bit-and b 255)))
      (throw (ex-info "not valid rgb" {:r r :g g :b b}))
      (let [rgb (bit-or (bit-shift-left r 16)
                        (bit-shift-left g 8) b)]
        (if (< r 16)
          (macro/str "0xFF" (subs (int->hex (bit-or 0x1000000 rgb)) 1))
          (macro/str "0xFF" (int->hex rgb)))))))

(defn rgb-to-hex [[r g b]]
  (let [hex-value (bit-or (bit-shift-left r 16) (bit-or (bit-shift-left g 8) b))]
    (str "0x" (format "%08X" hex-value))))

(defn make-color [arg]
  (cond
    (nil? arg) nil
    (instance? Color arg) arg
    (string? arg) (let [[r g b] (hex-to-rgb arg)]
                    (Color. ^int r ^int g ^int b))
    (keyword? arg) (or (colors arg)
                       (case arg
                         :black Color/BLACK
                         :white Color/WHITE
                         :red Color/RED
                         :blue Color/BLUE
                         :green Color/GREEN
                         :yellow Color/YELLOW
                         :cyan Color/CYAN
                         :magenta Color/MAGENTA
                         :gray Color/GRAY
                         :darkGray Color/DARK_GRAY
                         :lightGray Color/LIGHT_GRAY
                         :orange Color/ORANGE
                         :pink Color/PINK
                         nil))
    (and (vector? arg) (every? int? arg)) (Color. ^int (first arg) ^int (second arg) ^int (last arg))))

(defn color->rgb [color]
  (let [color (or (make-color color) Color/white)]
    [(.getRed ^Color color) (.getGreen ^Color color) (.getBlue ^Color color)]))

(defn color->hex [k]
  (-> (make-color k)
      (color->rgb)
      rgb->hex
      ;; rgb-to-hex
      ))

(defn brightness [color]
  (let [[r g b] (color->rgb (make-color color))]
    (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))

(defn wise-bright [color]
  (cond
    (nil? color) 0
    (string? color) (brightness color)
    (keyword? color) (brightness color)
    (and (vector? color) (= (count color) 3) (every? int? color)) (brightness color)
    (and (vector? color) (> (count color) 3) (or (every? keyword? color) (every? string? color))) (/ (->> (map brightness color) (reduce +)) (count color))
    (and (vector? color) (not (keyword? (first color))) (every? int? (first color)) (= 3 (count (first color))) (every? vector? color)) (/ (->> (map brightness color) (reduce +)) (count color))
    (and (vector? color) (< 1 (count color) 4) (or (every? string? color) (every? keyword? color))) (/ (->> (vec (flatten color)) (map brightness) (reduce +)) (count color))
    (and (vector? color) (> (count color) 1) (every? vector? color) (or (keyword? (first (first color))) (string? (first (first color))))) (/ (->> (vec (flatten color)) (map brightness) (reduce +)) (count (flatten color)))
    :else (brightness color)))

(defn wise-fill-colors [& {:keys [type color colors name]
                           :or {type :pure}}]
  (case type
    :pure (or color (get-one-color))
    :gradient (or (get-gradient-color name) color (get-gradient-color))
    :theme (let [theme-colors (or colors (get-theme-colors (or name :x-rainbow)))]
             theme-colors)
    :rand (let [rand-theme-colors (get-theme-colors)]
            rand-theme-colors)
    :dream (let [dream-colors (or colors (rand-nth [(get-gradient-colors) (get-gradient-colors 10 theme-colors)]))]
             dream-colors)
    :purple600))

(defn wise-fill-color [colors idx]
  (if (or (and (vector? colors) (vector? (first colors))) (and (vector? colors) (every? string? colors) (> (count colors) 3)))
    (get colors (mod idx (count colors)))
    colors))

(defn wise-font-color
  ([xcolors]
   (if (> (wise-bright xcolors) 160) :black :white))
  ([xcolors x-color y-color]
   (if (> (wise-bright xcolors) 160) x-color y-color)))



