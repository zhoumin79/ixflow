(ns xcommon.svg-path
  "SVG 路径字符串解析器，将 SVG 路径转换为 path.clj 兼容的路径向量格式"
  (:require [clojure.string :as str]))

(defn- parse-number
  "解析数字，支持整数和浮点数"
  [s]
  (when (and s (not (str/blank? s)))
    (try
      (if (str/includes? s ".")
        (Double/parseDouble s)
        (Long/parseLong s))
      (catch NumberFormatException _
        nil))))

(defn- tokenize-path
  "将 SVG 路径字符串分解为命令和参数的标记列表"
  [path-str]
  (when path-str
    (->> path-str
         (re-seq #"[MmLlHhVvAaCcQqZz]|[-+]?(?:\d*\.?\d+)")
         (map str/trim)
         (remove str/blank?))))

(defn- parse-coordinates
  "解析坐标参数列表"
  [args]
  (map parse-number args))

(defn- to-rad [deg] (Math/toRadians deg))

(defn- vector-angle [u v]
  (let [[ux uy] u
        [vx vy] v
        dot (+ (* ux vx) (* uy vy))
        len-u (Math/sqrt (+ (* ux ux) (* uy uy)))
        len-v (Math/sqrt (+ (* vx vx) (* vy vy)))
        denom (* len-u len-v)
        cos-theta (if (< denom 1.0E-12) 1.0 (max -1.0 (min 1.0 (/ dot denom))))
        theta (Math/acos cos-theta)
        sign (if (< (- (* ux vy) (* uy vx)) 0) -1.0 1.0)]
    (* sign theta)))

(defn svg-arc->bezier-curves
  [x0 y0 rx ry rot large-arc-flag sweep-flag x y]
  (let [eps 1.0E-6
        rx (Math/abs (double rx))
        ry (Math/abs (double ry))
        rx (if (< rx eps) eps rx)
        ry (if (< ry eps) eps ry)
        rot-rad (to-rad rot)
        cos-phi (Math/cos rot-rad)
        sin-phi (Math/sin rot-rad)
        dx2 (/ (- x0 x) 2.0)
        dy2 (/ (- y0 y) 2.0)
        x1p (+ (* cos-phi dx2) (* sin-phi dy2))
        y1p (+ (* (- sin-phi) dx2) (* cos-phi dy2))

        rx-sq (* rx rx)
        ry-sq (* ry ry)
        x1p-sq (* x1p x1p)
        y1p-sq (* y1p y1p)

        radii-check (+ (/ x1p-sq rx-sq) (/ y1p-sq ry-sq))
        [rx ry rx-sq ry-sq] (if (> radii-check 1.0)
                              (let [s (Math/sqrt radii-check)]
                                [(* rx s) (* ry s) (* rx-sq s s) (* ry-sq s s)])
                              [rx ry rx-sq ry-sq])

        sign (if (= large-arc-flag sweep-flag) -1.0 1.0)
        num (- (* rx-sq ry-sq) (* rx-sq y1p-sq) (* ry-sq x1p-sq))
        den (+ (* rx-sq y1p-sq) (* ry-sq x1p-sq))
        coef (if (< den 0.000001) 0.0 (* sign (Math/sqrt (max 0.0 (/ num den)))))

        cxp (* coef (/ (* rx y1p) ry))
        cyp (* coef (/ (* (- ry) x1p) rx))

        cx (+ (* cos-phi cxp) (* (- sin-phi) cyp) (/ (+ x0 x) 2.0))
        cy (+ (* sin-phi cxp) (* cos-phi cyp) (/ (+ y0 y) 2.0))

        ux (/ (- x1p cxp) rx)
        uy (/ (- y1p cyp) ry)
        vx (/ (- (- x1p) cxp) rx)
        vy (/ (- (- y1p) cyp) ry)

        start-angle (vector-angle [1.0 0.0] [ux uy])
        d-angle (vector-angle [ux uy] [vx vy])

        d-angle (cond
                  (and (= sweep-flag 0) (> d-angle 0)) (- d-angle (* 2 Math/PI))
                  (and (= sweep-flag 1) (< d-angle 0)) (+ d-angle (* 2 Math/PI))
                  :else d-angle)

        segments (max 1 (int (Math/ceil (/ (Math/abs d-angle) (/ Math/PI 2.0)))))
        segment-angle (if (< (Math/abs d-angle) eps) 0.0 (/ d-angle segments))
        k (if (< (Math/abs segment-angle) eps) 0.0 (/ (* 4.0 (Math/tan (/ segment-angle 4.0))) 3.0))]

    (loop [i 0
           commands []
           curr-angle start-angle]
      (if (or (>= i segments) (< (Math/abs segment-angle) eps))
        commands
        (let [next-angle (+ curr-angle segment-angle)

              eta1 curr-angle
              eta2 next-angle

              cp1x (+ (* rx (- (Math/cos eta1) (* k (Math/sin eta1)))) cxp)
              cp1y (+ (* ry (+ (Math/sin eta1) (* k (Math/cos eta1)))) cyp)

              cp2x (+ (* rx (+ (Math/cos eta2) (* k (Math/sin eta2)))) cxp)
              cp2y (+ (* ry (- (Math/sin eta2) (* k (Math/cos eta2)))) cyp)

              p2x (+ (* rx (Math/cos eta2)) cxp)
              p2y (+ (* ry (Math/sin eta2)) cyp)

              transform (fn [px py]
                          [(+ (* cos-phi px) (* (- sin-phi) py) cx)
                           (+ (* sin-phi px) (* cos-phi py) cy)])

              [tcp1x tcp1y] (transform cp1x cp1y)
              [tcp2x tcp2y] (transform cp2x cp2y)
              [tp2x tp2y] (transform p2x p2y)]

          (recur (inc i)
                 (conj commands [:cubic [tcp1x tcp1y tcp2x tcp2y tp2x tp2y]])
                 next-angle))))))

(defn- svg-command->path-def
  "将单个 SVG 命令转换为路径定义向量"
  [command args current-pos]
  (let [cmd (str/upper-case command)
        is-relative (not= command cmd)
        coords (parse-coordinates args)
        [curr-x curr-y] current-pos]

    (case cmd
      "M" (let [[x y] coords
                abs-x (if is-relative (+ curr-x x) x)
                abs-y (if is-relative (+ curr-y y) y)]
            {:commands [[:move [abs-x abs-y]]]
             :new-pos [abs-x abs-y]})

      "L" (let [[x y] coords
                abs-x (if is-relative (+ curr-x x) x)
                abs-y (if is-relative (+ curr-y y) y)]
            {:commands [[:line [abs-x abs-y]]]
             :new-pos [abs-x abs-y]})

      "H" (let [x (first coords)
                abs-x (if is-relative (+ curr-x x) x)]
            {:commands [[:line [abs-x curr-y]]]
             :new-pos [abs-x curr-y]})

      "V" (let [y (first coords)
                abs-y (if is-relative (+ curr-y y) y)]
            {:commands [[:line [curr-x abs-y]]]
             :new-pos [curr-x abs-y]})

      "A" (let [[rx ry x-axis-rotation large-arc-flag sweep-flag x y] coords
                abs-x (if is-relative (+ curr-x x) x)
                abs-y (if is-relative (+ curr-y y) y)
                ;; 转换弧形为贝塞尔曲线
                bezier-commands (svg-arc->bezier-curves curr-x curr-y rx ry
                                                        x-axis-rotation large-arc-flag sweep-flag
                                                        abs-x abs-y)]
            {:commands bezier-commands
             :new-pos [abs-x abs-y]})

      "C" (let [[x1 y1 x2 y2 x y] coords
                abs-x1 (if is-relative (+ curr-x x1) x1)
                abs-y1 (if is-relative (+ curr-y y1) y1)
                abs-x2 (if is-relative (+ curr-x x2) x2)
                abs-y2 (if is-relative (+ curr-y y2) y2)
                abs-x (if is-relative (+ curr-x x) x)
                abs-y (if is-relative (+ curr-y y) y)]
            {:commands [[:cubic [abs-x1 abs-y1 abs-x2 abs-y2 abs-x abs-y]]]
             :new-pos [abs-x abs-y]})

      "Q" (let [[x1 y1 x y] coords
                abs-x1 (if is-relative (+ curr-x x1) x1)
                abs-y1 (if is-relative (+ curr-y y1) y1)
                abs-x (if is-relative (+ curr-x x) x)
                abs-y (if is-relative (+ curr-y y) y)]
            {:commands [[:quad [abs-x1 abs-y1 abs-x abs-y]]]
             :new-pos [abs-x abs-y]})

      "Z" {:commands [[:close]]
           :new-pos current-pos}

      ;; 未知命令，忽略
      {:commands []
       :new-pos current-pos})))

(defn- group-command-args
  "将标记列表分组为命令和其参数"
  [tokens]
  (loop [tokens tokens
         groups []]
    (if (empty? tokens)
      groups
      (let [cmd (first tokens)
            remaining (rest tokens)]
        (if (re-matches #"[MmLlHhVvAaCcQqZz]" cmd)
          ;; 这是一个命令
          (let [arg-count (case (str/upper-case cmd)
                            ("M" "L") 2
                            ("H" "V") 1
                            "A" 7
                            "C" 6
                            "Q" 4
                            ("Z") 0
                            0)
                args (take arg-count remaining)
                next-tokens (drop arg-count remaining)]
            (recur next-tokens (conj groups [cmd args])))
          ;; 跳过无效的标记
          (recur remaining groups))))))

(defn svg-path->path-def
  "将 SVG 路径字符串转换为路径定义向量
   
   参数:
   - path-str: SVG 路径字符串，如 'M10 10L20 20Z'
   
   返回:
   - 路径定义向量，如 [[:move 10 10] [:line 20 20] [:close]]"
  [path-str]
  (when (and path-str (string? path-str) (not (str/blank? path-str)))
    (try
      (let [tokens (tokenize-path path-str)
            command-groups (group-command-args tokens)]
        (loop [groups command-groups
               current-pos [0 0]
               path-def []]
          (if (empty? groups)
            path-def
            (let [[cmd args] (first groups)
                  result (svg-command->path-def cmd args current-pos)
                  new-commands (:commands result)
                  new-pos (:new-pos result)]
              (recur (rest groups)
                     new-pos
                     (into path-def new-commands))))))
      (catch Exception e
        ;; 解析失败时返回空路径
        []))))

(defn interpolate-svg-path
  "插值 SVG 路径模板中的占位符，支持复杂数学表达式"
  [template params]
  (letfn [;; 手动解析简单表达式的后备方法
          (manual-parse-expression [expr]
            (cond
              ;; 纯数字
              (re-matches #"\d+(?:\.\d+)?" expr)
              (Double/parseDouble expr)

              ;; a+b*c/d-e 形式 (最复杂的情况)
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c d e] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)]
                (- (+ (Double/parseDouble a) (/ (* (Double/parseDouble b) (Double/parseDouble c)) (Double/parseDouble d))) (Double/parseDouble e)))

              ;; a+b*c/d 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c d] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)]
                (+ (Double/parseDouble a) (/ (* (Double/parseDouble b) (Double/parseDouble c)) (Double/parseDouble d))))

              ;; a+b/c+d 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c d] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)]
                (+ (Double/parseDouble a) (/ (Double/parseDouble b) (Double/parseDouble c)) (Double/parseDouble d)))

              ;; a+b/c-d 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c d] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)]
                (- (+ (Double/parseDouble a) (/ (Double/parseDouble b) (Double/parseDouble c))) (Double/parseDouble d)))

              ;; a+b/c 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)]
                (+ (Double/parseDouble a) (/ (Double/parseDouble b) (Double/parseDouble c))))

              ;; a-b/c 形式
              (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c] (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)]
                (- (Double/parseDouble a) (/ (Double/parseDouble b) (Double/parseDouble c))))

              ;; a*b/c 形式
              (re-find #"^(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c] (re-find #"^(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)]
                (/ (* (Double/parseDouble a) (Double/parseDouble b)) (Double/parseDouble c)))

              ;; a+b+c 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)]
                (+ (Double/parseDouble a) (Double/parseDouble b) (Double/parseDouble c)))

              ;; a-b-c 形式
              (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b c] (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)]
                (- (Double/parseDouble a) (Double/parseDouble b) (Double/parseDouble c)))

              ;; a+b 形式
              (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b] (re-find #"^(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)$" expr)]
                (+ (Double/parseDouble a) (Double/parseDouble b)))

              ;; a-b 形式
              (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b] (re-find #"^(\d+(?:\.\d+)?)-(\d+(?:\.\d+)?)$" expr)]
                (- (Double/parseDouble a) (Double/parseDouble b)))

              ;; a*b 形式
              (re-find #"^(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b] (re-find #"^(\d+(?:\.\d+)?)\*(\d+(?:\.\d+)?)$" expr)]
                (* (Double/parseDouble a) (Double/parseDouble b)))

              ;; a/b 形式
              (re-find #"^(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)
              (let [[_ a b] (re-find #"^(\d+(?:\.\d+)?)/(\d+(?:\.\d+)?)$" expr)]
                (/ (Double/parseDouble a) (Double/parseDouble b)))

              ;; 默认返回原表达式
              :else expr))

          ;; 数学表达式求值器
          (eval-math-expression [expr params]
            ;; 将变量替换为实际值
            (let [var-replaced (reduce-kv
                                (fn [s k v]
                                  (clojure.string/replace s (str (name k)) (str v)))
                                expr
                                params)]
              ;; 使用手动解析
              (manual-parse-expression var-replaced)))]

    (let [pattern #"\{([^}]+)\}"
          replacer (fn [match]
                     (let [expr (second match)
                           ;; 使用表达式求值器
                           result (try
                                    (eval-math-expression expr params)
                                    (catch Exception e
                                      ;; 如果求值失败，尝试作为简单变量查找
                                      (get params (keyword expr) expr)))]
                       (str result)))]
      (clojure.string/replace template pattern replacer))))

(defn path-def->svg-path
  "Converts a path definition vector back to an SVG path string.
   
   Args:
   - path-def: Vector of commands like [[:move [x y]] [:line [x y]] ...]
   
   Returns:
   - SVG path string"
  [path-def]
  (let [fmt-pt (fn [[x y]] (str x " " y))
        fmt-cmd (fn [[cmd args]]
                  (case cmd
                    :move (str "M " (fmt-pt args))
                    :line (str "L " (fmt-pt args))
                    :cubic (let [[x1 y1 x2 y2 x y] args]
                             (str "C " x1 " " y1 ", " x2 " " y2 ", " x " " y))
                    :quad (let [[x1 y1 x y] args]
                            (str "Q " x1 " " y1 ", " x " " y))
                    :close "Z"
                    ""))]
    (str/join " " (map fmt-cmd path-def))))

(defn svg-path-template->path-def
  "将 SVG 路径模板转换为路径定义向量
   
   参数:
   - path-template: SVG 路径模板字符串
   - params: 参数映射
   
   返回:
   - 路径定义向量"
  [path-template params]
  (when path-template
    (let [interpolated-path (interpolate-svg-path path-template params)]
      (svg-path->path-def interpolated-path))))

(defn svg-path-template->path-def
  "将 SVG 路径模板转换为路径定义向量
   
   参数:
   - path-template: SVG 路径模板字符串
   - params: 参数映射
   
   返回:
   - 路径定义向量"
  [path-template params]
  (when path-template
    (let [interpolated-path (interpolate-svg-path path-template params)]
      (svg-path->path-def interpolated-path))))

;; 测试函数
(comment
  ;; 测试基本路径解析
  (svg-path->path-def "M10 10L20 20Z")
  ;; => [[:move 10 10] [:line 20 20] [:close]]

  ;; 测试路径模板插值
  (interpolate-svg-path "M{x} {y}L{x+width} {y+height}Z"
                        {:x 10 :y 20 :width 100 :height 50})
  ;; => "M10 20L110 70Z"

  ;; 测试完整的模板到路径定义转换
  (svg-path-template->path-def "M{x} {y}h{width}v{height}H{x}v-{height}z"
                               {:x 100 :y 50 :width 200 :height 30})
  ;; => [[:move 100 50] [:line 300 50] [:line 300 80] [:line 100 80] [:line 100 50] [:close]]
  )