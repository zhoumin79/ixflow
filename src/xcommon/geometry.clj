(ns xcommon.geometry
  (:require [clojure.string :as str]
            [xcommon.svg-path :as svg-path]))

(defn tag-shape-points
  "Calculates the vertices of a tag shape (rectangle with a triangular tip).
   
   Args:
   - x, y: Origin coordinates.
   - w, h: Total width and height.
   - tip-sz: Size of the triangular tip.
   - dir: Direction of the tip (:left or :right).
   
   Returns:
   - Vector of [x y] points defining the polygon."
  [x y w h tip-sz dir]
  (let [rect-w (- w tip-sz)]
    (if (= dir :right)
      ;; Pointing Right: Tip at right end
      ;; Origin (x,y) is Top-Left of the rectangle part
      [[x y] ;; Top-Left
       [(+ x rect-w) y] ;; Top-Right (start of tip)
       [(+ x w) (+ y (/ h 2))] ;; Tip
       [(+ x rect-w) (+ y h)] ;; Bottom-Right (end of tip)
       [x (+ y h)]] ;; Bottom-Left

      ;; Pointing Left: Tip at left end
      ;; Origin (x,y) is the Tip's x coordinate
      ;; Note: The caller usually passes the bounding box x.
      ;; If x is the bounding box left:
      [[(+ x w) y] ;; Top-Right
       [(+ x tip-sz) y] ;; Top-Left (start of tip)
       [x (+ y (/ h 2))] ;; Tip
       [(+ x tip-sz) (+ y h)] ;; Bottom-Left (end of tip)
       [(+ x w) (+ y h)]]))) ;; Bottom-Right

(defn snake-axis-path
  "Calculates the path definition vector for a Snake (S-shaped) axis."
  [x1 x2 y-top y-bot radius]
  (let [arc-cmds (svg-path/svg-arc->bezier-curves x1 y-bot radius radius 0 1 0 x2 y-bot)]
    (vec (concat [[:move [x1 y-top]]
                  [:line [x1 y-bot]]]
                 arc-cmds
                 [[:line [x2 y-top]]]))))

(defn arrow-head-path
  "Calculates the path for an arrow head at the end of a line."
  [x y sz dir]
  [[:move [(- x sz) (+ y sz)]]
   [:line [x y]]
   [:line [(+ x sz) (+ y sz)]]
   [:close]])

(defn bezier-curve-path
  "Generates path definition for a cubic Bezier curve."
  [p0 p1 p2 p3]
  [[:move [(:x p0) (:y p0)]]
   [:cubic [(:x p1) (:y p1) (:x p2) (:y p2) (:x p3) (:y p3)]]])

(defn rotated-arrow-head-path
  "Generates path for an arrow head extending from (x,y) in direction angle-deg."
  [x y len width angle-deg]
  (let [rad (Math/toRadians angle-deg)
        cos-a (Math/cos rad)
        sin-a (Math/sin rad)
        p1-x (+ x (* 0.5 width sin-a))
        p1-y (- y (* 0.5 width cos-a))
        p2-x (+ x (* len cos-a))
        p2-y (+ y (* len sin-a))
        p3-x (- x (* 0.5 width sin-a))
        p3-y (+ y (* 0.5 width cos-a))]
    [[:move [p1-x p1-y]]
     [:line [p2-x p2-y]]
     [:line [p3-x p3-y]]
     [:close]]))

(defn badge-semi-ellipse-path
  "Generates path for a left-side semi-ellipse badge."
  [x y w h]
  (let [radius (/ h 2)
        start-x (+ x radius)
        start-y y
        end-x (+ x radius)
        end-y (+ y h)
        arc-cmds (svg-path/svg-arc->bezier-curves start-x start-y radius radius 0 0 0 end-x end-y)]
    (vec (concat [[:move [start-x start-y]]]
                 arc-cmds
                 [[:line [(+ x w) (+ y h)]]
                  [:line [(+ x w) y]]
                  [:line [start-x y]]
                  [:close]]))))

(defn badge-circle-path
  "Generates path for a circular badge."
  [cx cy r]
  (let [start-x cx
        start-y (- cy r)
        mid-x cx
        mid-y (+ cy r)
        arc1 (svg-path/svg-arc->bezier-curves start-x start-y r r 0 1 1 mid-x mid-y)
        arc2 (svg-path/svg-arc->bezier-curves mid-x mid-y r r 0 1 1 start-x start-y)]
    (vec (concat [[:move [start-x start-y]]]
                 arc1
                 arc2
                 [[:close]]))))

(defn badge-pill-path
  "Generates path for a rounded rectangle/pill badge."
  [x y w h]
  (let [r (/ h 2)
        start-x (+ x r)
        start-y y
        end-line-x (- (+ x w) r)
        arc1-start-x end-line-x
        arc1-start-y y
        arc1-end-x end-line-x
        arc1-end-y (+ y h)
        arc1 (svg-path/svg-arc->bezier-curves arc1-start-x arc1-start-y r r 0 0 1 arc1-end-x arc1-end-y)

        bot-line-end-x (+ x r)
        arc2-start-x bot-line-end-x
        arc2-start-y (+ y h)
        arc2-end-x (+ x r)
        arc2-end-y y
        arc2 (svg-path/svg-arc->bezier-curves arc2-start-x arc2-start-y r r 0 0 1 arc2-end-x arc2-end-y)]
    (vec (concat [[:move [start-x start-y]]
                  [:line [end-line-x y]]]
                 arc1
                 [[:line [bot-line-end-x (+ y h)]]]
                 arc2
                 [[:close]]))))

(defn ring-segment-path
  [cx cy r-inner r-outer theta1 theta2]
  (let [cx (double cx)
        cy (double cy)
        r-inner (double r-inner)
        r-outer (double r-outer)
        theta1 (double theta1)
        theta2 (double theta2)]
    (letfn [(arc-curves [r a1 a2]
              (let [delta (- (double a2) (double a1))
                    segments (max 1 (int (Math/ceil (/ (Math/abs delta) (/ Math/PI 2.0)))))
                    seg-angle (/ delta segments)]
                (vec
                 (mapcat
                  (fn [i]
                    (let [t1 (+ (double a1) (* i seg-angle))
                          t2 (+ t1 seg-angle)
                          k (/ (* 4.0 (Math/tan (/ (- t2 t1) 4.0))) 3.0)
                          x1 (+ cx (* r (Math/cos t1)))
                          y1 (+ cy (* r (Math/sin t1)))
                          x4 (+ cx (* r (Math/cos t2)))
                          y4 (+ cy (* r (Math/sin t2)))
                          dx1 (* r (- (Math/sin t1)))
                          dy1 (* r (Math/cos t1))
                          dx2 (* r (- (Math/sin t2)))
                          dy2 (* r (Math/cos t2))
                          x2 (+ x1 (* k dx1))
                          y2 (+ y1 (* k dy1))
                          x3 (- x4 (* k dx2))
                          y3 (- y4 (* k dy2))]
                      [[:cubic [x2 y2 x3 y3 x4 y4]]]))
                  (range segments)))))]
      (let [x1o (+ cx (* r-outer (Math/cos theta1)))
            y1o (+ cy (* r-outer (Math/sin theta1)))
            x2i (+ cx (* r-inner (Math/cos theta2)))
            y2i (+ cy (* r-inner (Math/sin theta2)))]
        (vec
         (concat
          [[:move [x1o y1o]]]
          (arc-curves r-outer theta1 theta2)
          [[:line [x2i y2i]]]
          (arc-curves r-inner theta2 theta1)
          [[:close]]))))))

(defn badge-rounded-rect-path
  "Generates path for a rounded rectangle using explicit cubic bezier curves."
  [x y w h r]
  (let [k 0.5522847498
        kr (* k r)

        ;; Coordinates
        left x
        right (+ x w)
        top y
        bottom (+ y h)

        ;; Corner points (start/end of arcs)
        ;; Top-Left
        tl-start-x left
        tl-start-y (+ top r)
        tl-end-x (+ left r)
        tl-end-y top

        ;; Top-Right
        tr-start-x (- right r)
        tr-start-y top
        tr-end-x right
        tr-end-y (+ top r)

        ;; Bottom-Right
        br-start-x right
        br-start-y (- bottom r)
        br-end-x (- right r)
        br-end-y bottom

        ;; Bottom-Left
        bl-start-x (+ left r)
        bl-start-y bottom
        bl-end-x left
        bl-end-y (- bottom r)]

    [[:move [tl-end-x tl-end-y]]

     ;; Top Edge
     [:line [tr-start-x tr-start-y]]

     ;; Top-Right Arc
     [:cubic [(+ tr-start-x kr) tr-start-y
              tr-end-x (- tr-end-y kr)
              tr-end-x tr-end-y]]

     ;; Right Edge
     [:line [br-start-x br-start-y]]

     ;; Bottom-Right Arc
     [:cubic [br-start-x (+ br-start-y kr)
              (+ br-end-x kr) br-end-y
              br-end-x br-end-y]]

     ;; Bottom Edge
     [:line [bl-start-x bl-start-y]]

     ;; Bottom-Left Arc
     [:cubic [(- bl-start-x kr) bl-start-y
              bl-end-x (+ bl-end-y kr)
              bl-end-x bl-end-y]]

     ;; Left Edge
     [:line [tl-start-x tl-start-y]]

     ;; Top-Left Arc
     [:cubic [tl-start-x (- tl-start-y kr)
              (- tl-end-x kr) tl-end-y
              tl-end-x tl-end-y]]

     [:close]]))

(defn wave-path
  "Generates a wave path for the bottom decoration."
  [width height y-start-left y-control y-end-right]
  [[:move [0 height]]
   [:line [0 y-start-left]]
   [:quad [(/ width 2) y-control width y-end-right]]
   [:line [width height]]
   [:close]])

(defn badge-flag-right-path
  "Generates path for a flag-like badge pointing right."
  [x y w h]
  (let [x (double x) y (double y) w (double w) h (double h)
        r (/ h 2.0)
        w-straight (- w r)

        start-x x start-y y
        top-line-end-x (+ x w-straight) top-line-end-y y

        arc (svg-path/svg-arc->bezier-curves top-line-end-x top-line-end-y r r 0 0 1 top-line-end-x (+ y h))]

    (vec (concat [[:move [start-x start-y]]
                  [:line [top-line-end-x top-line-end-y]]]
                 arc
                 [[:line [x (+ y h)]]
                  [:close]]))))

(defn badge-flag-left-path
  "Generates path for a flag-like badge (rounded left side)."
  [x y w h]
  (let [r (/ h 2)
        start-x (+ x w) start-y y
        top-line-end-x (+ x r) top-line-end-y y

        arc (svg-path/svg-arc->bezier-curves top-line-end-x top-line-end-y r r 0 0 0 top-line-end-x (+ y h))]

    (vec (concat [[:move [start-x start-y]]
                  [:line [top-line-end-x top-line-end-y]]]
                 arc
                 [[:line [(+ x w) (+ y h)]]
                  [:close]]))))

(defn arrow-straight-path
  "Generates a horizontal straight arrow axis."
  [x1 y1 x2 y2 height]
  (let [arrow-head-w 30
        half-h (/ height 2)
        y-top (- y1 half-h)
        y-bottom (+ y1 half-h)
        x-end (- x2 arrow-head-w)]
    [[:move [x1 y-top]]
     [:line [x-end y-top]]
     [:line [x2 y1]]
     [:line [x-end y-bottom]]
     [:line [x1 y-bottom]]
     [:close]]))

(defn connector-line-path
  "Generates a simple line path."
  [x1 y1 x2 y2]
  [[:move [(double x1) (double y1)]]
   [:line [(double x2) (double y2)]]])

(defn polyline-path [points & {:keys [radius] :or {radius 0}}]
  (let [extract-pt (fn [p]
                     (cond
                       (vector? p) [(first p) (second p)]
                       (map? p) [(or (:x p) (:cx p)) (or (:y p) (:cy p))]
                       :else [0 0]))
        pts (mapv extract-pt points)
        len (count pts)]
    (if (< len 2)
      ""
      (let [sb (StringBuilder.)
            [x0 y0] (first pts)]
        (.append sb (str "M " x0 " " y0))

        (if (or (zero? radius) (< len 3))
          ;; Straight lines
          (doseq [[x y] (rest pts)]
            (.append sb (str " L " x " " y)))

          ;; Rounded corners
          (let [;; Vector helpers
                sub (fn [[x1 y1] [x2 y2]] [(- x1 x2) (- y1 y2)])
                add (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
                mag (fn [[x y]] (Math/sqrt (+ (* x x) (* y y))))
                scale (fn [[x y] s] [(* x s) (* y s)])
                normalize (fn [v] (let [m (mag v)] (if (zero? m) [0 0] (scale v (/ 1.0 m)))))]

            (loop [i 1]
              (if (< i (dec len))
                (let [prev (nth pts (dec i))
                      curr (nth pts i)
                      next (nth pts (inc i))

                      ;; Vectors
                      v1 (sub curr prev)
                      v2 (sub next curr)

                      ;; Lengths
                      l1 (mag v1)
                      l2 (mag v2)

                      ;; Radius can't exceed half the segment length
                      r (min radius (/ l1 2) (/ l2 2))

                      ;; Start of curve (on v1, r away from curr)
                      p-start (sub curr (scale (normalize v1) r))

                      ;; End of curve (on v2, r away from curr)
                      p-end (add curr (scale (normalize v2) r))]

                  ;; Line to start of curve
                  (.append sb (str " L " (first p-start) " " (second p-start)))

                  ;; Quadratic bezier to end of curve, control point is curr
                  (.append sb (str " Q " (first curr) " " (second curr)
                                   " " (first p-end) " " (second p-end)))

                  (recur (inc i)))

                ;; Line to last point
                (let [[xl yl] (last pts)]
                  (.append sb (str " L " xl " " yl))))))) \n (.toString sb)))))

(defn polygon-path
  "Generates a path definition for a polygon from a sequence of points."
  [points]
  (vec (cons [:move (first points)]
             (concat (map (fn [p] [:line p]) (rest points))
                     [[:close]]))))

(defn basis-spline-path
  "Generates a B-spline path string from a sequence of points.
   Uses a pure functional approach without atoms."
  [points]
  (if (< (count points) 3)
    ""
    (let [pts (vec (concat [(first points)] points [(last points)]))
          n (count pts)
          start (nth pts 1)
          initial-cmd (str "M" (:x start) "," (:y start))
          segments (map (fn [i]
                          (let [p0 (nth pts i)
                                p1 (nth pts (inc i))
                                p2 (nth pts (+ i 2))
                                p3 (nth pts (+ i 3))
                                x1 (+ (* (/ 1.0 6.0) (:x p0)) (* (/ 2.0 3.0) (:x p1)) (* (/ 1.0 6.0) (:x p2)))
                                y1 (+ (* (/ 1.0 6.0) (:y p0)) (* (/ 2.0 3.0) (:y p1)) (* (/ 1.0 6.0) (:y p2)))
                                x2 (+ (* (/ 1.0 6.0) (:x p1)) (* (/ 2.0 3.0) (:x p2)) (* (/ 1.0 6.0) (:x p3)))
                                y2 (+ (* (/ 1.0 6.0) (:y p1)) (* (/ 2.0 3.0) (:y p2)) (* (/ 1.0 6.0) (:y p3)))
                                cp1-x (+ (* (/ 2.0 3.0) (:x p1)) (* (/ 1.0 3.0) (:x p2)))
                                cp1-y (+ (* (/ 2.0 3.0) (:y p1)) (* (/ 1.0 3.0) (:y p2)))
                                cp2-x (+ (* (/ 1.0 3.0) (:x p1)) (* (/ 2.0 3.0) (:x p2)))
                                cp2-y (+ (* (/ 1.0 3.0) (:y p1)) (* (/ 2.0 3.0) (:y p2)))]
                            (cond-> ""
                              (zero? i) (str "L" x1 "," y1 " ")
                              :always (str "C" cp1-x "," cp1-y " " cp2-x "," cp2-y " " x2 "," y2))))
                        (range (- n 3)))
          end (last points)
          final-cmd (str "L" (:x end) "," (:y end))]
      (str/join " " (concat [initial-cmd] segments [final-cmd])))))

(defn smooth-curve-path
  "Generates a smooth curve path string (S-curve or B-spline) depending on point count."
  [points]
  (let [pts (vec points)
        n (count pts)]
    (cond
      (< n 2) ""
      (= n 2)
      (let [p0 (nth pts 0)
            p1 (nth pts 1)
            dx (- (:x p1) (:x p0))
            dy (- (:y p1) (:y p0))
            vertical? (>= (Math/abs (double dy)) (Math/abs (double dx)))
            pull (double (min 80.0 (* 0.4 (if vertical? (Math/abs (double dy)) (Math/abs (double dx))))))
            sgn (fn [v] (if (neg? v) -1.0 1.0))
            [cp1-x cp1-y cp2-x cp2-y]
            (if vertical?
              [(:x p0) (+ (:y p0) (* pull (sgn dy)))
               (:x p1) (- (:y p1) (* pull (sgn dy)))]
              [(+ (:x p0) (* pull (sgn dx))) (:y p0)
               (- (:x p1) (* pull (sgn dx))) (:y p1)])]
        (str "M" (:x p0) "," (:y p0) " "
             "C" cp1-x "," cp1-y " " cp2-x "," cp2-y " " (:x p1) "," (:y p1)))
      :else
      (let [is-ortho? (every? (fn [i]
                                (let [p1 (nth pts i)
                                      p2 (nth pts (inc i))]
                                  (or (= (:x p1) (:x p2)) (= (:y p1) (:y p2)))))
                              (range (dec n)))]
        (if is-ortho?
          (polyline-path points :radius 20)
          (basis-spline-path points))))))
