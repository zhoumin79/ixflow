(ns xflow.layout.routing.ortho
  (:require [xflow.layout.config :as config]))

;; --- Orthogonal Routing Utilities ---

(defn- get-node-bounds [node padding]
  {:x1 (- (:x node) padding)
   :y1 (- (:y node) padding)
   :x2 (+ (:x node) (:w node) padding)
   :y2 (+ (:y node) (:h node) padding)})

(defn- distribute-points [start length fixed-coord count axis]
  "Returns a sequence of {:x ... :y ...} points distributed along a line.
   axis: :x (vary x, fixed y) or :y (vary y, fixed x)"
  (let [step (double (/ length (inc count)))]
    (map (fn [i]
           (let [var-coord (double (+ start (* (inc i) step)))]
             (if (= axis :x)
               {:x var-coord :y (double fixed-coord)}
               {:x (double fixed-coord) :y var-coord})))
         (range count))))

(defn- is-vertical? [direction]
  (let [d (if (keyword? direction) (name direction) (str direction))]
    (or (= d "tb") (= d "vertical") (= d "") (= d "nil"))))

;; --- Interval Coloring / Track Assignment ---

(defn- assign-segment-tracks [segments interval-key-start interval-key-end key-fn]
  (let [sorted (sort-by interval-key-start segments)
        ;; State: {:tracks [end-pos-of-track-0, ...], :mapping {seg-key track-idx}}
        result (reduce
                (fn [state seg]
                  (let [start (get seg interval-key-start)
                        end (get seg interval-key-end)
                        ;; Ensure start <= end and add padding
                        [s e] (if (< start end) [start end] [end start])
                        s (- s 1)
                        e (+ e 1)

                        tracks (:tracks state)
                        ;; Find first track where last-end <= s
                        fit-idx (first (keep-indexed (fn [i last-end]
                                                       (when (>= s last-end) i))
                                                     tracks))
                        use-idx (or fit-idx (count tracks))
                        new-tracks (if fit-idx
                                     (assoc tracks fit-idx e)
                                     (conj tracks e))]
                    {:tracks new-tracks
                     :mapping (assoc (:mapping state) (key-fn seg) use-idx)}))
                {:tracks [] :mapping {}}
                sorted)]
    {:mapping (:mapping result)
     :count (count (:tracks result))}))

(defn- nudge-segments-smart [segments axis gap]
  "Nudges segments perpendicular to axis.
   If axis is :x (vertical segments), we nudge in X.
   Group by X, then assign Y-intervals to tracks."
  (let [coord-key (if (= axis :x) :x1 :y1)
        interval-start (if (= axis :x) :y1 :x1)
        interval-end (if (= axis :x) :y2 :x2)
        groups (group-by coord-key segments)]

    (mapcat (fn [[coord segs]]
              (if (<= (count segs) 1)
                []
                ;; Merge segments of same edge to ensure they get same track
                (let [edge-groups (group-by :edge-id segs)
                      meta-segs (keep (fn [[eid e-segs]]
                                        (let [starts (map interval-start e-segs)
                                              ends (map interval-end e-segs)
                                              valid-vals (concat (remove nil? starts) (remove nil? ends))]
                                          (when (seq valid-vals)
                                            {:edge-id eid
                                             :start (reduce min valid-vals)
                                             :end (reduce max valid-vals)})))
                                      edge-groups)

                      ;; Assign tracks to meta-segments (keyed by edge-id)
                      {:keys [mapping count]} (assign-segment-tracks meta-segs :start :end :edge-id)]

                  (if (> count 1)
                    (map (fn [s]
                           (let [track-idx (get mapping (:edge-id s))
                                 ;; Center tracks: 0, 1, 2 -> -1, 0, 1
                                 offset (if track-idx
                                          (* (- track-idx (/ (dec count) 2.0)) gap)
                                          0)]
                             {:edge-id (:edge-id s)
                              :idx (:idx s)
                              :offset offset}))
                         segs)
                    []))))
            groups)))

;; --- Port Allocation ---

(defn- assign-ports [nodes edges options]
  "Assigns specific start and end points for each edge on their respective nodes.
   Returns a map: {edge-id {:source {x y :side} :target {x y :side}}}"
  (let [direction (:direction options :tb)
        ;; Check swimlane-mode strictly
        swimlane-mode? (let [m (:swimlane-mode options)]
                         (println "DEBUG: assign-ports swimlane-mode=" m)
                         (or (= m "vertical") (= m "horizontal")
                             (= m :vertical) (= m :horizontal)))

        nodes-map (into {} (map (fn [n] [(:id n) n]) nodes))
        out-edges (group-by :from edges)
        in-edges (group-by :to edges)
        vertical? (is-vertical? direction)]
    (reduce
     (fn [acc node]
       (let [node-id (:id node)
             inputs (get in-edges node-id [])
             outputs (get out-edges node-id [])

             ;; Helper to check geometric relationship
             get-rel-type (fn [other-node is-input?]
                            (if (or (nil? (:x other-node)) (nil? (:y other-node))
                                    (nil? (:x node)) (nil? (:y node)))
                              :normal
                              (if vertical?
                                (let [dx (- (:x other-node) (:x node))
                                      abs-dx (Math/abs (double dx))
                                      w (or (:w node) 100)
                                      ;; Check if nodes are in different columns (overlap < 50%)
                                      cross-lane? (> abs-dx (* w 0.5))
                                      ;; Check if adjacent (within 2.5 widths)
                                      adjacent? (< abs-dx (* w 2.5))

                                      dy (- (:y other-node) (:y node))
                                      target-above? (< dy 0)
                                      target-below? (> dy 0)

                                      ;; Check forward distance (vertical gap)
                                      ;; If sufficient vertical gap, prefer Normal (Top/Bottom) even if cross-lane
                                      h (or (:h node) 100)
                                      ;; Use fixed buffer of 20px instead of percentage to be more robust
                                      forward-gap? (> (Math/abs (double dy)) (+ h 20))]
                                  (cond
                                    ;; Back edge Check
                                    ;; If Input: Back if Source is Below (dy > 0)
                                    ;; If Output: Back if Target is Above (dy < 0)
                                    (if is-input? target-below? target-above?) :back

                                    ;; Forward gap preference (Standard Flow)
                                    ;; If we have good forward distance, use normal ports (Bottom/Top)
                                    forward-gap? :normal

                                    ;; Only apply strict cross-lane logic if in Swimlane Mode
                                    (and swimlane-mode? cross-lane? (> (:x other-node) (:x node)))
                                    :right ;; Always Right (Direct or Channel)

                                    (and swimlane-mode? cross-lane? (< (:x other-node) (:x node)))
                                    (if adjacent? :left :right) ;; Non-adjacent uses Right (Channel)

                                    ;; Normal (Downwards/Same column OR non-swimlane general flow)
                                    :else :normal))
                                (let [dy (- (:y other-node) (:y node))
                                      abs-dy (Math/abs (double dy))
                                      h (or (:h node) 100)
                                      ;; Check if nodes are in different rows
                                      cross-lane? (> abs-dy (* h 0.5))
                                      ;; Check if adjacent (within 2.5 heights)
                                      adjacent? (< abs-dy (* h 2.5))

                                      dx (- (:x other-node) (:x node))
                                      target-left? (< dx 0)
                                      target-right? (> dx 0)

                                      ;; Check forward distance (horizontal gap)
                                      w (or (:w node) 100)
                                      ;; Use fixed buffer of 20px instead of percentage
                                      forward-gap? (> (Math/abs (double dx)) (+ w 20))

                                      decision
                                      (cond
                                        ;; Back edge Check
                                        ;; If Input: Back if Source is Right (dx > 0)
                                        ;; If Output: Back if Target is Left (dx < 0)
                                        (if is-input? target-right? target-left?) :back

                                        ;; Forward gap preference (Standard Flow)
                                        ;; If we have good horizontal distance, use normal ports (Right/Left)
                                        forward-gap? :normal

                                        ;; Only apply strict cross-lane logic if in Swimlane Mode
                                        (and swimlane-mode? cross-lane? (> (:y other-node) (:y node)))
                                        :bottom ;; Always Bottom

                                        (and swimlane-mode? cross-lane? (< (:y other-node) (:y node)))
                                        (if adjacent? :top :bottom) ;; Non-adjacent uses Bottom (Channel)

                                        ;; Normal (Rightwards/Same row OR non-swimlane general flow)
                                        :else :normal)]
                                  (when (or (= (:id node) "Approve?") (= (:id other-node) "Approve?"))
                                    (println "DEBUG: Port Decision Node=" (:id node) " Other=" (:id other-node)
                                             " SwimlaneMode=" swimlane-mode? " CrossLane=" cross-lane? " ForwardGap=" forward-gap?
                                             " Decision=" decision " dy=" dy " dx=" dx))
                                  decision))))

             classify-edge (fn [e is-input?]
                             (let [other-id (if is-input? (:from e) (:to e))
                                   other-node (get nodes-map other-id)
                                   rel (if other-node
                                         (get-rel-type other-node is-input?)
                                         :normal)]
                               rel))

             ;; Group inputs/outputs by type
             grouped-in (group-by #(classify-edge % true) inputs)
             grouped-out (group-by #(classify-edge % false) outputs)

             ;; Sort logic (stable sort by coordinate of the OTHER node)
             sort-fn (fn [e is-input?]
                       (let [other-id (if is-input? (:from e) (:to e))
                             other-node (get nodes-map other-id)]
                         (if vertical?
                           (:x other-node 0)
                           (:y other-node 0))))

             ;; Helper to get sorted list for a group
             get-sorted (fn [groups type is-input?]
                          (sort-by #(sort-fn % is-input?) (get groups type [])))

             ;; Determine Port Map based on direction
             ports-map
             (if vertical?
               ;; Vertical (TB)
               {:top (concat (get-sorted grouped-in :normal true)
                             (get-sorted grouped-in :back true))
                :bottom (concat (get-sorted grouped-out :normal false)
                                (get-sorted grouped-out :back false))
                :left (concat (get-sorted grouped-in :left true)
                              (get-sorted grouped-out :left false))
                :right (concat (get-sorted grouped-in :right true)
                               (get-sorted grouped-out :right false))}
               ;; Horizontal (LR)
               {:left (concat (get-sorted grouped-in :normal true)
                              (get-sorted grouped-in :back true))
                :right (concat (get-sorted grouped-out :normal false)
                               (get-sorted grouped-out :back false))
                :top (concat (get-sorted grouped-in :top true)
                             (get-sorted grouped-out :top false))
                :bottom (concat (get-sorted grouped-in :bottom true)
                                (get-sorted grouped-out :bottom false))})

             ;; Distribute points function
             dist (fn [side count]
                    (let [x (:x node) y (:y node) w (:w node) h (:h node)]
                      (case side
                        :top (distribute-points x w y count :x)
                        :bottom (distribute-points x w (+ y h) count :x)
                        :left (distribute-points y h x count :y)
                        :right (distribute-points y h (+ x w) count :y))))

             ;; Generate coordinates
             generated-ports
             (reduce-kv (fn [m side edges]
                          (if (seq edges)
                            (let [pts (dist side (count edges))
                                  tagged (map #(assoc % :side side) pts)]
                              (when (= (:id node) "Approve?")
                                (println "DEBUG: Approve? ports side" side "edges" (map :id edges) "pts " pts))
                              (assoc m side (zipmap (map :id edges) tagged)))
                            m))
                        {}
                        ports-map)]

         ;; Update accumulator with port assignments
         (reduce
          (fn [m [side port-map]]
            (reduce-kv
             (fn [m2 edge-id port]
               (let [is-input? (= (:to (first (filter #(= (:id %) edge-id) (concat inputs outputs)))) node-id)
                     key (if is-input? :target :source)]
                 (assoc-in m2 [edge-id key] port)))
             m
             port-map))
          acc
          generated-ports)))
     {}
     nodes)))

;; --- Path Simplification ---

(defn- collinear? [p1 p2 p3]
  (or (and (= (:x p1) (:x p2)) (= (:x p2) (:x p3)))
      (and (= (:y p1) (:y p2)) (= (:y p2) (:y p3)))))

(defn- simplify-points [points]
  (if (< (count points) 3)
    points
    (reduce
     (fn [acc p]
       (if (< (count acc) 2)
         (conj acc p)
         (let [p2 (peek acc)
               p1 (peek (pop acc))]
           (if (collinear? p1 p2 p)
             (conj (pop acc) p) ;; Replace middle point
             (conj acc p)))))
     [(first points) (second points)]
     (drop 2 points))))

;; --- Main Routing Logic ---

(defn- route-segment-simple [p1 p2 direction]
  (let [vertical? (is-vertical? direction)]
    (cond
      ;; If practically same point or aligned, return straight line
      (or (< (Math/abs (double (- (:x p1) (:x p2)))) 1.0)
          (< (Math/abs (double (- (:y p1) (:y p2)))) 1.0))
      [p1 p2]

      vertical?
      ;; Vertical (TB): Split Y
      (let [mid-y (if (> (:y p2) (:y p1))
                    (double (/ (+ (:y p1) (:y p2)) 2))
                    (double (+ (:y p1) 20)))]
        [p1
         {:x (double (:x p1)) :y mid-y}
         {:x (double (:x p2)) :y mid-y}
         p2])

      :else
      ;; Horizontal (LR): Split X
      (let [mid-x (if (> (:x p2) (:x p1))
                    (double (/ (+ (:x p1) (:x p2)) 2))
                    (double (+ (:x p1) 20)))]
        [p1
         {:x mid-x :y (double (:y p1))}
         {:x mid-x :y (double (:y p2))}
         p2]))))

(defn- get-all-bounds [nodes]
  (reduce
   (fn [acc n]
     (-> acc
         (update :min-x min (:x n))
         (update :max-x max (+ (:x n) (:w n)))
         (update :min-y min (:y n))
         (update :max-y max (+ (:y n) (:h n)))))
   {:min-x Double/MAX_VALUE :max-x Double/MIN_VALUE
    :min-y Double/MAX_VALUE :max-y Double/MIN_VALUE}
   nodes))

(defn- route-segment-smart [p1 p2 direction bounds]
  (let [vertical? (is-vertical? direction)
        stub 20.0
        side-start (:side p1)
        side-end (:side p2)

        ;; Calculate safe exit points based on port side
        p1-safe (case side-start
                  :top {:x (double (:x p1)) :y (double (- (:y p1) stub))}
                  :bottom {:x (double (:x p1)) :y (double (+ (:y p1) stub))}
                  :left {:x (double (- (:x p1) stub)) :y (double (:y p1))}
                  :right {:x (double (+ (:x p1) stub)) :y (double (:y p1))}
                  p1)
        p2-safe (case side-end
                  :top {:x (double (:x p2)) :y (double (- (:y p2) stub))}
                  :bottom {:x (double (:x p2)) :y (double (+ (:y p2) stub))}
                  :left {:x (double (- (:x p2) stub)) :y (double (:y p2))}
                  :right {:x (double (+ (:x p2) stub)) :y (double (:y p2))}
                  p2)

        ;; Helper for internal routing between safe points
        route-internal
        (fn [start end]
          (cond
            ;; --- Side-to-Side Back Edge Routing (TB: Right->Right, LR: Bottom->Bottom) ---
            (or (and (= side-start :right) (= side-end :right) vertical?)
                (and (= side-start :bottom) (= side-end :bottom) (not vertical?)))
            (let [channel-coord (double (if vertical?
                                          (+ (:max-x bounds) 50)
                                          (+ (:max-y bounds) 50)))]
              (if vertical?
                ;; TB: Right -> Channel -> Right
                [start
                 {:x channel-coord :y (double (:y start))}
                 {:x channel-coord :y (double (:y end))}
                 end]
                ;; LR: Bottom -> Channel -> Bottom
                [start
                 {:x (double (:x start)) :y channel-coord}
                 {:x (double (:x end)) :y channel-coord}
                 end]))

            ;; --- Gutter Routing for Side-to-Side Cross-Lane ---
            (and vertical? (or (and (= side-start :right) (= side-end :left))
                               (and (= side-start :left) (= side-end :right))))
            (let [mid-x (double (/ (+ (:x start) (:x end)) 2))]
              [start
               {:x mid-x :y (double (:y start))}
               {:x mid-x :y (double (:y end))}
               end])

            ;; --- Gutter Routing for Top-to-Bottom Cross-Lane ---
            (and (not vertical?) (or (and (= side-start :bottom) (= side-end :top))
                                     (and (= side-start :top) (= side-end :bottom))))
            (let [mid-y (double (/ (+ (:y start) (:y end)) 2))]
              [start
               {:x (double (:x start)) :y mid-y}
               {:x (double (:x end)) :y mid-y}
               end])

            ;; --- Mixed Side Routing (e.g. Right -> Top) ---
            (and vertical? (= side-start :right) (= side-end :top))
            (let [mid-x (if (> (:x end) (:x start))
                          (double (/ (+ (:x start) (:x end)) 2))
                          (double (+ (:x start) stub)))]
              [start
               {:x mid-x :y (double (:y start))}
               {:x mid-x :y (double (:y end))}
               end])

            ;; --- Normal Routing ---
            :else
            (route-segment-simple start end direction)))]

    ;; Combine: p1 -> p1-safe -> (internal path) -> p2-safe -> p2
    (let [internal-path (route-internal p1-safe p2-safe)
          ;; Filter duplicates
          full-path (concat
                     (if (= p1 p1-safe) [] [p1])
                     internal-path
                     (if (= p2 p2-safe) [] [p2]))]
      (simplify-points (vec full-path)))))

(defn- bounds-contain? [node point padding]
  (let [x (:x node) y (:y node) w (:w node) h (:h node)]
    (and (> (:x point) (- x padding))
         (< (:x point) (+ x w padding))
         (> (:y point) (- y padding))
         (< (:y point) (+ y h padding)))))

(defn- sanitize-waypoints [points nodes direction]
  (let [vertical? (is-vertical? direction)
        padding 5] ;; Small padding for detection
    (mapv (fn [p]
            (reduce (fn [curr-p node]
                      (if (bounds-contain? node curr-p padding)
                        ;; Collision detected! Move point to nearest safe edge + margin
                        (if vertical?
                          ;; Vertical Layout (TB): Move in X
                          (let [dist-left (Math/abs (double (- (:x curr-p) (:x node))))
                                dist-right (Math/abs (double (- (:x curr-p) (+ (:x node) (:w node)))))]
                            (if (< dist-left dist-right)
                              (assoc curr-p :x (double (- (:x node) 25)))
                              (assoc curr-p :x (double (+ (:x node) (:w node) 25)))))
                          ;; Horizontal Layout (LR): Move in Y
                          (let [dist-top (Math/abs (double (- (:y curr-p) (:y node))))
                                dist-bottom (Math/abs (double (- (:y curr-p) (+ (:y node) (:h node)))))]
                            (if (< dist-top dist-bottom)
                              (assoc curr-p :y (double (- (:y node) 25)))
                              (assoc curr-p :y (double (+ (:y node) (:h node) 25))))))
                        curr-p))
                    p
                    nodes))
          points)))

(defn route-edges [layout options]
  (let [nodes (:nodes layout)
        edges (:edges layout)
        config (config/resolve-config options)
        edge-sep (:edge-sep config 10)
        direction (:direction options :tb)

        layout-bounds (get-all-bounds nodes)

        ;; 1. Assign Ports Smartly
        port-assignments (assign-ports nodes edges options)

        ;; 2. Initial Routing (Manhattan)
        routed-edges
        (map (fn [edge]
               (let [ports (get port-assignments (:id edge))
                     p1 (:source ports)
                     p2 (:target ports)
                     ;; Sanitize waypoints to ensure they don't fall inside nodes
                     waypoints (sanitize-waypoints (:points edge) nodes direction)]
                 ;; ADD DEBUG HERE
                 (when (or (= (:to edge) "Approve?") (= (:from edge) "Approve?"))
                   (println "DEBUG: route-edges" (:id edge) (:from edge) "->" (:to edge) "p1=" p1 "p2=" p2))

                 (if (and p1 p2)
                   (let [;; Combine ports with existing waypoints (if any)
                         all-points (concat [p1] waypoints [p2])

                         ;; Route between each pair of points
                         segment-points (mapcat (fn [[start end]]
                                                  (route-segment-smart start end direction layout-bounds))
                                                (partition 2 1 all-points))

                         ;; Remove duplicates
                         unique-points (simplify-points (vec (dedupe segment-points)))

                         ;; 3. Post-Routing Sanitization
                         ;; Check if any generated points fall inside nodes and move them
                         ;; EXCLUDING start and end points (ports)
                         sanitized-points
                         (if (> (count unique-points) 2)
                           (let [start (first unique-points)
                                 end (peek unique-points)
                                 middle (subvec unique-points 1 (dec (count unique-points)))
                                 sanitized-mid (sanitize-waypoints middle nodes direction)]
                             (vec (concat [start] sanitized-mid [end])))
                           unique-points)

                         ;; 4. Re-Orthogonalize
                         ;; If points were moved, we might have diagonal lines.
                         ;; Run simple routing again to fix connections.
                         final-points (if (= unique-points sanitized-points)
                                        unique-points
                                        (let [re-routed (mapcat (fn [[start end]]
                                                                  (route-segment-simple start end direction))
                                                                (partition 2 1 sanitized-points))]
                                          (simplify-points (vec (dedupe re-routed)))))]

                     (assoc edge :points final-points))
                   edge)))
             edges)]

    ;; 3. Return updated layout
    (assoc layout :edges routed-edges)))