(ns day09
  "Day 9: Movie Theater - Find largest rectangle with red tile corners."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Red tiles: input coordinates forming vertices of an orthogonal polygon
;; Green tiles: edges connecting consecutive red tiles + polygon interior
;; Part 1: Find largest rectangle with any two red tiles as opposite corners
;; Part 2: Rectangle must fit entirely inside polygon (only red/green tiles)
;; Uses ray casting for point-in-polygon, segment crossing for validation

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "Example red tile positions from problem description."
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn- parse
  "Parse input into vector of [x y] coordinates."
  [input]
  (mapv #(mapv parse-long (str/split % #",")) (str/split-lines input)))

;; ─────────────────────────────────────────────────────────────
;; Polygon
;; ─────────────────────────────────────────────────────────────

(defn- polygon-edges
  "Build polygon edges connecting consecutive red tiles (wrapping)."
  [points]
  (map vector points (concat (rest points) [(first points)])))

(defn- normalize-h-edge
  "Normalize horizontal edge to [y x1 x2] with x1 <= x2."
  [[[x1 y] [x2 _]]]
  [y (min x1 x2) (max x1 x2)])

(defn- normalize-v-edge
  "Normalize vertical edge to [x y1 y2] with y1 <= y2."
  [[[x y1] [_ y2]]]
  [x (min y1 y2) (max y1 y2)])

(defn- partition-edges
  "Separate edges into horizontal [y x1 x2] and vertical [x y1 y2].
   H-edges sorted by y, V-edges sorted by x for binary search."
  [edges]
  (let [h-edge? (fn [[[_ y1] [_ y2]]] (= y1 y2))
        v-edge? (fn [[[x1 _] [x2 _]]] (= x1 x2))]
    [(vec (sort-by first (map normalize-h-edge (filter h-edge? edges))))
     (vec (sort-by first (map normalize-v-edge (filter v-edge? edges))))]))

;; ─────────────────────────────────────────────────────────────
;; Ray Casting
;; ─────────────────────────────────────────────────────────────

(defn- binary-search-gt
  "Find index of first element in sorted vec where (first elem) > target."
  [v target]
  (let [n (count v)]
    (loop [lo 0 hi n]
      (if (>= lo hi)
        lo
        (let [mid (quot (+ lo hi) 2)]
          (if (> (first (v mid)) target)
            (recur lo mid)
            (recur (inc mid) hi)))))))

(defn- on-h-boundary?
  "Check if point lies on any horizontal boundary edge.
   Uses binary search since h-edges are sorted by y."
  [h-edges px py]
  (let [n (count h-edges)
        ;; Find edges where y = py
        idx (binary-search-gt h-edges (dec py))]
    (loop [i idx]
      (when (< i n)
        (let [[y x1 x2] (h-edges i)]
          (cond
            (> y py) false
            (and (= y py) (<= x1 px x2)) true
            :else (recur (inc i))))))))

(defn- on-v-boundary?
  "Check if point lies on any vertical boundary edge.
   Uses binary search since v-edges are sorted by x."
  [v-edges px py]
  (let [n (count v-edges)
        idx (binary-search-gt v-edges (dec px))]
    (loop [i idx]
      (when (< i n)
        (let [[x y1 y2] (v-edges i)]
          (cond
            (> x px) false
            (and (= x px) (<= y1 py y2)) true
            :else (recur (inc i))))))))

(defn- ray-crossing-count
  "Count vertical edges crossed by rightward ray from point.
   Uses binary search since v-edges are sorted by x."
  [v-edges px py]
  (let [start-idx (binary-search-gt v-edges px)]
    (loop [i start-idx cnt 0]
      (if (>= i (count v-edges))
        cnt
        (let [[_ y1 y2] (v-edges i)]
          (recur (inc i) (if (< y1 py y2) (inc cnt) cnt)))))))

(defn- point-inside?
  "Check if point is inside polygon using ray casting."
  [h-edges v-edges px py]
  (or (on-h-boundary? h-edges px py)
      (on-v-boundary? v-edges px py)
      (odd? (ray-crossing-count v-edges px py))))

;; ─────────────────────────────────────────────────────────────
;; Rectangle Validation
;; ─────────────────────────────────────────────────────────────

(defn- v-edge-crosses-h-segment?
  "Check if any vertical edge crosses through horizontal segment.
   Uses binary search since v-edges are sorted by x."
  [v-edges lx hx y]
  (let [start-idx (binary-search-gt v-edges lx)
        n (count v-edges)]
    (loop [i start-idx]
      (when (< i n)
        (let [[x y1 y2] (v-edges i)]
          (cond
            (>= x hx) false
            (< y1 y y2) true
            :else (recur (inc i))))))))

(defn- h-edge-crosses-v-segment?
  "Check if any horizontal edge crosses through vertical segment.
   Uses binary search since h-edges are sorted by y."
  [h-edges ly hy x]
  (let [start-idx (binary-search-gt h-edges ly)
        n (count h-edges)]
    (loop [i start-idx]
      (when (< i n)
        (let [[y x1 x2] (h-edges i)]
          (cond
            (>= y hy) false
            (< x1 x x2) true
            :else (recur (inc i))))))))

(defn- rectangle-inside?
  "Check if rectangle is fully inside polygon."
  [h-edges v-edges lx hx ly hy]
  (let [mid-x (quot (+ lx hx) 2)
        mid-y (quot (+ ly hy) 2)
        inside? (partial point-inside? h-edges v-edges)]
    (and (inside? lx ly) (inside? hx ly) (inside? lx hy) (inside? hx hy)
         (inside? mid-x ly) (not (v-edge-crosses-h-segment? v-edges lx hx ly))
         (inside? mid-x hy) (not (v-edge-crosses-h-segment? v-edges lx hx hy))
         (inside? lx mid-y) (not (h-edge-crosses-v-segment? h-edges ly hy lx))
         (inside? hx mid-y) (not (h-edge-crosses-v-segment? h-edges ly hy hx)))))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn- rectangle-area
  "Calculate rectangle area: (|dx|+1) * (|dy|+1)."
  [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(defn- point-pairs
  "Generate all unique pairs of indices [i j] where i < j."
  [n]
  (for [i (range n), j (range (inc i) n)] [i j]))

(defn part1
  "Find largest rectangle area with two red tiles as opposite corners."
  [input]
  (let [points (parse input)]
    (->> (point-pairs (count points))
         (map (fn [[i j]] (rectangle-area (points i) (points j))))
         (reduce max 0))))

(defn- valid-rectangle-area
  "Return rectangle area if fully inside polygon, else 0."
  [h-edges v-edges [p1 p2]]
  (let [[x1 y1] p1
        [x2 y2] p2
        lx (min x1 x2) hx (max x1 x2)
        ly (min y1 y2) hy (max y1 y2)]
    (if (rectangle-inside? h-edges v-edges lx hx ly hy)
      (* (inc (- hx lx)) (inc (- hy ly)))
      0)))

(defn- max-valid-rectangle
  "Find max rectangle area with red corners fully inside polygon.
   Sorts pairs by area descending for early termination."
  [red-tiles]
  (let [[h-edges v-edges] (partition-edges (polygon-edges red-tiles))
        pairs (sort-by (fn [[p1 p2]] (- (rectangle-area p1 p2)))
                       (for [i (range (count red-tiles))
                             j (range (inc i) (count red-tiles))]
                         [(red-tiles i) (red-tiles j)]))]
    (reduce (fn [best pair]
              (let [area (valid-rectangle-area h-edges v-edges pair)]
                (if (pos? area)
                  (reduced area)  ; First valid = largest (sorted desc)
                  best)))
            0 pairs)))

(defn part2
  "Find largest rectangle using only red/green tiles with red corners."
  [input]
  (max-valid-rectangle (parse input)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [points (parse example)]
    (is (= 8 (count points)))
    (is (= [7 1] (first points)))
    (is (= [7 3] (last points)))))

(deftest test-rectangle-area
  (is (= 24 (rectangle-area [2 5] [9 7])))
  (is (= 35 (rectangle-area [7 1] [11 7])))
  (is (= 6 (rectangle-area [7 3] [2 3]))))

(deftest test-part1-example
  (is (= 50 (part1 example))))

(deftest test-part2-example
  (is (= 24 (part2 example))))
