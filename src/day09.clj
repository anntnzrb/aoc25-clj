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
;;
;; Optimizations:
;; - Primitive long arrays for edges (stride 3: key, min, max)
;; - Binary search with unchecked math
;; - Early termination on sorted pairs by area

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
  "Parse input into flat long array [x0 y0 x1 y1 ...]."
  ^longs [input]
  (let [lines (str/split-lines input)
        n (count lines)
        arr (long-array (* 2 n))]
    (dotimes [i n]
      (let [[x y] (str/split (lines i) #",")
            base (unchecked-multiply-int 2 i)]
        (aset arr base (parse-long x))
        (aset arr (unchecked-inc base) (parse-long y))))
    arr))

(defn- point-count ^long [^longs arr] (quot (alength arr) 2))

;; ─────────────────────────────────────────────────────────────
;; Polygon Edges (primitive arrays)
;; ─────────────────────────────────────────────────────────────

(defn- build-edges
  "Build sorted edge arrays. Returns [h-arr h-cnt v-arr v-cnt] where
   each array stores [key min max] triples sorted by key."
  [^longs points]
  (let [n (point-count points)
        h-list (java.util.ArrayList.)
        v-list (java.util.ArrayList.)]
    (dotimes [i n]
      (let [base1 (unchecked-multiply-int 2 i)
            j (mod (unchecked-inc i) n)
            base2 (unchecked-multiply-int 2 j)
            x1 (aget points base1)
            y1 (aget points (unchecked-inc base1))
            x2 (aget points base2)
            y2 (aget points (unchecked-inc base2))]
        (if (== y1 y2)
          (.add h-list (long-array [(long y1) (min x1 x2) (max x1 x2)]))
          (.add v-list (long-array [(long x1) (min y1 y2) (max y1 y2)])))))
    (let [h-sorted (sort-by #(aget ^longs % 0) h-list)
          v-sorted (sort-by #(aget ^longs % 0) v-list)
          h-cnt (count h-sorted)
          v-cnt (count v-sorted)
          h-arr (long-array (* 3 h-cnt))
          v-arr (long-array (* 3 v-cnt))]
      (dotimes [i h-cnt]
        (let [^longs e (nth h-sorted i)
              base (* 3 i)]
          (aset h-arr base (aget e 0))
          (aset h-arr (inc base) (aget e 1))
          (aset h-arr (+ base 2) (aget e 2))))
      (dotimes [i v-cnt]
        (let [^longs e (nth v-sorted i)
              base (* 3 i)]
          (aset v-arr base (aget e 0))
          (aset v-arr (inc base) (aget e 1))
          (aset v-arr (+ base 2) (aget e 2))))
      [h-arr (long h-cnt) v-arr (long v-cnt)])))

;; ─────────────────────────────────────────────────────────────
;; Binary Search
;; ─────────────────────────────────────────────────────────────

(defn- bsearch-gt
  "Find index of first element where key > target in stride-3 array."
  ^long [^longs arr ^long cnt ^long target]
  (loop [lo (long 0) hi cnt]
    (if (>= lo hi)
      lo
      (let [mid (+ lo (bit-shift-right (- hi lo) 1))]
        (if (> (aget arr (* 3 mid)) target)
          (recur lo mid)
          (recur (inc mid) hi))))))

;; ─────────────────────────────────────────────────────────────
;; Point Containment (primitive)
;; ─────────────────────────────────────────────────────────────

(defn- on-h-boundary?
  ^long [^longs h-arr ^long h-cnt ^long px ^long py]
  (let [start (bsearch-gt h-arr h-cnt (dec py))]
    (loop [i start]
      (if (>= i h-cnt)
        0
        (let [base (* 3 i)
              y (aget h-arr base)]
          (cond
            (> y py) 0
            (and (== y py)
                 (<= (aget h-arr (inc base)) px)
                 (<= px (aget h-arr (+ base 2)))) 1
            :else (recur (inc i))))))))

(defn- on-v-boundary?
  ^long [^longs v-arr ^long v-cnt ^long px ^long py]
  (let [start (bsearch-gt v-arr v-cnt (dec px))]
    (loop [i start]
      (if (>= i v-cnt)
        0
        (let [base (* 3 i)
              x (aget v-arr base)]
          (cond
            (> x px) 0
            (and (== x px)
                 (<= (aget v-arr (inc base)) py)
                 (<= py (aget v-arr (+ base 2)))) 1
            :else (recur (inc i))))))))

(defn- ray-crossings
  ^long [^longs v-arr ^long v-cnt ^long px ^long py]
  (let [start (bsearch-gt v-arr v-cnt px)]
    (loop [i start cnt (long 0)]
      (if (>= i v-cnt)
        cnt
        (let [base (* 3 i)
              y1 (aget v-arr (inc base))
              y2 (aget v-arr (+ base 2))]
          (recur (inc i)
                 (if (and (< y1 py) (< py y2)) (inc cnt) cnt)))))))

(defn- point-inside?
  [^longs h-arr h-cnt ^longs v-arr v-cnt px py]
  (let [h-cnt (long h-cnt) v-cnt (long v-cnt) px (long px) py (long py)]
    (if (pos? (on-h-boundary? h-arr h-cnt px py))
      1
      (if (pos? (on-v-boundary? v-arr v-cnt px py))
        1
        (bit-and (ray-crossings v-arr v-cnt px py) 1)))))

;; ─────────────────────────────────────────────────────────────
;; Rectangle Validation (primitive)
;; ─────────────────────────────────────────────────────────────

(defn- v-crosses-h?
  [^longs v-arr v-cnt lx hx y]
  (let [v-cnt (long v-cnt) lx (long lx) hx (long hx) y (long y)
        start (bsearch-gt v-arr v-cnt lx)]
    (loop [i start]
      (if (>= i v-cnt)
        0
        (let [base (* 3 i)
              x (aget v-arr base)]
          (cond
            (>= x hx) 0
            (and (< (aget v-arr (inc base)) y)
                 (< y (aget v-arr (+ base 2)))) 1
            :else (recur (inc i))))))))

(defn- h-crosses-v?
  [^longs h-arr h-cnt ly hy x]
  (let [h-cnt (long h-cnt) ly (long ly) hy (long hy) x (long x)
        start (bsearch-gt h-arr h-cnt ly)]
    (loop [i start]
      (if (>= i h-cnt)
        0
        (let [base (* 3 i)
              y (aget h-arr base)]
          (cond
            (>= y hy) 0
            (and (< (aget h-arr (inc base)) x)
                 (< x (aget h-arr (+ base 2)))) 1
            :else (recur (inc i))))))))

(defn- rect-inside?
  [^longs h-arr h-cnt ^longs v-arr v-cnt lx hx ly hy]
  (let [h-cnt (long h-cnt) v-cnt (long v-cnt)
        lx (long lx) hx (long hx) ly (long ly) hy (long hy)
        mid-x (bit-shift-right (+ lx hx) 1)
        mid-y (bit-shift-right (+ ly hy) 1)
        in? (fn [x y] (point-inside? h-arr h-cnt v-arr v-cnt x y))]
    (and (pos? (in? lx ly)) (pos? (in? hx ly))
         (pos? (in? lx hy)) (pos? (in? hx hy))
         (pos? (in? mid-x ly)) (zero? (v-crosses-h? v-arr v-cnt lx hx ly))
         (pos? (in? mid-x hy)) (zero? (v-crosses-h? v-arr v-cnt lx hx hy))
         (pos? (in? lx mid-y)) (zero? (h-crosses-v? h-arr h-cnt ly hy lx))
         (pos? (in? hx mid-y)) (zero? (h-crosses-v? h-arr h-cnt ly hy hx)))))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn- rect-area
  ^long [^long x1 ^long y1 ^long x2 ^long y2]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(defn part1
  "Find largest rectangle area with two red tiles as opposite corners."
  [input]
  (let [pts (parse input)
        n (point-count pts)]
    (loop [i (long 0) best (long 0)]
      (if (>= i n)
        best
        (let [base-i (* 2 i)
              x1 (aget pts base-i)
              y1 (aget pts (inc base-i))]
          (recur (inc i)
                 (loop [j (inc i) best best]
                   (if (>= j n)
                     best
                     (let [base-j (* 2 j)
                           x2 (aget pts base-j)
                           y2 (aget pts (inc base-j))
                           area (rect-area x1 y1 x2 y2)]
                       (recur (inc j) (max best area)))))))))))

(defn part2
  "Find largest rectangle using only red/green tiles with red corners."
  [input]
  (let [pts (parse input)
        n (point-count pts)
        [^longs h-arr h-cnt ^longs v-arr v-cnt] (build-edges pts)
        ;; Build pairs array: [lx hx ly hy area] sorted by -area
        pairs (long-array (* 5 (quot (* n (dec n)) 2)))
        pair-cnt (loop [i (long 0) idx (long 0)]
                   (if (>= i n)
                     (quot idx 5)
                     (recur (inc i)
                            (loop [j (inc i) idx idx]
                              (if (>= j n)
                                idx
                                (let [bi (* 2 i) bj (* 2 j)
                                      x1 (aget pts bi) y1 (aget pts (inc bi))
                                      x2 (aget pts bj) y2 (aget pts (inc bj))
                                      lx (min x1 x2) hx (max x1 x2)
                                      ly (min y1 y2) hy (max y1 y2)
                                      area (rect-area x1 y1 x2 y2)]
                                  (aset pairs idx lx)
                                  (aset pairs (inc idx) hx)
                                  (aset pairs (+ idx 2) ly)
                                  (aset pairs (+ idx 3) hy)
                                  (aset pairs (+ idx 4) area)
                                  (recur (inc j) (+ idx 5))))))))]
    ;; Process rectangles by area descending using a packed key array
    (let [mask (dec (bit-shift-left 1 20))
          keys (long-array pair-cnt)]
      (dotimes [i pair-cnt]
        (let [area (aget pairs (+ (* 5 i) 4))]
          ;; pack (-area, i) so ascending sort gives descending area
          (aset keys i (bit-or (bit-shift-left (- area) 20) i))))
      (java.util.Arrays/sort keys)
      (loop [k 0 best (long 0)]
        (if (>= k pair-cnt)
          best
          (let [key (aget keys k)
                pi (bit-and key mask)
                base (* 5 pi)
                area (aget pairs (+ base 4))]
            (if (<= area best)
              best
              (let [lx (aget pairs base)
                    hx (aget pairs (inc base))
                    ly (aget pairs (+ base 2))
                    hy (aget pairs (+ base 3))]
                (if (rect-inside? h-arr h-cnt v-arr v-cnt lx hx ly hy)
                  area
                  (recur (inc k) best))))))))))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [pts (parse example)]
    (is (= 8 (point-count pts)))
    (is (= 7 (aget pts 0)))
    (is (= 1 (aget pts 1)))))

(deftest test-rectangle-area
  (is (= 24 (rect-area 2 5 9 7)))
  (is (= 35 (rect-area 7 1 11 7)))
  (is (= 6 (rect-area 7 3 2 3))))

(deftest test-part1-example
  (is (= 50 (part1 example))))

(deftest test-part2-example
  (is (= 24 (part2 example))))
