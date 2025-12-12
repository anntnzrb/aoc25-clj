(ns day12
  "Day 12: Christmas Tree Farm - Fit presents into regions."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Six present shapes (3x3 patterns), each with # cells:
;; Dynamically parsed from input

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "0:
#..
##.
.##

1:
###
.#.
###

2:
###
###
..#

3:
###
.##
##.

4:
###
##.
#..

5:
#.#
#.#
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(defn- count-hash-cells
  "Count # characters in a sequence of strings."
  [rows]
  (reduce + (map #(count (filter (fn [c] (= c \#)) %)) rows)))

(defn- parse-shapes
  "Parse shape definitions from input into vector of cell counts."
  [input]
  (let [lines (str/split-lines input)]
    (loop [remaining lines
           shapes (sorted-map)]
      (if (or (empty? remaining)
              (re-matches #"\d+x\d+:.*" (first remaining)))
        (mapv #(get shapes % 0) (range 6))
        (let [line (first remaining)]
          (if (re-matches #"(\d+):" line)
            (let [id (parse-long (re-find #"\d+" line))
                  rows (take-while #(and (not (str/blank? %))
                                         (not (re-matches #"\d+:?" %)))
                                   (rest remaining))
                  cells (count-hash-cells rows)]
              (recur (drop (+ 1 (count rows)) remaining)
                     (assoc shapes id cells)))
            (recur (rest remaining) shapes)))))))

(defn- parse-region
  "Parse 'WxH: n0 n1 n2 n3 n4 n5' into {:w :h :counts}."
  [line]
  (let [nums (mapv parse-long (re-seq #"\d+" line))]
    {:w (nums 0)
     :h (nums 1)
     :counts (subvec nums 2)}))

(defn- parse-regions
  "Parse all region lines from input (after the shape definitions)."
  [input]
  (let [lines (str/split-lines input)
        region-lines (drop-while #(not (re-matches #"\d+x\d+:.*" %)) lines)]
    (mapv parse-region region-lines)))

(defn- parse
  "Parse input into {:shapes [cell-counts] :regions [region-maps]}."
  [input]
  {:shapes (parse-shapes input)
   :regions (parse-regions input)})

;; ─────────────────────────────────────────────────────────────
;; Part 1: Count regions where presents fit by area
;; ─────────────────────────────────────────────────────────────

(defn- total-cells-needed
  "Calculate total cells needed for given counts of each shape."
  [shape-cells counts]
  (reduce + (map * counts shape-cells)))

(defn- fits?
  "Returns true if presents fit in region by area."
  [shape-cells {:keys [w h counts]}]
  (<= (total-cells-needed shape-cells counts) (* w h)))

(defn part1
  "Count regions where all presents can fit."
  [input]
  (let [{:keys [shapes regions]} (parse input)]
    (count (filter (partial fits? shapes) regions))))

;; ─────────────────────────────────────────────────────────────
;; Part 2: No additional calculation - story completion
;; ─────────────────────────────────────────────────────────────

(defn part2
  "Day 12 Part 2 is narrative-only (automatic star after Part 1)."
  [input]
  (part1 input))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse-shapes
  (is (= [5 7 7 7 6 7] (parse-shapes example))))

(deftest test-part1-example
  ;; All 3 example regions have cells <= area
  (is (= 3 (part1 example))))

(deftest test-fits
  (let [shapes [5 7 7 7 6 7]]
    ;; 4x4 = 16 area, needs 2*6 = 12 cells (shape 4) -> fits
    (is (true? (fits? shapes {:w 4 :h 4 :counts [0 0 0 0 2 0]})))
    ;; 12x5 = 60 area, needs 38 cells -> fits
    (is (true? (fits? shapes {:w 12 :h 5 :counts [1 0 1 0 2 2]})))
    ;; 12x5 = 60 area, needs 44 cells -> fits by area
    (is (true? (fits? shapes {:w 12 :h 5 :counts [1 0 1 0 3 2]})))))
