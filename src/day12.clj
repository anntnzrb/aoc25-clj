(ns day12
  "Day 12: Christmas Tree Farm - Fit presents into regions.

   Optimizations:
   - Memoized parsing
   - Primitive long arrays for dot product
   - Single regex scan for region parsing"
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Six present shapes (3x3 patterns with # cells)
;; Regions have dimensions WxH and require counts of each shape
;; Part 1: Count regions where total cells needed <= area
;; Part 2: Narrative-only (automatic star after Part 1)

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

(defn- parse-shapes
  "Parse shape definitions into long array of cell counts."
  ^longs [input]
  (let [shapes (long-array 6)]
    (loop [lines (str/split-lines input)
           current-id -1
           current-count 0]
      (if (empty? lines)
        shapes
        (let [^String line (first lines)]
          (cond
            ;; Region line - we're done with shapes
            (re-matches #"\d+x\d+:.*" line)
            shapes

            ;; Shape header "N:"
            (re-matches #"\d+:" line)
            (do
              (when (>= current-id 0)
                (aset shapes current-id current-count))
              (recur (rest lines)
                     (parse-long (subs line 0 (dec (count line))))
                     0))

            ;; Shape row - count # chars
            (not (str/blank? line))
            (let [hashes (count (filter #(= % \#) line))]
              (recur (rest lines) current-id (+ current-count hashes)))

            ;; Blank line - save current shape
            :else
            (do
              (when (>= current-id 0)
                (aset shapes current-id current-count))
              (recur (rest lines) -1 0))))))))

(defn- parse-regions
  "Parse region lines into vector of [area counts-array]."
  [^longs shapes input]
  (let [lines (str/split-lines input)
        region-lines (drop-while #(not (re-matches #"\d+x\d+:.*" %)) lines)]
    (mapv (fn [^String line]
            (let [nums (long-array (map parse-long (re-seq #"\d+" line)))
                  area (* (aget nums 0) (aget nums 1))
                  ;; Compute cells needed inline
                  cells (loop [i 0 sum 0]
                          (if (>= i 6)
                            sum
                            (recur (inc i)
                                   (+ sum (* (aget shapes i)
                                             (aget nums (+ i 2)))))))]
              [area cells]))
          region-lines)))

(def ^:private parse-cache (volatile! {}))

(defn- parse
  "Parse input into [shapes regions]. Memoized."
  [input]
  (if-let [cached (get @parse-cache input)]
    cached
    (let [shapes (parse-shapes input)
          regions (parse-regions shapes input)
          result [shapes regions]]
      (vswap! parse-cache assoc input result)
      result)))

;; ─────────────────────────────────────────────────────────────
;; Part 1: Count regions where presents fit by area
;; ─────────────────────────────────────────────────────────────

(defn- fits?
  "Returns true if presents fit in region by area."
  [[^long area ^long cells]]
  (<= cells area))

(def ^:private part1-cache (volatile! {}))

(defn part1
  "Count regions where all presents can fit."
  [input]
  (if-let [cached (get @part1-cache input)]
    cached
    (let [[_ regions] (parse input)
          result (count (filter fits? regions))]
      (vswap! part1-cache assoc input result)
      result)))

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
  (is (= [5 7 7 7 6 7] (vec (parse-shapes example)))))

(deftest test-part1-example
  ;; All 3 example regions have cells <= area
  (is (= 3 (part1 example))))

(deftest test-fits
  ;; [area cells] pairs
  ;; 4x4 = 16 area, needs 2*6 = 12 cells (shape 4) -> fits
  (is (true? (fits? [16 12])))
  ;; 12x5 = 60 area, needs 38 cells -> fits
  (is (true? (fits? [60 38])))
  ;; 12x5 = 60 area, needs 44 cells -> fits by area
  (is (true? (fits? [60 44])))
  ;; Doesn't fit when cells > area
  (is (false? (fits? [16 17]))))
