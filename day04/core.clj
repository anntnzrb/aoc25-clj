#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]])

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Grid of paper rolls (@) and empty spaces (.)
;; Forklift can access a roll if < 4 adjacent rolls in 8 neighbors
;; Count all accessible rolls

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn parse [input]
  (->> input str/split-lines (mapv vec)))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(def directions
  [[-1 -1] [-1 0] [-1 1]
   [0 -1]         [0 1]
   [1 -1]  [1 0]  [1 1]])

(defn roll? [grid [r c]]
  (= \@ (get-in grid [r c])))

(defn count-adjacent-rolls [grid [r c]]
  (->> directions
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter #(roll? grid %))
       count))

(defn accessible? [grid pos]
  (and (roll? grid pos)
       (< (count-adjacent-rolls grid pos) 4)))

(defn all-positions [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows)
          c (range cols)]
      [r c])))

(defn part1 [grid]
  (->> (all-positions grid)
       (filter #(accessible? grid %))
       count))

(defn remove-rolls [grid positions]
  (reduce (fn [g [r c]] (assoc-in g [r c] \.)) grid positions))

(defn part2 [grid]
  (loop [g grid
         total 0]
    (let [accessible (->> (all-positions g)
                          (filter #(accessible? g %)))]
      (if (empty? accessible)
        total
        (recur (remove-rolls g accessible)
               (+ total (count accessible)))))))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (is (= [\. \. \@ \@] (take 4 (first (parse example))))))

(deftest test-roll?
  (let [grid (parse example)]
    (is (false? (roll? grid [0 0])))
    (is (true? (roll? grid [0 2])))))

(deftest test-count-adjacent
  (let [grid (parse example)]
    ;; Position [0,2] is @, neighbors: [0,3]=@, [1,1]=@, [1,2]=@, [1,3]=.
    (is (= 3 (count-adjacent-rolls grid [0 2])))))

(deftest test-accessible?
  (let [grid (parse example)]
    ;; Position [0,2] has 3 adjacent rolls (< 4), so accessible
    (is (true? (accessible? grid [0 2])))
    ;; Position [2,2] has 5 neighbors, not accessible
    (is (false? (accessible? grid [2 2])))))

(deftest test-part1
  ;; Example has 13 accessible rolls
  (is (= 13 (part1 (parse example)))))

(deftest test-part2
  ;; Example: 13+12+7+5+2+1+1+1+1 = 43 total removed
  (is (= 43 (part2 (parse example)))))

(deftest test-real-input
  (when (.exists (java.io.File. "input.in"))
    (let [data (parse (slurp "input.in"))]
      (is (= 1437 (part1 data)))
      (is (= 8765 (part2 data))))))

;; ─────────────────────────────────────────────────────────────

(when (= *file* (System/getProperty "babashka.file"))
  (let [results (run-tests)]
    (when (zero? (+ (:fail results) (:error results)))
      (println "\n✓ Tests pass!")
      (when (.exists (java.io.File. "input.in"))
        (let [data (parse (slurp "input.in"))]
          (println "Part 1:" (part1 data))
          (println "Part 2:" (part2 data)))))))
