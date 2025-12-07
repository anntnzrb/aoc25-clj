(ns day07
  "Day 7: Laboratories - Tachyon beam splitting through quantum manifold."
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is run-tests]]))


;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Tachyon manifold: grid with S (start), . (empty), ^ (splitter)
;; Beam enters at S, moves downward
;; Splitters stop beam and emit two new beams: left and right
;; Beams merge when they reach the same column (use set)
;; Count total number of splitters hit (splits)

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "Example tachyon manifold from problem description."
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")


(defn parse
  "Parse input into grid data: {:grid [[char]] :start-col int :height int :width int}"
  [input]
  (let [lines (str/split-lines input)
        grid (mapv vec lines)
        height (count grid)
        width (count (first grid))
        [s-row s-col] (first (for [r (range height)
                                   c (range width)
                                   :when (= \S (get-in grid [r c]))]
                               [r c]))]
    {:grid grid
     :start-row s-row
     :start-col s-col
     :height height
     :width width}))


;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn- splitter?
  "Returns true if cell is a splitter (^)."
  [c]
  (= c \^))


(defn- simulate-with
  "Generic beam simulation. merge-fn controls how counts combine at same column."
  [{:keys [grid start-row start-col height width]} merge-fn]
  (loop [row (inc start-row)
         state {start-col 1}
         splits 0]
    (if (or (>= row height) (empty? state))
      [splits (reduce + (vals state))]
      (let [[new-state new-splits]
            (reduce-kv
              (fn [[acc n] col cnt]
                (if (splitter? (get-in grid [row col]))
                  [(cond-> acc
                     (pos? col) (update (dec col) merge-fn cnt)
                     (< (inc col) width) (update (inc col) merge-fn cnt))
                   (+ n cnt)]
                  [(update acc col merge-fn cnt) n]))
              [{} 0]
              state)]
        (recur (inc row) new-state (+ splits new-splits))))))


(defn part1
  "Count total number of splits in the manifold."
  [input]
  (first (simulate-with (parse input) (constantly 1))))


(defn part2
  "Count total timelines using many-worlds interpretation."
  [input]
  (second (simulate-with (parse input) (fnil + 0))))


;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [{:keys [start-row start-col height width]} (parse example)]
    (is (= 0 start-row))
    (is (= 7 start-col))
    (is (= 16 height))
    (is (= 15 width))))


(deftest test-part1
  (is (= 21 (part1 example))))


(deftest test-part2
  (is (= 40 (part2 example))))
