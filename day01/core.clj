(ns day01.core
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))


;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; A dial with positions 0-99 (circular)
;; Rotations: L (left, toward lower) or R (right, toward higher)
;; Start position: 50
;; Part 1: count times dial lands on 0 after a rotation
;; Part 2: count ALL times dial passes through 0 (including during rotations)

(def dial-size
  "Number of positions on the dial (0-99)."
  100)


(def start-pos
  "Initial dial position."
  50)


;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")


(defn- parse-rotation
  "Parses 'L68' or 'R48' into {:dir \\L :dist 68}."
  [line]
  (let [dir (first line)
        dist (parse-long (subs line 1))]
    {:dir dir :dist dist}))


(defn- parse
  "Parses input into a vector of rotation instructions."
  [input]
  (->> input str/split-lines (mapv parse-rotation)))


;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn apply-rotation
  "Returns new dial position after applying rotation from pos."
  [pos {:keys [dir dist]}]
  (let [delta (if (= dir \L) (- dist) dist)]
    (mod (+ pos delta) dial-size)))


(defn part1
  "Counts times dial lands on 0 after a rotation."
  [input]
  (let [rotations (parse input)]
    (->> rotations
         (reductions apply-rotation start-pos)
         rest
         (filter zero?)
         count)))


(defn count-zeros-in-rotation
  "Count how many times the dial passes through 0 during a rotation.
   For Right: first 0 at step (100 - start), then every 100 steps
   For Left: first 0 at step start, then every 100 steps"
  [start {:keys [dir dist]}]
  (let [first-k (if (= dir \R)
                  (mod (- dial-size start) dial-size)
                  (mod start dial-size))
        first-k (if (zero? first-k) dial-size first-k)]
    (if (<= first-k dist)
      (inc (quot (- dist first-k) dial-size))
      0)))


(defn part2
  "Counts all times dial passes through 0, including during rotations."
  [input]
  (let [rotations (parse input)]
    (->> rotations
         (reductions (fn [[pos _] rot]
                       [(apply-rotation pos rot)
                        (count-zeros-in-rotation pos rot)])
                     [start-pos 0])
         rest
         (map second)
         (reduce +))))


;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (is (= {:dir \L :dist 68} (parse-rotation "L68")))
  (is (= {:dir \R :dist 48} (parse-rotation "R48"))))


(deftest test-apply-rotation
  ;; From 11, R8 -> 19
  (is (= 19 (apply-rotation 11 {:dir \R :dist 8})))
  ;; From 19, L19 -> 0
  (is (zero? (apply-rotation 19 {:dir \L :dist 19})))
  ;; From 5, L10 -> 95 (wraps around)
  (is (= 95 (apply-rotation 5 {:dir \L :dist 10})))
  ;; From 95, R5 -> 0 (wraps around)
  (is (zero? (apply-rotation 95 {:dir \R :dist 5}))))


(deftest test-part1
  ;; Example: dial lands on 0 three times (after R48, L55, L99)
  (is (= 3 (part1 example))))


(deftest test-count-zeros
  ;; R1000 from 50 should hit 0 ten times
  (is (= 10 (count-zeros-in-rotation 50 {:dir \R :dist 1000})))
  ;; L68 from 50 hits 0 once (at step 50)
  (is (= 1 (count-zeros-in-rotation 50 {:dir \L :dist 68})))
  ;; L30 from 82 doesn't hit 0 (would need 82 steps)
  (is (zero? (count-zeros-in-rotation 82 {:dir \L :dist 30})))
  ;; R48 from 52 hits 0 exactly at the end
  (is (= 1 (count-zeros-in-rotation 52 {:dir \R :dist 48}))))


(deftest test-part2
  ;; Example: 3 at end + 3 during = 6
  (is (= 6 (part2 example))))
