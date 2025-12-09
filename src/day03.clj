(ns day03
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Batteries in banks, each labeled with joltage 1-9
;; Part 1: Pick exactly 2 batteries per bank to form a 2-digit number
;; Part 2: Pick exactly 12 batteries per bank to form a 12-digit number
;; Maximize the joltage for each bank, sum all maximums

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "987654321111111
811111111111119
234234234234278
818181911112111")

(defn- parse-line
  "Converts a string of digits into a vector of ints."
  [line]
  (mapv #(Character/digit % 10) line))

(defn- parse
  "Parses input into a vector of digit vectors (battery banks)."
  [input]
  (->> input str/split-lines (mapv parse-line)))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn max-joltage
  "Find the maximum 2-digit joltage from a bank of batteries.
   For each position i, the best joltage is digit[i]*10 + max(digits[i+1:]).
   Uses suffix-max for O(n) performance."
  [digits]
  (let [^ints arr (int-array digits)
        n (alength arr)
        ;; Build suffix-max array
        ^ints smax (int-array n)]
    (aset smax (dec n) (aget arr (dec n)))
    (loop [i (- n 2)]
      (when (>= i 0)
        (aset smax i (max (aget arr i) (aget smax (inc i))))
        (recur (dec i))))
    ;; Find best joltage
    (loop [i 0 best 0]
      (if (>= i (dec n))
        best
        (recur (inc i)
               (max best (+ (* 10 (aget arr i)) (aget smax (inc i)))))))))

(defn part1
  "Sums max 2-digit joltages from all banks."
  [input]
  (transduce (map max-joltage) + (parse input)))

(defn max-joltage-k
  "Find the maximum k-digit joltage from a bank of batteries.
   Greedy: for each step, pick the largest digit that leaves
   enough remaining digits."
  [digits k]
  (let [^ints arr (int-array digits)
        n (alength arr)]
    (loop [pos 0 result 0 step 0]
      (if (= step k)
        result
        (let [end (- n (- k step))
              ;; Find best index in [pos, end]
              best-idx (loop [bi pos i (inc pos)]
                         (if (> i end)
                           bi
                           (recur (if (> (aget arr i) (aget arr bi)) i bi)
                                  (inc i))))]
          (recur (inc best-idx)
                 (+ (* 10 result) (aget arr best-idx))
                 (inc step)))))))

(defn part2
  "Sums max 12-digit joltages from all banks."
  [input]
  (transduce (map #(max-joltage-k % 12)) + (parse input)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (is (= [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] (parse-line "987654321111111")))
  (is (= [[9 8 7] [1 2 3]] (parse "987\n123"))))

(deftest test-max-joltage
  ;; 987654321111111 -> 98 (first two are largest)
  (is (= 98 (max-joltage [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])))
  ;; 811111111111119 -> 89 (8 first, then 9 at end)
  (is (= 89 (max-joltage [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9])))
  ;; 234234234234278 -> 78 (last two digits)
  (is (= 78 (max-joltage [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8])))
  ;; 818181911112111 -> 92 (9 then 2)
  (is (= 92 (max-joltage [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1]))))

(deftest test-part1
  ;; Example: 98 + 89 + 78 + 92 = 357
  (is (= 357 (part1 example))))

(deftest test-max-joltage-k
  ;; 987654321111111 -> 987654321111 (skip 3 ones at end)
  (is (= 987654321111 (max-joltage-k [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] 12)))
  ;; 811111111111119 -> 811111111119 (skip 3 ones)
  (is (= 811111111119 (max-joltage-k [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9] 12)))
  ;; 234234234234278 -> 434234234278 (skip 2,3,2 at start)
  (is (= 434234234278 (max-joltage-k [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8] 12)))
  ;; 818181911112111 -> 888911112111 (skip some 1s)
  (is (= 888911112111 (max-joltage-k [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1] 12))))

(deftest test-part2
  ;; Example: 987654321111 + 811111111119 + 434234234278 + 888911112111 = 3121910778619
  (is (= 3121910778619 (part2 example))))
