#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]])

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Worksheet: vertical arrangement of math problems
;; - Numbers are stacked vertically in columns
;; - Operator (+/*) is at the bottom
;; - Problems are separated by columns of only spaces
;; - Problem: {:numbers [n1 n2 ...] :op + or *}

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

(defn transpose [rows]
  (apply mapv vector rows))

(defn pad-rows [rows]
  (let [max-len (apply max (map count rows))]
    (mapv #(vec (concat % (repeat (- max-len (count %)) \space))) rows)))

(defn column-all-spaces? [col]
  (every? #(= \space %) col))

(defn split-on-space-columns [columns]
  (->> columns
       (partition-by column-all-spaces?)
       (remove #(column-all-spaces? (first %)))))

(defn parse-problem [columns]
  ;; Each column contributes one character per row
  ;; Last row has the operator, other rows have number digits
  (let [rows (transpose columns)
        op-row (last rows)
        num-rows (butlast rows)
        op (first (remove #(= \space %) op-row))
        ;; Each row is a line of characters - extract numbers
        numbers (for [row num-rows
                      :let [s (apply str row)
                            nums (re-seq #"\d+" s)]
                      :when (seq nums)]
                  (mapv parse-long nums))]
    {:op (if (= op \+) + *)
     :numbers (apply concat numbers)}))

(defn parse-problem-cephalopod [columns]
  ;; Cephalopod math: each column is ONE number (top=most significant, bottom=least)
  ;; Read columns right-to-left
  ;; columns is a seq of columns, each column is [row0-char row1-char ... rowN-char]
  ;; The last row (last element of each column) is the operator row
  (let [op-row (mapv last columns)
        op (first (remove #(= \space %) op-row))
        ;; Each column (except operator row) forms one number
        numbers (for [col (reverse columns)  ;; right-to-left
                      :let [digits (butlast col)  ;; exclude operator row
                            digit-str (apply str (remove #(= \space %) digits))]
                      :when (seq digit-str)]
                  (parse-long digit-str))]
    {:op (if (= op \+) + *)
     :numbers (vec numbers)}))

(defn parse [input]
  (let [lines (str/split-lines input)
        rows (pad-rows (mapv vec lines))
        columns (transpose rows)
        problem-groups (split-on-space-columns columns)]
    (mapv parse-problem problem-groups)))

(defn parse2 [input]
  (let [lines (str/split-lines input)
        rows (pad-rows (mapv vec lines))
        columns (transpose rows)
        problem-groups (split-on-space-columns columns)]
    (mapv parse-problem-cephalopod problem-groups)))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn solve-problem [{:keys [op numbers]}]
  (reduce op numbers))

(defn part1 [problems]
  (->> problems
       (map solve-problem)
       (reduce +)))

(defn part2 [problems]
  (->> problems
       (map solve-problem)
       (reduce +)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [problems (parse example)]
    (is (= 4 (count problems)))
    (is (= {:op * :numbers [123 45 6]} (first problems)))
    (is (= {:op + :numbers [328 64 98]} (second problems)))))

(deftest test-solve-problem
  (is (= 33210 (solve-problem {:op * :numbers [123 45 6]})))
  (is (= 490 (solve-problem {:op + :numbers [328 64 98]})))
  (is (= 4243455 (solve-problem {:op * :numbers [51 387 215]})))
  (is (= 401 (solve-problem {:op + :numbers [64 23 314]}))))

(deftest test-part1
  (is (= 4277556 (part1 (parse example)))))

(deftest test-parse-cephalopod
  (let [problems (parse2 example)]
    (is (= 4 (count problems)))
    (is (= {:op * :numbers [356 24 1]} (first problems)))       ;; 8544
    (is (= {:op + :numbers [8 248 369]} (second problems)))     ;; 625
    (is (= {:op * :numbers [175 581 32]} (nth problems 2)))     ;; 3253600
    (is (= {:op + :numbers [4 431 623]} (nth problems 3)))))

(deftest test-part2
  (is (= 3263827 (part2 (parse2 example)))))

(deftest test-real-input
  (when (.exists (java.io.File. "input.in"))
    (let [input (slurp "input.in")]
      (is (= 4583860641327 (part1 (parse input))))
      (is (= 11602774058280 (part2 (parse2 input)))))))

;; ─────────────────────────────────────────────────────────────

(when (= *file* (System/getProperty "babashka.file"))
  (let [results (run-tests)]
    (when (zero? (+ (:fail results) (:error results)))
      (println "\n✓ Tests pass!")
      (when (.exists (java.io.File. "input.in"))
        (let [input (slurp "input.in")]
          (println "Part 1:" (part1 (parse input)))
          (println "Part 2:" (part2 (parse2 input))))))))
