(ns day06
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

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

(def example
  "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

(defn- transpose
  "Transposes a 2D vector (rows become columns)."
  [rows]
  (apply mapv vector rows))

(defn- pad-rows
  "Pads all rows to the same length with spaces."
  [rows]
  (let [max-len (apply max (map count rows))]
    (mapv #(vec (concat % (repeat (- max-len (count %)) \space))) rows)))

(defn- column-all-spaces?
  "Returns true if column contains only spaces."
  [col]
  (every? #(= \space %) col))

(defn- split-on-space-columns
  "Splits columns into groups separated by all-space columns."
  [columns]
  (->> columns
       (partition-by column-all-spaces?)
       (remove #(column-all-spaces? (first %)))))

(defn- parse-problem
  "Parses columns into {:op fn :numbers [n...]} for standard math."
  [columns]
  (let [rows (transpose columns)
        op-row (last rows)
        num-rows (butlast rows)
        op (first (remove #(= \space %) op-row))
        ;; Each row is a line of characters - extract numbers
        numbers (for [row num-rows
                      :let [s (str/join row)
                            nums (re-seq #"\d+" s)]
                      :when (seq nums)]
                  (mapv parse-long nums))]
    {:op (if (= op \+) + *)
     :numbers (apply concat numbers)}))

(defn- parse-problem-cephalopod
  "Parses columns into {:op fn :numbers [n...]} for cephalopod math (vertical digits, right-to-left)."
  [columns]
  (let [op-row (mapv last columns)
        op (first (remove #(= \space %) op-row))
        ;; Each column (except operator row) forms one number
        numbers (for [col (reverse columns)  ; right-to-left
                      :let [digits (butlast col)  ; exclude operator row
                            digit-str (str/join (remove #(= \space %) digits))]
                      :when (seq digit-str)]
                  (parse-long digit-str))]
    {:op (if (= op \+) + *)
     :numbers (vec numbers)}))

(defn- parse-with
  "Parses worksheet input into problems using the given problem parser."
  [parser-fn input]
  (let [lines (str/split-lines input)
        rows (pad-rows (mapv vec lines))
        columns (transpose rows)
        problem-groups (split-on-space-columns columns)]
    (mapv parser-fn problem-groups)))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn solve-problem
  "Evaluates a problem by reducing numbers with the operator."
  [{:keys [op numbers]}]
  (reduce op numbers))

(defn part1
  "Sums results of all problems (standard parsing)."
  [input]
  (->> (parse-with parse-problem input)
       (map solve-problem)
       (reduce +)))

(defn part2
  "Sums results of all problems (cephalopod parsing)."
  [input]
  (->> (parse-with parse-problem-cephalopod input)
       (map solve-problem)
       (reduce +)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [problems (parse-with parse-problem example)]
    (is (= 4 (count problems)))
    (is (= {:op * :numbers [123 45 6]} (first problems)))
    (is (= {:op + :numbers [328 64 98]} (second problems)))))

(deftest test-solve-problem
  (is (= 33210 (solve-problem {:op * :numbers [123 45 6]})))
  (is (= 490 (solve-problem {:op + :numbers [328 64 98]})))
  (is (= 4243455 (solve-problem {:op * :numbers [51 387 215]})))
  (is (= 401 (solve-problem {:op + :numbers [64 23 314]}))))

(deftest test-part1
  (is (= 4277556 (part1 example))))

(deftest test-parse-cephalopod
  (let [problems (parse-with parse-problem-cephalopod example)]
    (is (= 4 (count problems)))
    (is (= {:op * :numbers [356 24 1]} (first problems)))       ; 8544
    (is (= {:op + :numbers [8 248 369]} (second problems)))     ; 625
    (is (= {:op * :numbers [175 581 32]} (nth problems 2)))     ; 3253600
    (is (= {:op + :numbers [4 431 623]} (nth problems 3)))))

(deftest test-part2
  (is (= 3263827 (part2 example))))
