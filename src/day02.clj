(ns day02
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))


;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Part 1: Invalid IDs are those where a sequence is repeated exactly twice.
;; Examples: 55 (5 twice), 6464 (64 twice), 123123 (123 twice)
;;
;; Part 2: Invalid IDs are those where a sequence is repeated at least twice.
;; Examples: 12341234 (1234 x2), 123123123 (123 x3), 1111111 (1 x7)

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")


(defn- parse-range
  "Parses 'start-end' into [start end] longs."
  [s]
  (let [[start end] (str/split s #"-")]
    [(parse-long start) (parse-long end)]))


(defn- parse
  "Parses comma-separated ranges into vector of [start end] pairs."
  [input]
  (->> (str/split (str/trim input) #",")
       (map str/trim)
       (filter seq)
       (mapv parse-range)))


;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn invalid?
  "Check if a number is invalid (first half of digits = second half)"
  [n]
  (let [s (str n)
        len (count s)]
    (and (even? len)
         (= (subs s 0 (/ len 2))
            (subs s (/ len 2))))))


(defn generate-invalids-in-range
  "Generate all invalid numbers in the given range [start, end].
   Instead of iterating through all numbers, we generate candidate invalid numbers
   and check if they fall in the range."
  [start end]
  (let [;; Maximum number of digits in half of the largest number
        max-digits (count (str end))
        max-half-len (quot (inc max-digits) 2)]
    (for [half-len (range 1 (inc max-half-len))
          :let [min-base (long (Math/pow 10 (dec half-len)))
                ;; For half-len 1: min-base = 1 (gives 11, 22, ..., 99)
                min-base (if (= half-len 1) 1 min-base)
                max-base (dec (long (Math/pow 10 half-len)))]
          base (range min-base (inc max-base))
          :let [s (str base)
                invalid-num (parse-long (str s s))]
          :when (and (>= invalid-num start) (<= invalid-num end))]
      invalid-num)))


(defn part1
  "Sums all invalid IDs (half=half pattern) across all ranges."
  [input]
  (let [ranges (parse input)]
    (transduce
      (mapcat (fn [[start end]] (generate-invalids-in-range start end)))
      +
      0
      ranges)))


;; ─────────────────────────────────────────────────────────────
;; Part 2 - Pattern repeated at least twice
;; ─────────────────────────────────────────────────────────────

(defn invalid-v2?
  "Check if a number is invalid (some pattern repeated at least twice)"
  [n]
  (let [s (str n)
        len (count s)]
    (some (fn [pattern-len]
            (when (zero? (mod len pattern-len))
              (let [pattern (subs s 0 pattern-len)
                    repeats (quot len pattern-len)]
                (and (>= repeats 2)
                     (= s (str/join (repeat repeats pattern)))))))
          (range 1 (inc (quot len 2))))))


(defn generate-invalids-in-range-v2
  "Generate all invalid numbers in the given range [start, end] for part 2.
   Pattern can be repeated 2 or more times."
  [start end]
  (let [min-digits (count (str start))
        max-digits (count (str end))]
    (->> (for [num-digits (range min-digits (inc max-digits))
               pattern-len (range 1 (inc (quot num-digits 2)))
               :when (zero? (mod num-digits pattern-len))
               :let [repeats (quot num-digits pattern-len)
                     min-pattern (long (Math/pow 10 (dec pattern-len)))
                     max-pattern (dec (long (Math/pow 10 pattern-len)))]
               pattern-num (range min-pattern (inc max-pattern))
               :let [pattern-str (str pattern-num)
                     invalid-str (str/join (repeat repeats pattern-str))
                     invalid-num (parse-long invalid-str)]
               :when (and (>= invalid-num start)
                          (<= invalid-num end))]
           invalid-num)
         (into #{})  ; Remove duplicates (e.g., 1111 = "11"x2 = "1"x4)
         sort)))


(defn part2
  "Sums all invalid IDs (any repeated pattern) across all ranges."
  [input]
  (let [ranges (parse input)]
    (transduce
      (mapcat (fn [[start end]] (generate-invalids-in-range-v2 start end)))
      +
      0
      ranges)))


;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-invalid?
  (is (invalid? 55))
  (is (invalid? 6464))
  (is (invalid? 123123))
  (is (invalid? 11))
  (is (invalid? 22))
  (is (invalid? 99))
  (is (invalid? 1010))
  (is (invalid? 1188511885))
  (is (invalid? 222222))
  (is (invalid? 446446))
  (is (invalid? 38593859))
  (is (not (invalid? 101)))  ; odd number of digits
  (is (not (invalid? 12)))   ; first half != second half
  (is (not (invalid? 1234))) ; first half != second half
  (is (not (invalid? 0101))) ; would have leading zero, not a valid ID
  )


(deftest test-generate-invalids
  ;; 11-22 has two invalid IDs: 11 and 22
  (is (= [11 22] (generate-invalids-in-range 11 22)))
  ;; 95-115 has one invalid ID: 99
  (is (= [99] (generate-invalids-in-range 95 115)))
  ;; 998-1012 has one invalid ID: 1010
  (is (= [1010] (generate-invalids-in-range 998 1012)))
  ;; 1188511880-1188511890 has one invalid ID: 1188511885
  (is (= [1188511885] (generate-invalids-in-range 1188511880 1188511890)))
  ;; 222220-222224 has one invalid ID: 222222
  (is (= [222222] (generate-invalids-in-range 222220 222224)))
  ;; 1698522-1698528 contains no invalid IDs
  (is (= [] (generate-invalids-in-range 1698522 1698528)))
  ;; 446443-446449 has one invalid ID: 446446
  (is (= [446446] (generate-invalids-in-range 446443 446449)))
  ;; 38593856-38593862 has one invalid ID: 38593859
  (is (= [38593859] (generate-invalids-in-range 38593856 38593862)))
  ;; 565653-565659 contains no invalid IDs
  (is (= [] (generate-invalids-in-range 565653 565659)))
  ;; 824824821-824824827 contains no invalid IDs
  (is (= [] (generate-invalids-in-range 824824821 824824827)))
  ;; 2121212118-2121212124 contains no invalid IDs
  (is (= [] (generate-invalids-in-range 2121212118 2121212124))))


(deftest test-part1
  ;; Example sum: 11 + 22 + 99 + 1010 + 1188511885 + 222222 + 446446 + 38593859 = 1227775554
  (is (= 1227775554 (part1 example))))


(deftest test-invalid-v2?
  (is (invalid-v2? 55))
  (is (invalid-v2? 6464))
  (is (invalid-v2? 123123))
  (is (invalid-v2? 12341234))   ; 1234 x2
  (is (invalid-v2? 123123123))  ; 123 x3
  (is (invalid-v2? 1212121212)) ; 12 x5
  (is (invalid-v2? 1111111))    ; 1 x7
  (is (invalid-v2? 111))        ; 1 x3
  (is (invalid-v2? 999))        ; 9 x3
  (is (invalid-v2? 565656))     ; 56 x3
  (is (invalid-v2? 824824824))  ; 824 x3
  (is (invalid-v2? 2121212121)) ; 21 x5
  (is (not (invalid-v2? 101)))
  (is (not (invalid-v2? 12345))))


(deftest test-generate-invalids-v2
  ;; 11-22 still has two invalid IDs: 11 and 22
  (is (= [11 22] (generate-invalids-in-range-v2 11 22)))
  ;; 95-115 now has two invalid IDs: 99 and 111
  (is (= [99 111] (generate-invalids-in-range-v2 95 115)))
  ;; 998-1012 now has two invalid IDs: 999 and 1010
  (is (= [999 1010] (generate-invalids-in-range-v2 998 1012)))
  ;; 1188511880-1188511890 still has one invalid ID: 1188511885
  (is (= [1188511885] (generate-invalids-in-range-v2 1188511880 1188511890)))
  ;; 222220-222224 still has one invalid ID: 222222
  (is (= [222222] (generate-invalids-in-range-v2 222220 222224)))
  ;; 1698522-1698528 still contains no invalid IDs
  (is (= [] (generate-invalids-in-range-v2 1698522 1698528)))
  ;; 446443-446449 still has one invalid ID: 446446
  (is (= [446446] (generate-invalids-in-range-v2 446443 446449)))
  ;; 38593856-38593862 still has one invalid ID: 38593859
  (is (= [38593859] (generate-invalids-in-range-v2 38593856 38593862)))
  ;; 565653-565659 now has one invalid ID: 565656
  (is (= [565656] (generate-invalids-in-range-v2 565653 565659)))
  ;; 824824821-824824827 now has one invalid ID: 824824824
  (is (= [824824824] (generate-invalids-in-range-v2 824824821 824824827)))
  ;; 2121212118-2121212124 now has one invalid ID: 2121212121
  (is (= [2121212121] (generate-invalids-in-range-v2 2121212118 2121212124))))


(deftest test-part2
  ;; Example sum: 4174379265
  (is (= 4174379265 (part2 example))))
