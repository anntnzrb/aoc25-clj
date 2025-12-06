(require '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]])

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Ranges: list of [start end] pairs (inclusive)
;; Available IDs: list of ingredient IDs to check
;; Fresh: an ID is fresh if it falls within any range

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn parse-range
  "Parses 'start-end' into [start end] longs."
  [line]
  (let [[start end] (str/split line #"-")]
    [(parse-long start) (parse-long end)]))

(defn parse
  "Parses input into {:ranges [[s e]...] :ids [id...]}."
  [input]
  (let [[ranges-section ids-section] (str/split input #"\n\n")
        ranges (->> ranges-section str/split-lines (mapv parse-range))
        ids (->> ids-section str/split-lines (mapv parse-long))]
    {:ranges ranges :ids ids}))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn in-range?
  "Returns true if id is within [start end] inclusive."
  [id [start end]]
  (and (>= id start) (<= id end)))

(defn fresh?
  "Returns true if id falls within any range."
  [ranges id]
  (boolean (some #(in-range? id %) ranges)))

(defn merge-ranges
  "Merges overlapping/adjacent ranges into disjoint ranges."
  [ranges]
  (let [sorted (sort-by first ranges)]
    (reduce (fn [merged [start end]]
              (if (empty? merged)
                [[start end]]
                (let [[prev-start prev-end] (peek merged)]
                  (if (<= start (inc prev-end))
                    (conj (pop merged) [prev-start (max prev-end end)])
                    (conj merged [start end])))))
            []
            sorted)))

(defn part1
  "Counts how many IDs fall within any range."
  [{:keys [ranges ids]}]
  (let [merged (merge-ranges ranges)]
    (count (filter #(fresh? merged %) ids))))

(defn range-size
  "Returns the count of integers in range [start end] inclusive."
  [[start end]]
  (inc (- end start)))

(defn part2
  "Counts total unique IDs covered by all ranges."
  [{:keys [ranges]}]
  (->> ranges
       merge-ranges
       (map range-size)
       (reduce +)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [{:keys [ranges ids]} (parse example)]
    (is (= [[3 5] [10 14] [16 20] [12 18]] ranges))
    (is (= [1 5 8 11 17 32] ids))))

(deftest test-in-range?
  (is (true? (in-range? 5 [3 5])))
  (is (true? (in-range? 3 [3 5])))
  (is (false? (in-range? 2 [3 5])))
  (is (false? (in-range? 6 [3 5]))))

(deftest test-fresh?
  (let [{:keys [ranges]} (parse example)]
    (is (false? (fresh? ranges 1)))
    (is (true? (fresh? ranges 5)))
    (is (false? (fresh? ranges 8)))
    (is (true? (fresh? ranges 11)))
    (is (true? (fresh? ranges 17)))
    (is (false? (fresh? ranges 32)))))

(deftest test-part1
  (is (= 3 (part1 (parse example)))))

(deftest test-merge-ranges
  (is (= [[3 5] [10 20]] (merge-ranges [[3 5] [10 14] [16 20] [12 18]]))))

(deftest test-part2
  (is (= 14 (part2 (parse example)))))

(deftest test-real-input
  (when (.exists (java.io.File. "input.in"))
    (let [data (parse (slurp "input.in"))]
      (is (= 773 (part1 data)))
      (is (= 332067203034711 (part2 data))))))

;; ─────────────────────────────────────────────────────────────

(defn -main
  "Runs tests and prints solutions for real input."
  [& _]
  (let [results (run-tests)]
    (when (zero? (+ (:fail results) (:error results)))
      (println "\n✓ Tests pass!")
      (when (.exists (java.io.File. "input.in"))
        (let [data (parse (slurp "input.in"))]
          (println "Part 1:" (part1 data))
          (println "Part 2:" (part2 data)))))))

(-main)
