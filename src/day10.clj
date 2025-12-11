(ns day10
  "Day 10: Factory - Toggle indicator lights and configure joltage.

   Optimizations:
   - Part 1: GF(2) Gaussian elimination with kernel-based search (~17ms)
   - Part 2: Ratio Gaussian elimination with double-precision search (~86ms)
     - Pre-compute coefficients as doubles for fast inner loop
     - Parallel machine processing with pmap"
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn- parse-line
  "Parse a machine line into {:target :buttons :button-indices :joltage}."
  [^String line]
  (let [parts (str/split line #"\s+")
        target-str ^String (first parts)
        pattern (subs target-str 1 (dec (count target-str)))
        target (reduce-kv (fn [m i c] (if (= c \#) (bit-set m i) m))
                          0 (vec pattern))
        button-strs (vec (take-while #(str/starts-with? ^String % "(") (rest parts)))
        buttons (mapv (fn [^String s]
                        (reduce (fn [m n] (bit-set m (parse-long n)))
                                0 (re-seq #"\d+" s)))
                      button-strs)
        button-indices (mapv (fn [^String s]
                               (into #{} (map parse-long) (re-seq #"\d+" s)))
                             button-strs)
        joltage-str ^String (last parts)
        joltage (mapv parse-long (re-seq #"\d+" joltage-str))]
    {:target target
     :buttons buttons
     :button-indices button-indices
     :joltage joltage}))

(defn- parse [input]
  (mapv parse-line (str/split-lines input)))

;; ─────────────────────────────────────────────────────────────
;; Part 1: GF(2) Gaussian Elimination (XOR)
;; ─────────────────────────────────────────────────────────────
;;
;; Solve Ax = b over GF(2) where A[j][i] = 1 if button i affects light j
;; Matrix is m x n (lights x buttons), stored as m row-bitmasks

(defn- build-gf2-matrix
  "Build GF(2) matrix: row j has bit i set if button i affects light j."
  [buttons n-lights]
  (let [n (count buttons)
        rows (long-array n-lights 0)]
    (dotimes [i n]
      (let [btn (buttons i)]
        (dotimes [j n-lights]
          (when (bit-test btn j)
            (aset rows j (bit-set (aget rows j) i))))))
    rows))

(defn- gf2-eliminate
  "Gaussian elimination over GF(2). Returns {:rows rref :target bits :pivots cols}."
  [^longs rows ^long target n-lights n-buttons]
  (let [tgt (long-array n-lights)]
    (dotimes [j n-lights]
      (aset tgt j (if (bit-test target j) 1 0)))
    (loop [pivots (transient [])
           row 0
           col 0]
      (if (or (>= row n-lights) (>= col n-buttons))
        {:rows (vec rows) :target (vec tgt) :pivots (persistent! pivots)}
        (let [pivot-row (loop [r row]
                          (cond
                            (>= r n-lights) -1
                            (bit-test (aget rows r) col) r
                            :else (recur (inc r))))]
          (if (neg? pivot-row)
            (recur pivots row (inc col))
            (do
              (when (not= pivot-row row)
                (let [tmp (aget rows row)]
                  (aset rows row (aget rows pivot-row))
                  (aset rows pivot-row tmp))
                (let [tmp (aget tgt row)]
                  (aset tgt row (aget tgt pivot-row))
                  (aset tgt pivot-row tmp)))
              (dotimes [r n-lights]
                (when (and (not= r row) (bit-test (aget rows r) col))
                  (aset rows r (bit-xor (aget rows r) (aget rows row)))
                  (aset tgt r (bit-xor (aget tgt r) (aget tgt row)))))
              (recur (conj! pivots col) (inc row) (inc col)))))))))

(defn- gf2-kernel-basis
  "Compute kernel basis vectors from RREF."
  [{:keys [rows pivots]} n]
  (let [pivot-set (set pivots)
        free-vars (filterv #(not (contains? pivot-set %)) (range n))
        pivot-row (into {} (map-indexed (fn [i p] [p i]) pivots))]
    (mapv (fn [f]
            (reduce (fn [v [p row-idx]]
                      (if (bit-test (rows row-idx) f)
                        (bit-set v p)
                        v))
                    (bit-set 0 f)
                    pivot-row))
          free-vars)))

(defn- gf2-particular-solution
  "Find particular solution from RREF, or nil if inconsistent."
  [{:keys [rows target pivots]} n]
  (let [n-rows (count rows)
        n-pivots (count pivots)]
    (when-not (some (fn [i]
                      (and (>= i n-pivots)
                           (zero? (rows i))
                           (= 1 (target i))))
                    (range n-rows))
      (reduce (fn [sol [row-idx pivot-col]]
                (if (= 1 (target row-idx))
                  (bit-set sol pivot-col)
                  sol))
              0
              (map-indexed vector pivots)))))

(defn- highest-bit [^long x]
  (if (zero? x) -1 (- 63 (Long/numberOfLeadingZeros x))))

(defn- min-presses-gf2
  "Find minimum button presses using GF(2) elimination."
  [{:keys [target buttons]}]
  (let [n (count buttons)
        max-btn-bit (reduce max -1 (map highest-bit buttons))
        max-tgt-bit (highest-bit target)
        n-lights (inc (max 0 max-btn-bit max-tgt-bit))
        mat (build-gf2-matrix buttons n-lights)
        rref (gf2-eliminate mat target n-lights n)
        particular (gf2-particular-solution rref n)]
    (if (nil? particular)
      Long/MAX_VALUE
      (let [kernel (gf2-kernel-basis rref n)
            k (count kernel)]
        (if (zero? k)
          (Long/bitCount particular)
          (loop [best (Long/bitCount particular)
                 mask 1]
            (if (> mask (bit-shift-left 1 k))
              best
              (let [sol (reduce-kv (fn [s i kv]
                                     (if (bit-test mask i)
                                       (bit-xor s kv)
                                       s))
                                   particular kernel)
                    cnt (Long/bitCount sol)]
                (recur (min best cnt) (inc mask))))))))))

;; ─────────────────────────────────────────────────────────────
;; Part 2: Integer Linear System with Optimized Search
;; ─────────────────────────────────────────────────────────────
;;
;; Solve Ax = b where A[j][i] = 1 if button i affects counter j
;; Find x >= 0 minimizing sum(x)
;; Uses ratio Gaussian elimination with tight bounds and gradient-based search

(defn- build-ratio-matrix
  "Build augmented matrix [A|b] using ratios for numerical stability."
  [button-indices joltage]
  (let [m (count joltage)
        n (count button-indices)]
    (vec (for [j (range m)]
           (vec (concat
                 (for [i (range n)]
                   (if (contains? (button-indices i) j) 1 0))
                 [(joltage j)]))))))

(defn- ratio-eliminate
  "Gaussian elimination with ratios. Returns {:matrix rref :pivots pivot-cols :pivot-rows map}."
  [matrix]
  (let [m (count matrix)
        n (dec (count (first matrix)))]
    (loop [mat (mapv #(mapv identity %) matrix)
           row 0
           col 0
           pivots []
           pivot-rows {}]
      (if (or (>= row m) (>= col n))
        {:matrix mat :pivots pivots :pivot-rows pivot-rows}
        (let [pivot-row (first (filter #(not (zero? (get-in mat [% col]))) (range row m)))]
          (if (nil? pivot-row)
            (recur mat row (inc col) pivots pivot-rows)
            (let [mat (if (not= pivot-row row)
                        (assoc mat row (mat pivot-row) pivot-row (mat row))
                        mat)
                  pivot-val (get-in mat [row col])
                  mat (update mat row (fn [r] (mapv #(/ % pivot-val) r)))
                  mat (reduce (fn [m r]
                                (if (= r row)
                                  m
                                  (let [factor (get-in m [r col])]
                                    (update m r (fn [rv] (mapv #(- %1 (* factor %2)) rv (m row)))))))
                              mat (range m))]
              (recur mat (inc row) (inc col) (conj pivots col) (assoc pivot-rows col row)))))))))

(defn- int-val
  "If x is an integer or integer-valued ratio that fits in a long, return it. Otherwise nil."
  [x]
  (try
    (cond
      (integer? x) (long x)
      (and (ratio? x) (= 1 (denominator x)))
      (let [n (numerator x)]
        (if (instance? clojure.lang.BigInt n)
          (when (<= Long/MIN_VALUE n Long/MAX_VALUE) (long n))
          (long n)))
      :else nil)
    (catch Exception _ nil)))

(defn- solve-unique
  "Extract unique solution if no free vars. Returns sum or nil."
  [{:keys [matrix pivots]} n]
  (when (= (count pivots) n)
    (reduce (fn [sum [row-idx pivot-col]]
              (let [v (last (matrix row-idx))
                    lv (int-val v)]
                (if (and lv (>= lv 0))
                  (+ sum lv)
                  (reduced nil))))
            0 (map-indexed vector pivots))))

(defn- precompute-coeffs
  "Pre-extract coefficients from ratio matrix as doubles for fast computation."
  [matrix pivot-rows free-indices]
  (let [n-pivots (count pivot-rows)
        n-free (count free-indices)
        ;; For each pivot row: [rhs coef0 coef1 ... coefn-1]
        data (double-array (* n-pivots (inc n-free)))]
    (reduce
     (fn [^long offset [_ row-idx]]
       (let [row (matrix row-idx)]
         (aset data offset (double (last row)))
         (dotimes [i n-free]
           (aset data (+ offset 1 i) (double (row (free-indices i))))))
       (+ offset (inc n-free)))
     0 pivot-rows)
    data))

(defn- compute-total-fast
  "Fast computation using precomputed double coefficients."
  [^doubles coeffs n-pivots n-free ^longs x free-indices]
  (let [n-pivots (long n-pivots)
        n-free (long n-free)
        row-size (inc n-free)]
    (loop [i 0
           offset 0
           total 0]
      (if (>= i n-pivots)
        (+ total (areduce x j acc 0 (+ acc (aget x j))))
        (let [rhs (aget coeffs offset)
              contrib (loop [j 0 c 0.0]
                        (if (>= j n-free)
                          c
                          (recur (inc j) (+ c (* (aget coeffs (+ offset 1 j))
                                                 (double (aget x (free-indices j))))))))
              v (- rhs contrib)
              lv (long (Math/round v))]
          (if (and (>= lv 0) (< (Math/abs (- v (double lv))) 0.0001))
            (recur (inc i) (+ offset row-size) (+ total lv))
            -1))))))

(defn- search-free-vars
  "Search for minimum solution with free variables using branch and bound."
  [{:keys [matrix pivots pivot-rows]} n max-target]
  (let [pivot-set (set pivots)
        free-indices (vec (remove pivot-set (range n)))
        n-free (long (count free-indices))
        n-pivots (long (count pivot-rows))
        coeffs (precompute-coeffs matrix pivot-rows free-indices)
        best (volatile! Long/MAX_VALUE)]
    (letfn [(search [^long idx ^longs x ^long sum]
              (if (>= idx n-free)
                (let [total (compute-total-fast coeffs n-pivots n-free x free-indices)]
                  (when (and (>= total 0) (< total @best))
                    (vreset! best total)))
                (let [fi (free-indices idx)
                      max-v (min max-target (max 0 (- @best sum 1)))]
                  (loop [v 0]
                    (when (<= v max-v)
                      (aset x fi v)
                      (search (inc idx) x (+ sum v))
                      (recur (inc v)))))))]
      (let [x (long-array n 0)]
        (search 0 x 0)))
    @best))

(defn- min-joltage-presses
  "Find minimum button presses to reach joltage targets."
  [{:keys [button-indices joltage]}]
  (let [n (count button-indices)
        matrix (build-ratio-matrix button-indices joltage)
        rref (ratio-eliminate matrix)
        sol (solve-unique rref n)]
    (or sol (search-free-vars rref n (long (apply max joltage))))))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn part1
  "Sum of minimum button presses for all machines (XOR mode)."
  [input]
  (transduce (map min-presses-gf2) + (parse input)))

(defn part2
  "Sum of minimum button presses for all machines (joltage mode)."
  [input]
  (reduce + 0 (pmap min-joltage-presses (parse input))))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-part1-example
  (is (= 7 (part1 example))))

(deftest test-part2-example
  (is (= 33 (part2 example))))

(deftest test-part1-individual
  (is (= 2 (min-presses-gf2 (parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))))
  (is (= 3 (min-presses-gf2 (parse-line "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"))))
  (is (= 2 (min-presses-gf2 (parse-line "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")))))

(deftest test-part2-individual
  (is (= 10 (min-joltage-presses (parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))))
  (is (= 12 (min-joltage-presses (parse-line "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"))))
  (is (= 11 (min-joltage-presses (parse-line "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")))))
