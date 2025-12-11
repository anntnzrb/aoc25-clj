(ns day10
  "Day 10: Factory - Toggle indicator lights and configure joltage."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Part 1: Toggle lights (XOR) - find min presses for target pattern
;; Part 2: Increment counters - solve Ax=b with x>=0 integers, minimize sum(x)

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn- parse-target
  "Parse [.##.] into target bitmask (# = ON = 1)."
  [s]
  (let [pattern (subs s 1 (dec (count s)))]
    (reduce-kv (fn [mask i ch]
                 (if (= ch \#) (bit-set mask i) mask))
               0 (vec pattern))))

(defn- parse-button
  "Parse (0,2,3) into bitmask of toggled lights."
  [s]
  (let [nums (re-seq #"\d+" s)]
    (reduce (fn [mask n] (bit-set mask (parse-long n))) 0 nums)))

(defn- parse-button-indices
  "Parse (0,2,3) into set of counter indices."
  [s]
  (set (map parse-long (re-seq #"\d+" s))))

(defn- parse-joltage
  "Parse {3,5,4,7} into vector of target values."
  [s]
  (mapv parse-long (re-seq #"\d+" s)))

(defn- parse-line
  "Parse a machine line into full structure."
  [line]
  (let [parts (str/split line #"\s+")
        target-str (first parts)
        button-strs (take-while #(str/starts-with? % "(") (rest parts))
        joltage-str (last parts)]
    {:target (parse-target target-str)
     :buttons (mapv parse-button button-strs)
     :button-indices (mapv parse-button-indices button-strs)
     :joltage (parse-joltage joltage-str)}))

(defn- parse
  "Parse input into seq of machines."
  [input]
  (mapv parse-line (str/split-lines input)))

;; ─────────────────────────────────────────────────────────────
;; Part 1: XOR Solving (brute force)
;; ─────────────────────────────────────────────────────────────

(defn- apply-buttons
  "XOR all buttons selected by bitmask to get resulting light pattern."
  [buttons selection]
  (reduce-kv (fn [state i btn]
               (if (bit-test selection i) (bit-xor state btn) state))
             0 buttons))

(defn- min-presses-brute
  "Find minimum button presses via brute force (for small button counts)."
  [{:keys [target buttons]}]
  (let [n (count buttons)]
    (loop [best Long/MAX_VALUE
           sel 0]
      (if (>= sel (bit-shift-left 1 n))
        (if (= best Long/MAX_VALUE) nil best)
        (let [presses (Long/bitCount sel)]
          (if (and (< presses best)
                   (= target (apply-buttons buttons sel)))
            (recur presses (inc sel))
            (recur best (inc sel))))))))

;; ─────────────────────────────────────────────────────────────
;; Part 2: Linear System Solving (Gaussian elimination with rationals)
;; ─────────────────────────────────────────────────────────────

(defn- build-augmented-matrix
  "Build augmented matrix [A|b] for system Ax=b using ratios."
  [button-indices joltage]
  (let [m (count joltage)
        n (count button-indices)]
    (vec (for [j (range m)]
           (vec (concat
                  (for [i (range n)]
                    (if (contains? (button-indices i) j) 1 0))
                  [(joltage j)]))))))

(defn- swap-rows [mat r1 r2]
  (assoc mat r1 (mat r2) r2 (mat r1)))

(defn- gauss-jordan
  "Gauss-Jordan elimination to RREF. Returns {:matrix rref :pivots pivot-cols}."
  [matrix]
  (let [m (count matrix)
        n (dec (count (first matrix)))]
    (loop [mat (mapv #(mapv (fn [x] (/ x 1)) %) matrix) ; convert to ratios
           row 0
           col 0
           pivots []]
      (if (or (>= row m) (>= col n))
        {:matrix mat :pivots pivots}
        (let [pivot-row (->> (range row m)
                             (filter #(not (zero? (get-in mat [% col]))))
                             first)]
          (if (nil? pivot-row)
            (recur mat row (inc col) pivots)
            (let [mat (if (not= pivot-row row) (swap-rows mat pivot-row row) mat)
                  pivot-val (get-in mat [row col])
                  mat (update mat row (fn [r] (mapv #(/ % pivot-val) r)))
                  mat (reduce (fn [m r]
                                (if (= r row)
                                  m
                                  (let [factor (get-in m [r col])]
                                    (update m r (fn [rv]
                                                  (mapv #(- %1 (* factor %2)) rv (m row)))))))
                              mat (range m))]
              (recur mat (inc row) (inc col) (conj pivots col)))))))))

(defn- extract-unique-solution
  "Extract solution if unique (no free variables). Returns vector or nil."
  [{:keys [matrix pivots]} n]
  (let [pivot-set (set pivots)
        free-vars (remove pivot-set (range n))]
    (when (empty? free-vars)
      ;; Check consistency and extract solution
      (when-not (some (fn [row]
                        (and (every? zero? (butlast row))
                             (not (zero? (last row)))))
                      matrix)
        (let [sol (vec (repeat n 0))]
          (reduce (fn [s [row-idx col-idx]]
                    (assoc s col-idx (last (matrix row-idx))))
                  sol (map vector (range (count pivots)) pivots)))))))

(defn- compute-result
  "Compute Ax for given button presses x."
  [button-indices x]
  (let [m (apply max (mapcat identity button-indices))]
    (vec (for [j (range (inc m))]
           (reduce + (for [i (range (count x))
                           :when (contains? (button-indices i) j)]
                       (x i)))))))

(defn- search-with-free-vars
  "Search for minimum solution when free variables exist."
  [{:keys [matrix pivots]} button-indices joltage n]
  (let [pivot-set (set pivots)
        free-indices (vec (remove pivot-set (range n)))
        m (count joltage)
        max-val (apply max joltage)]
    ;; Search over free variable values
    (loop [best Long/MAX_VALUE
           free-vals (vec (repeat (count free-indices) 0))]
      (if (nil? free-vals)
        best
        (let [;; Compute pivot variables from free variables
              x (vec (repeat n 0))
              x (reduce-kv (fn [v i fi] (assoc v fi (free-vals i)))
                           x free-indices)
              ;; Solve for pivot vars: for each pivot row, x[pivot] = rhs - sum(coef*x[free])
              x (reduce
                  (fn [v [row-idx pivot-col]]
                    (let [row (matrix row-idx)
                          rhs (last row)
                          contrib (reduce + (for [fi free-indices]
                                              (* (row fi) (v fi))))]
                      (assoc v pivot-col (- rhs contrib))))
                  x (map vector (range (count pivots)) pivots))
              valid? (and (every? #(and (integer? %) (>= % 0)) x)
                          (= (take m (compute-result button-indices x)) joltage))
              total (reduce + x)
              best (if (and valid? (< total best)) total best)
              ;; Next combination of free vars
              next-free (loop [fv free-vals i 0]
                          (cond
                            (>= i (count free-indices)) nil
                            (< (fv i) max-val) (assoc fv i (inc (fv i)))
                            :else (recur (assoc fv i 0) (inc i))))]
          (recur best next-free))))))

(defn- min-joltage-presses
  "Find minimum button presses to reach joltage targets."
  [{:keys [button-indices joltage]}]
  (let [n (count button-indices)
        matrix (build-augmented-matrix button-indices joltage)
        rref (gauss-jordan matrix)
        sol (extract-unique-solution rref n)]
    (if (and sol (every? #(and (integer? %) (>= % 0)) sol))
      (reduce + sol)
      ;; Has free variables or no integer solution - search
      (search-with-free-vars rref button-indices joltage n))))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn part1
  "Sum of minimum button presses for all machines (XOR mode)."
  [input]
  (reduce + (map min-presses-brute (parse input))))

(defn part2
  "Sum of minimum button presses for all machines (joltage mode)."
  [input]
  (long (reduce + (map min-joltage-presses (parse input)))))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse-target
  (is (= 2r0110 (parse-target "[.##.]")))
  (is (= 2r01000 (parse-target "[...#.]")))
  (is (= 2r101110 (parse-target "[.###.#]"))))

(deftest test-parse-button
  (is (= 2r1000 (parse-button "(3)")))
  (is (= 2r1010 (parse-button "(1,3)")))
  (is (= 2r11101 (parse-button "(0,2,3,4)"))))

(deftest test-min-presses
  (is (= 2 (min-presses-brute (parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))))
  (is (= 3 (min-presses-brute (parse-line "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"))))
  (is (= 2 (min-presses-brute (parse-line "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")))))

(deftest test-part1-example
  (is (= 7 (part1 example))))

(deftest test-joltage-presses
  (is (= 10 (min-joltage-presses (parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))))
  (is (= 12 (min-joltage-presses (parse-line "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"))))
  (is (= 11 (min-joltage-presses (parse-line "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")))))

(deftest test-part2-example
  (is (= 33 (part2 example))))
