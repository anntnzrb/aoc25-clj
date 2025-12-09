(ns day08
  "Day 8: Playground - Connect junction boxes using Union-Find."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example
  "Example junction box positions from problem description."
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn- parse
  "Parse input into vector of [x y z] coordinates."
  [input]
  (mapv #(mapv parse-long (str/split % #",")) (str/split-lines input)))

;; ─────────────────────────────────────────────────────────────
;; Union-Find (mutable for performance)
;; ─────────────────────────────────────────────────────────────

(defn- make-uf
  "Create mutable Union-Find structure for n elements."
  [n]
  {:parent (int-array (range n))
   :rank (int-array n)})

(defn- find-root!
  "Find root of element with path compression. Mutates parent array."
  [{:keys [^ints parent]} x]
  (let [p (aget parent x)]
    (if (= p x)
      x
      (let [root (find-root! {:parent parent} p)]
        (aset parent x root)
        root))))

(defn- union!
  "Union two sets. Mutates uf. Returns true if merged."
  [{:keys [^ints parent ^ints rank] :as uf} a b]
  (let [ra (find-root! uf a)
        rb (find-root! uf b)]
    (if (= ra rb)
      false
      (let [rank-a (aget rank ra)
            rank-b (aget rank rb)]
        (cond
          (< rank-a rank-b)
          (aset parent ra rb)

          (> rank-a rank-b)
          (aset parent rb ra)

          :else
          (do (aset parent rb ra)
              (aset rank ra (inc rank-a))))
        true))))

(defn- component-sizes
  "Get sizes of all components."
  [{:keys [^ints parent] :as uf} n]
  ;; Ensure all paths are compressed
  (doseq [i (range n)]
    (find-root! uf i))
  (->> (range n)
       (map #(aget parent %))
       frequencies
       vals))

;; ─────────────────────────────────────────────────────────────
;; Solution
;; ─────────────────────────────────────────────────────────────

(defn- distance-sq
  "Squared Euclidean distance between two points (avoids sqrt)."
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (* (- x2 x1) (- x2 x1))
     (* (- y2 y1) (- y2 y1))
     (* (- z2 z1) (- z2 z1))))

(defn- all-pairs
  "Generate all pairs with distances, sorted by distance."
  [points]
  (let [n (count points)
        pairs (object-array
               (for [i (range n)
                     j (range (inc i) n)]
                 (long-array [i j (distance-sq (points i) (points j))])))]
    (java.util.Arrays/parallelSort
     pairs
     (reify java.util.Comparator
       (compare [_ a b]
         (Long/compare (aget ^longs a 2) (aget ^longs b 2)))))
    (vec pairs)))

(defn- solve
  "Connect k closest pairs and return product of 3 largest circuit sizes."
  [points pairs k]
  (let [n (count points)
        uf (make-uf n)]
    (doseq [^longs p (take k pairs)]
      (union! uf (aget p 0) (aget p 1)))
    (->> (component-sizes uf n)
         (sort >)
         (take 3)
         (reduce *))))

(defn- find-last-merge
  "Find the last pair that merges two separate circuits. Returns [i j]."
  [points pairs]
  (let [n (count points)
        uf (make-uf n)]
    (loop [remaining pairs
           components n
           last-merge nil]
      (if (or (= components 1) (empty? remaining))
        last-merge
        (let [^longs p (first remaining)
              i (aget p 0)
              j (aget p 1)
              merged? (union! uf (int i) (int j))]
          (recur (rest remaining)
                 (if merged? (dec components) components)
                 (if merged? [i j] last-merge)))))))

(defn part1
  "Connect 1000 closest pairs, multiply 3 largest circuit sizes."
  [input]
  (let [points (parse input)
        pairs (all-pairs points)]
    (solve points pairs 1000)))

(defn part2
  "Find last merge to form single circuit, return product of X coordinates."
  [input]
  (let [points (parse input)
        pairs (all-pairs points)
        [i j] (find-last-merge points pairs)]
    (* (first (points i)) (first (points j)))))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-parse
  (let [points (parse example)]
    (is (= 20 (count points)))
    (is (= [162 817 812] (first points)))
    (is (= [425 690 689] (last points)))))

(deftest test-part1-example
  (let [points (parse example)
        pairs (all-pairs points)]
    (is (= 40 (solve points pairs 10)))))

(deftest test-part2-example
  (is (= 25272 (part2 example))))
