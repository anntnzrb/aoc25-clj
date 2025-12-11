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

(defn- parse-flat
  "Parse input into flat long array [x0 y0 z0 x1 y1 z1 ...]."
  ^longs [input]
  (let [lines (str/split-lines input)
        n (count lines)
        arr (long-array (* 3 n))]
    (dotimes [i n]
      (let [[x y z] (mapv parse-long (str/split (lines i) #","))
            idx (* 3 i)]
        (aset arr idx (long x))
        (aset arr (+ idx 1) (long y))
        (aset arr (+ idx 2) (long z))))
    arr))

(defn- distance-sq-flat
  "Distance squared from flat array."
  ^long [^longs arr ^long i ^long j]
  (let [idx-i (unchecked-multiply 3 i)
        idx-j (unchecked-multiply 3 j)
        dx (unchecked-subtract (aget arr idx-j) (aget arr idx-i))
        dy (unchecked-subtract (aget arr (unchecked-inc idx-j)) (aget arr (unchecked-inc idx-i)))
        dz (unchecked-subtract (aget arr (unchecked-add idx-j 2)) (aget arr (unchecked-add idx-i 2)))]
    (unchecked-add (unchecked-multiply dx dx)
                   (unchecked-add (unchecked-multiply dy dy)
                                  (unchecked-multiply dz dz)))))

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
  ^long [[x1 y1 z1] [x2 y2 z2]]
  (+ (* (- x2 x1) (- x2 x1))
     (* (- y2 y1) (- y2 y1))
     (* (- z2 z1) (- z2 z1))))

(defn- distance-sq-idx
  "Squared distance between points at indices i and j in points vector."
  ^long [points ^long i ^long j]
  (let [[x1 y1 z1] (points i)
        [x2 y2 z2] (points j)]
    (+ (* (- x2 x1) (- x2 x1))
       (* (- y2 y1) (- y2 y1))
       (* (- z2 z1) (- z2 z1)))))

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

;; ─────────────────────────────────────────────────────────────
;; Optimized Part 1: Flat array pairs + partial sort
;; ─────────────────────────────────────────────────────────────

(defn- build-pairs-flat
  "Build flat array of pairs: [dist i j dist i j ...]."
  [^longs coords ^long n]
  (let [num-pairs (quot (unchecked-multiply n (unchecked-dec n)) 2)
        arr (long-array (unchecked-multiply 3 num-pairs))
        counter (long-array 1)]
    (dotimes [i n]
      (let [li (long i)]
        (dotimes [j i]
          (let [k (aget counter 0)
                d (distance-sq-flat coords li (long j))]
            (aset arr k d)
            (aset arr (unchecked-inc k) li)
            (aset arr (unchecked-add k 2) (long j))
            (aset counter 0 (unchecked-add k 3))))))
    arr))

(defn- swap-triple!
  "Swap two triples at indices i and j (each triple is 3 longs)."
  [^longs arr ^long i ^long j]
  (let [i3 (unchecked-multiply 3 i)
        j3 (unchecked-multiply 3 j)
        t0 (aget arr i3)
        t1 (aget arr (unchecked-inc i3))
        t2 (aget arr (unchecked-add i3 2))]
    (aset arr i3 (aget arr j3))
    (aset arr (unchecked-inc i3) (aget arr (unchecked-inc j3)))
    (aset arr (unchecked-add i3 2) (aget arr (unchecked-add j3 2)))
    (aset arr j3 t0)
    (aset arr (unchecked-inc j3) t1)
    (aset arr (unchecked-add j3 2) t2)))

(defn- partition-pairs!
  "Partition around pivot, returns final pivot position."
  ^long [^longs arr ^long lo ^long hi]
  (let [pivot (aget arr (unchecked-multiply 3 hi))]
    (loop [i lo, j lo]
      (if (== j hi)
        (do (swap-triple! arr i hi) i)
        (if (< (aget arr (unchecked-multiply 3 j)) pivot)
          (do (swap-triple! arr i j)
              (recur (unchecked-inc i) (unchecked-inc j)))
          (recur i (unchecked-inc j)))))))

(defn- quickselect-k!
  "Partially sort array so first k elements are the k smallest."
  [^longs arr ^long k ^long len]
  (loop [lo (long 0), hi (unchecked-dec len)]
    (when (< lo hi)
      (let [p (partition-pairs! arr lo hi)]
        (cond
          (== p k) nil
          (> p k) (recur lo (unchecked-dec p))
          :else (recur (unchecked-inc p) hi))))))

(defn- solve-flat
  "Connect k closest pairs using flat arrays. Returns product of 3 largest sizes."
  [^longs coords ^long n ^long k]
  (let [num-pairs (quot (unchecked-multiply n (unchecked-dec n)) 2)
        pairs (build-pairs-flat coords n)
        uf (make-uf n)]
    ;; Partially sort to get k smallest
    (quickselect-k! pairs k num-pairs)
    ;; Sort just the first k pairs by distance
    (let [first-k (long-array (unchecked-multiply 3 k))
          _ (System/arraycopy pairs 0 first-k 0 (unchecked-multiply 3 k))
          ;; Create index array as Integer[] for sorting
          indices (object-array (range k))]
      (java.util.Arrays/sort indices
                             (reify java.util.Comparator
                               (compare [_ a b]
                                 (Long/compare (aget first-k (unchecked-multiply 3 (int a)))
                                               (aget first-k (unchecked-multiply 3 (int b)))))))
      ;; Union in order
      (doseq [i indices]
        (let [base (unchecked-multiply 3 (int i))]
          (union! uf (aget first-k (unchecked-inc base)) (aget first-k (unchecked-add base 2))))))
    (->> (component-sizes uf n)
         (sort >)
         (take 3)
         (reduce *))))

;; ─────────────────────────────────────────────────────────────
;; Optimized Part 2: Prim's MST - O(n²) without sorting
;; ─────────────────────────────────────────────────────────────

(defn- prim-last-edge
  "Find the maximum-weight edge in MST using Prim's algorithm.
   Returns [i j] for the last edge needed to connect all vertices."
  [^longs coords ^long n]
  (let [in-tree (boolean-array n)
        min-dist (long-array n Long/MAX_VALUE)
        parent (int-array n -1)]
    (aset in-tree 0 true)
    ;; Initialize distances from node 0
    (loop [i (long 1)]
      (when (< i n)
        (aset min-dist i (distance-sq-flat coords 0 i))
        (aset parent i 0)
        (recur (unchecked-inc i))))
    ;; Build MST, tracking max edge
    (loop [edges-added (long 0)
           max-dist (long 0)
           max-i (long 0)
           max-j (long 0)]
      (if (== edges-added (unchecked-dec n))
        [max-i max-j]
        ;; Find minimum distance vertex not in tree
        (let [min-v (loop [v (long 1), best-v (long -1), best-d Long/MAX_VALUE]
                      (if (== v n)
                        best-v
                        (let [d (aget min-dist v)]
                          (if (and (not (aget in-tree v)) (< d best-d))
                            (recur (unchecked-inc v) v d)
                            (recur (unchecked-inc v) best-v best-d)))))
              min-d (aget min-dist min-v)
              min-parent (long (aget parent min-v))]
          (aset in-tree min-v true)
          ;; Update distances to remaining vertices (inlined for speed)
          (let [base (unchecked-multiply 3 min-v)
                x1 (aget coords base)
                y1 (aget coords (unchecked-inc base))
                z1 (aget coords (unchecked-add base 2))]
            (loop [v (long 0)]
              (when (< v n)
                (when-not (aget in-tree v)
                  (let [bv (unchecked-multiply 3 v)
                        dx (unchecked-subtract (aget coords bv) x1)
                        dy (unchecked-subtract (aget coords (unchecked-inc bv)) y1)
                        dz (unchecked-subtract (aget coords (unchecked-add bv 2)) z1)
                        d (unchecked-add (unchecked-multiply dx dx)
                                         (unchecked-add (unchecked-multiply dy dy)
                                                        (unchecked-multiply dz dz)))]
                    (when (< d (aget min-dist v))
                      (aset min-dist v d)
                      (aset parent v (int min-v)))))
                (recur (unchecked-inc v)))))
          ;; Track max edge
          (if (> min-d max-dist)
            (recur (unchecked-inc edges-added) min-d min-parent min-v)
            (recur (unchecked-inc edges-added) max-dist max-i max-j)))))))

(defn part1
  "Connect 1000 closest pairs, multiply 3 largest circuit sizes."
  [input]
  (let [coords (parse-flat input)
        n (quot (alength coords) 3)]
    (solve-flat coords n 1000)))

(defn part2
  "Find last merge to form single circuit, return product of X coordinates."
  [input]
  (let [coords (parse-flat input)
        n (quot (alength coords) 3)
        [i j] (prim-last-edge coords n)]
    (* (aget coords (* 3 i)) (aget coords (* 3 j)))))

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
