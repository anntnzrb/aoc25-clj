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
;; Union-Find
;; ─────────────────────────────────────────────────────────────

(defn- make-uf
  "Create Union-Find structure for n elements."
  [n]
  {:parent (vec (range n))
   :rank (vec (repeat n 0))})

(defn- find-root
  "Find root of element with path compression. Returns [uf root]."
  [{:keys [parent] :as uf} x]
  (let [p (parent x)]
    (if (= p x)
      [uf x]
      (let [[uf' root] (find-root uf p)]
        [(assoc-in uf' [:parent x] root) root]))))

(defn- union
  "Union two sets. Returns [uf merged?]."
  [uf a b]
  (let [[uf' ra] (find-root uf a)
        [uf'' rb] (find-root uf' b)]
    (if (= ra rb)
      [uf'' false]
      (let [rank-a (get-in uf'' [:rank ra])
            rank-b (get-in uf'' [:rank rb])
            uf''' (cond
                    (< rank-a rank-b)
                    (assoc-in uf'' [:parent ra] rb)

                    (> rank-a rank-b)
                    (assoc-in uf'' [:parent rb] ra)

                    :else
                    (-> uf''
                        (assoc-in [:parent rb] ra)
                        (update-in [:rank ra] inc)))]
        [uf''' true]))))

(defn- component-sizes
  "Get sizes of all components."
  [uf n]
  (let [uf-final (reduce (fn [u i]
                           (first (find-root u i)))
                         uf
                         (range n))]
    (->> (range n)
         (map #(get-in uf-final [:parent %]))
         frequencies
         vals)))

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
  (let [n (count points)]
    (sort-by #(nth % 2)
             (for [i (range n)
                   j (range (inc i) n)]
               [i j (distance-sq (points i) (points j))]))))

(defn- solve
  "Connect k closest pairs and return product of 3 largest circuit sizes."
  [input k]
  (let [points (parse input)
        n (count points)
        pairs (all-pairs points)
        uf (reduce (fn [u [i j _]] (first (union u i j)))
                   (make-uf n)
                   (take k pairs))
        sizes (->> (component-sizes uf n)
                   (sort >)
                   (take 3))]
    (reduce * sizes)))

(defn- find-last-merge
  "Find the last pair that merges two separate circuits. Returns [i j]."
  [input]
  (let [points (parse input)
        n (count points)
        pairs (all-pairs points)]
    (loop [uf (make-uf n)
           remaining pairs
           components n
           last-merge nil]
      (if (or (= components 1) (empty? remaining))
        last-merge
        (let [[i j _] (first remaining)
              [uf' merged?] (union uf i j)]
          (recur uf'
                 (rest remaining)
                 (if merged? (dec components) components)
                 (if merged? [i j] last-merge)))))))

(defn part1
  "Connect 1000 closest pairs, multiply 3 largest circuit sizes."
  [input]
  (solve input 1000))

(defn part2
  "Find last merge to form single circuit, return product of X coordinates."
  [input]
  (let [points (parse input)
        [i j] (find-last-merge input)]
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
  (is (= 40 (solve example 10))))

(deftest test-part2-example
  (is (= 25272 (part2 example))))
