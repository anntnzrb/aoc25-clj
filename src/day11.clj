(ns day11
  "Day 11: Reactor - Count paths through a device network."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; ─────────────────────────────────────────────────────────────
;; Domain
;; ─────────────────────────────────────────────────────────────

;; Directed graph of devices: each line "node: dest1 dest2 ..." defines edges
;; Part 1: Count all paths from 'you' to 'out'
;; Part 2: Count paths from 'svr' to 'out' visiting both 'dac' and 'fft'
;;         Decompose into: (svr→fft→dac→out) + (svr→dac→fft→out)
;;         Product of segment path counts since paths are independent

;; ─────────────────────────────────────────────────────────────
;; Parsing
;; ─────────────────────────────────────────────────────────────

(def example1
  "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(def example2
  "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defn- parse-line
  "Parse 'node: dest1 dest2 ...' into [node [dest1 dest2 ...]]."
  [line]
  (let [[src & dests] (str/split line #"[:\s]+")]
    [src (vec dests)]))

(defn- parse
  "Parse input into adjacency map {node [destinations]}."
  [input]
  (into {} (map parse-line) (str/split-lines input)))

;; ─────────────────────────────────────────────────────────────
;; Part 1: Count all paths from 'you' to 'out' using memoized DFS
;; ─────────────────────────────────────────────────────────────

(defn- count-paths
  "Count all paths from start to target using memoization."
  [graph start target]
  (let [memo (volatile! {})]
    (letfn [(dfs [node]
              (cond
                (= node target) 1
                (contains? @memo node) (@memo node)
                :else
                (let [neighbors (get graph node [])
                      cnt (reduce + 0 (map dfs neighbors))]
                  (vswap! memo assoc node cnt)
                  cnt)))]
      (dfs start))))

(defn part1
  "Count all distinct paths from 'you' to 'out'."
  [input]
  (count-paths (parse input) "you" "out"))

;; ─────────────────────────────────────────────────────────────
;; Part 2: Count paths svr→out visiting both dac and fft
;; ─────────────────────────────────────────────────────────────

(defn part2
  "Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'."
  [input]
  (let [graph (parse input)
        ;; fft first: svr → fft → dac → out
        fft-first (* (count-paths graph "svr" "fft")
                     (count-paths graph "fft" "dac")
                     (count-paths graph "dac" "out"))
        ;; dac first: svr → dac → fft → out
        dac-first (* (count-paths graph "svr" "dac")
                     (count-paths graph "dac" "fft")
                     (count-paths graph "fft" "out"))]
    (+ fft-first dac-first)))

;; ─────────────────────────────────────────────────────────────
;; Tests
;; ─────────────────────────────────────────────────────────────

(deftest test-part1-example
  (is (= 5 (part1 example1))))

(deftest test-part2-example
  (is (= 2 (part2 example2))))
