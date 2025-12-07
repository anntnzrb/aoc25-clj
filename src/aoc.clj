(ns aoc
  "Advent of Code 2025 solutions."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [run-tests]]
   [clj-http.client :as http]))

(def year
  "Advent of Code year."
  2025)

(def aoc-url
  "Base URL for Advent of Code."
  "https://adventofcode.com")

(defn read-env
  "Parses a .env file into a map of key-value pairs. Returns nil if file doesn't exist."
  [file]
  (when (.exists file)
    (->> (slurp file)
         str/split-lines
         (remove #(or (str/blank? %) (str/starts-with? % "#")))
         (map #(str/split % #"=" 2))
         (filter #(= 2 (count %)))
         (into {}))))

(defn get-session
  "Returns AOC_SESSION from .env. Throws if not found."
  []
  (let [env (read-env (io/file ".env"))
        session (get env "AOC_SESSION")]
    (when-not session
      (throw (ex-info "AOC_SESSION not found. Add it to .env" {:type :missing-session})))
    session))

(defn input-path
  "Returns the filesystem path for a day's input file."
  [day]
  (str "src/" (format "day%02d" day) ".in"))

(defn input-exists?
  "Returns true if the input file for the given day exists."
  [day]
  (.exists (io/file (input-path day))))

(defn fetch-input
  "Fetches puzzle input from AoC API. Requires AOC_SESSION in env."
  [day]
  (let [url (format "%s/%d/day/%d/input" aoc-url year day)
        response (http/get url {:headers {"Cookie" (str "session=" (get-session))
                                          "User-Agent" "aoc25-clj"}
                                :throw-exceptions false})]
    (if (= 200 (:status response))
      (str/trim-newline (:body response))
      (throw (ex-info (str "Fetch failed: " (:status response)) {:body (:body response)})))))

(defn get-input
  "Gets input for day, fetching from AoC if not cached. Use :force to re-fetch."
  [day & {:keys [force]}]
  (let [path (input-path day)]
    (if (and (input-exists? day) (not force))
      (println (str "Cached: " path))
      (do
        (println (str "Fetching day " day "..."))
        (let [content (fetch-input day)]
          (spit path content)
          (println (str "Saved: " path)))))))

(def usage
  "
Usage: clj -M:run [options] [days...]

Run Advent of Code 2025 solutions.

Arguments:
  days    Day number(s) to run (1-25). Supports:
          - Single day: 1
          - Multiple days: 1 3 5
          - Range: 1-5
          - Mixed: 1 3-5 7

Options:
  -h, --help       Show this help
  -l, --list       List available days
  -a, --all        Run all available days
  -f, --fetch      Fetch input before running (auto-fetches if missing)
  --fetch-only     Only fetch inputs, don't run solutions

Examples:
  clj -M:run               Run all available days
  clj -M:run 1             Run day 1
  clj -M:run 1 2 3         Run days 1, 2, and 3
  clj -M:run 1-5           Run days 1 through 5
  clj -M:run --list        Show available days
  clj -M:run --fetch 1     Fetch and run day 1
  clj -M:run --fetch-only  Fetch all inputs without running

Setup (for fetching):
  1. Get session cookie from adventofcode.com (DevTools > Application > Cookies)
  2. Add to .env:  AOC_SESSION=your-cookie-value
")

(defn day-path
  "Returns the path to a day's solution file."
  [day]
  (str "src/day" (format "%02d" day) ".clj"))

(defn day-exists?
  "Returns true if a solution file exists for the given day."
  [day]
  (.exists (java.io.File. (day-path day))))

(defn available-days
  "Returns a seq of day numbers (1-25) that have solution files."
  []
  (filter day-exists? (range 1 26)))

(defn valid-day?
  "Returns true if day is an integer between 1 and 25."
  [day]
  (and (integer? day) (<= 1 day 25)))

;; Expected answers for regression tests
(def expected-answers
  "Map of day number to [part1-answer part2-answer]."
  {1 [1182 6907]
   2 [8576933996 25663320831]
   3 [17074 169512729575727]
   4 [1437 8765]
   5 [773 332067203034711]
   6 [4583860641327 11602774058280]
   7 [1687 390684413472684]})

(defn ensure-input
  "Fetches input for day if it doesn't exist. Returns true on success."
  [day]
  (when-not (input-exists? day)
    (try
      (println (str "Input missing for day " day ", fetching..."))
      (get-input day)
      true
      (catch Exception e
        (println (str "Warning: Could not fetch input: " (.getMessage e)))
        false))))

(defn run-day
  "Loads and runs a single day's solution. Returns true on success."
  [day & {:keys [fetch] :or {fetch false}}]
  (cond
    (not (valid-day? day))
    (do (println (str "Error: Invalid day " day " (must be 1-25)"))
        false)

    (not (day-exists? day))
    (do (println (str "Error: Day " day " not found"))
        false)

    :else
    (do
      (when fetch
        (ensure-input day))
      (let [ns-sym (symbol (str "day" (format "%02d" day)))
            file-path (day-path day)]
        ;; Load file directly
        (load-file file-path)
        (let [ns-obj (find-ns ns-sym)
              part1-fn (ns-resolve ns-obj 'part1)
              part2-fn (ns-resolve ns-obj 'part2)
              results (run-tests ns-sym)]
          (when (zero? (+ (:fail results) (:error results)))
            (println "\n✓ Tests pass!")
            (when (input-exists? day)
              (let [input (slurp (input-path day))]
                (println "Part 1:" (part1-fn input))
                (println "Part 2:" (part2-fn input)))))
          (zero? (+ (:fail results) (:error results))))))))

(defn run-days
  "Runs multiple days' solutions. Returns true if all succeed."
  [days & {:keys [fetch] :or {fetch false}}]
  (let [results (for [day days]
                  (do
                    (when (> (count days) 1)
                      (println (str "\n═══ Day " day " ═══")))
                    (run-day day :fetch fetch)))]
    (every? true? results)))

(defn parse-day-arg
  "Parses a CLI arg like '1' or '1-5' into a seq of day numbers."
  [arg]
  (if-let [[_ start end] (re-matches #"(\d+)-(\d+)" arg)]
    (range (parse-long start) (inc (parse-long end)))
    (when-let [n (parse-long arg)]
      [n])))

(defn parse-args
  "Parses CLI args into a sorted seq of unique day numbers."
  [args]
  (->> args
       (mapcat parse-day-arg)
       (filter some?)
       distinct
       sort))

(defn list-days
  "Prints all available days to stdout."
  []
  (let [days (available-days)]
    (if (seq days)
      (do
        (println "Available days:")
        (doseq [day days]
          (println (str "  Day " day))))
      (println "No days available yet."))))

(defn -main
  "CLI entry point. Parses args and runs appropriate commands."
  [& args]
  (let [fetch? (some #{"-f" "--fetch"} args)
        fetch-only? (some #{"--fetch-only"} args)
        force? (some #{"--force"} args)
        filtered-args (remove #{"-f" "--fetch" "--fetch-only" "--force"} args)]
    (cond
      (some #{"-h" "--help"} args)
      (println usage)

      (some #{"-l" "--list"} args)
      (list-days)

      fetch-only?
      (let [days (if (seq (parse-args filtered-args))
                   (parse-args filtered-args)
                   (available-days))]
        (doseq [day days]
          (try
            (get-input day :force force?)
            (catch Exception e
              (println (str "Failed to fetch day " day ": " (.getMessage e)))))))

      (or (empty? filtered-args) (some #{"-a" "--all"} filtered-args))
      (let [days (available-days)]
        (if (seq days)
          (when-not (run-days days :fetch fetch?)
            (System/exit 1))
          (println "No days available yet.")))

      :else
      (let [days (parse-args filtered-args)]
        (if (seq days)
          (when-not (run-days days :fetch fetch?)
            (System/exit 1))
          (do
            (println "Error: No valid days specified")
            (println "Run with --help for usage")
            (System/exit 1)))))))
