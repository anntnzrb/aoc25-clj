(ns aoc
  "Advent of Code 2025 solutions."
  (:require
   [babashka.cli :as cli]
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

(def cli-spec
  "CLI options specification for babashka/cli."
  {:help       {:alias :h :desc "Show this help"}
   :list       {:alias :l :desc "List available days"}
   :all        {:alias :a :desc "Run all available days"}
   :fetch      {:alias :f :desc "Fetch input before running"}
   :fetch-only {:desc "Only fetch inputs, don't run solutions"}
   :force      {:desc "Force re-fetch even if cached"}})

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
   7 [1687 390684413472684]
   8 [244188 8361881885]
   9 [4760959496 1343576598]
   10 [512 19857]})

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

(defn- load-day-ns
  "Loads day namespace and returns {:ns :part1 :part2}."
  [day]
  (let [ns-sym (symbol (str "day" (format "%02d" day)))]
    (load-file (day-path day))
    (let [ns-obj (find-ns ns-sym)]
      {:ns ns-sym
       :part1 (ns-resolve ns-obj 'part1)
       :part2 (ns-resolve ns-obj 'part2)})))

(defn- run-day-solutions
  "Runs tests and solutions for loaded day. Returns true if tests pass."
  [{:keys [ns part1 part2]} day]
  (let [results (run-tests ns)
        passed? (zero? (+ (:fail results) (:error results)))]
    (when passed?
      (println "\n✓ Tests pass!")
      (when (input-exists? day)
        (let [input (slurp (input-path day))]
          (println "Part 1:" (part1 input))
          (println "Part 2:" (part2 input)))))
    passed?))

(defn run-day
  "Loads and runs a single day's solution. Returns true on success."
  [day & {:keys [fetch] :or {fetch false}}]
  (cond
    (not (valid-day? day))
    (do (println (str "Error: Invalid day " day " (must be 1-25)")) false)

    (not (day-exists? day))
    (do (println (str "Error: Day " day " not found")) false)

    :else
    (do
      (when fetch (ensure-input day))
      (run-day-solutions (load-day-ns day) day))))

(defn run-days
  "Runs multiple days' solutions. Returns true if all succeed."
  [days & {:keys [fetch] :or {fetch false}}]
  (let [results (for [day days]
                  (do
                    (when (> (count days) 1)
                      (println (str "\n═══ Day " day " ═══")))
                    (run-day day :fetch fetch)))]
    (every? true? results)))

(defn- parse-day-arg
  "Parses a CLI arg like '1' or '1-5' into a seq of day numbers."
  [arg]
  (if-let [[_ start end] (re-matches #"(\d+)-(\d+)" arg)]
    (range (parse-long start) (inc (parse-long end)))
    (when-let [n (parse-long arg)]
      [n])))

(defn- parse-days
  "Parses day args into a sorted seq of unique day numbers."
  [args]
  (->> args
       (mapcat parse-day-arg)
       (filter some?)
       distinct
       sort))

(defn- print-help
  "Prints help message."
  []
  (println
   (str "Usage: clj -M:run [options] [days...]

Run Advent of Code 2025 solutions.

Days can be: 1, 1 2 3, 1-5, or mixed (1 3-5 7)

Options:
" (cli/format-opts {:spec cli-spec}) "

Setup: Add AOC_SESSION=<cookie> to .env")))

(defn- list-days
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
  "CLI entry point."
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})
        {:keys [help list all fetch fetch-only force]} opts
        days (parse-days args)]
    (cond
      help
      (print-help)

      list
      (list-days)

      fetch-only
      (doseq [day (if (seq days) days (available-days))]
        (try
          (get-input day :force force)
          (catch Exception e
            (println (str "Failed to fetch day " day ": " (.getMessage e))))))

      :else
      (let [days-to-run (if (or all (empty? days)) (available-days) days)]
        (if (seq days-to-run)
          (when-not (run-days days-to-run :fetch fetch)
            (System/exit 1))
          (println "No days available yet."))))))
