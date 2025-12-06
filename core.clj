(require '[babashka.process :refer [shell]]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def babashka?
  "True if running in Babashka, false if JVM Clojure."
  (some? (System/getProperty "babashka.version")))

(if babashka?
  (require '[babashka.http-client :as http])
  (require '[clj-http.client :as http]))

(defn http-get
  "Performs HTTP GET, abstracting differences between BB and JVM HTTP clients."
  [url opts]
  (http/get url (if babashka?
                  opts
                  (assoc opts :throw-exceptions false))))

(def year "Advent of Code year." 2025)
(def aoc-url "Base URL for Advent of Code." "https://adventofcode.com")

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
  "Returns AOC_SESSION from ~/.env or .env. Throws if not found."
  []
  (let [home-env (read-env (io/file (System/getProperty "user.home") ".env"))
        local-env (read-env (io/file ".env"))
        session (or (get local-env "AOC_SESSION")
                    (get home-env "AOC_SESSION"))]
    (when-not session
      (throw (ex-info "AOC_SESSION not found. Add it to ~/.env or .env" {:type :missing-session})))
    session))

(defn input-path
  "Returns the filesystem path for a day's input file."
  [day]
  (str (format "day%02d" day) "/input.in"))

(defn input-exists?
  "Returns true if the input file for the given day exists."
  [day]
  (.exists (io/file (input-path day))))

(defn fetch-input
  "Fetches puzzle input from AoC API. Requires AOC_SESSION in env."
  [day]
  (let [url (format "%s/%d/day/%d/input" aoc-url year day)
        response (http-get url {:headers {"Cookie" (str "session=" (get-session))
                                          "User-Agent" "aoc25-clj"}})]
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

(def usage "
Usage: bb core.clj [options] [days...]

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
  bb core.clj              Run all available days
  bb core.clj 1            Run day 1
  bb core.clj 1 2 3        Run days 1, 2, and 3
  bb core.clj 1-5          Run days 1 through 5
  bb core.clj --list       Show available days
  bb core.clj --fetch 1    Fetch and run day 1
  bb core.clj --fetch-only Fetch all inputs without running

Setup (for fetching):
  1. Get session cookie from adventofcode.com (DevTools > Application > Cookies)
  2. Add to ~/.env or .env:  AOC_SESSION=your-cookie-value
")

(defn day-path
  "Returns the path to a day's solution file."
  [day]
  (str "day" (format "%02d" day) "/core.clj"))

(defn day-exists?
  "Returns true if a solution file exists for the given day."
  [day]
  (.exists (java.io.File. (day-path day))))

(defn available-days
  "Returns a seq of day numbers (1-25) that have solution files."
  []
  (->> (range 1 26)
       (filter day-exists?)))

(defn valid-day?
  "Returns true if day is an integer between 1 and 25."
  [day]
  (and (integer? day) (<= 1 day 25)))

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
  "Runs a single day's solution. Returns true on success."
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
      (let [{:keys [exit]} (shell {:dir (str "day" (format "%02d" day))
                                    :continue true}
                                   "bb" "core.clj")]
        (zero? exit)))))

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

(apply -main *command-line-args*)
