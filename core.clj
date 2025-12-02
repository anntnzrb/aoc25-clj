#!/usr/bin/env bb

(require '[babashka.process :refer [shell]]
         '[clojure.string :as str])

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
  -h, --help    Show this help
  -l, --list    List available days
  -a, --all     Run all available days

Examples:
  bb core.clj           Run all available days
  bb core.clj 1         Run day 1
  bb core.clj 1 2 3     Run days 1, 2, and 3
  bb core.clj 1-5       Run days 1 through 5
  bb core.clj --list    Show available days
")

(defn day-path [day]
  (str "day" (format "%02d" day) "/core.clj"))

(defn day-exists? [day]
  (.exists (java.io.File. (day-path day))))

(defn available-days []
  (->> (range 1 26)
       (filter day-exists?)))

(defn valid-day? [day]
  (and (integer? day) (<= 1 day 25)))

(defn run-day [day]
  (cond
    (not (valid-day? day))
    (do (println (str "Error: Invalid day " day " (must be 1-25)"))
        false)

    (not (day-exists? day))
    (do (println (str "Error: Day " day " not found"))
        false)

    :else
    (let [{:keys [exit]} (shell {:dir (str "day" (format "%02d" day))
                                  :continue true}
                                 "bb" "core.clj")]
      (zero? exit))))

(defn run-days [days]
  (let [results (for [day days]
                  (do
                    (when (> (count days) 1)
                      (println (str "\n═══ Day " day " ═══")))
                    (run-day day)))]
    (every? true? results)))

(defn parse-day-arg [arg]
  (if-let [[_ start end] (re-matches #"(\d+)-(\d+)" arg)]
    (range (parse-long start) (inc (parse-long end)))
    (when-let [n (parse-long arg)]
      [n])))

(defn parse-args [args]
  (->> args
       (mapcat parse-day-arg)
       (filter some?)
       distinct
       sort))

(defn list-days []
  (let [days (available-days)]
    (if (seq days)
      (do
        (println "Available days:")
        (doseq [day days]
          (println (str "  Day " day))))
      (println "No days available yet."))))

(let [args *command-line-args*]
  (cond
    (some #{"-h" "--help"} args)
    (println usage)

    (some #{"-l" "--list"} args)
    (list-days)

    (or (empty? args) (some #{"-a" "--all"} args))
    (let [days (available-days)]
      (if (seq days)
        (when-not (run-days days)
          (System/exit 1))
        (println "No days available yet.")))

    :else
    (let [days (parse-args args)]
      (if (seq days)
        (when-not (run-days days)
          (System/exit 1))
        (do
          (println "Error: No valid days specified")
          (println "Run with --help for usage")
          (System/exit 1))))))
