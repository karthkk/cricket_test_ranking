(ns test_cricket_ranking.data_processor
  (:require [clojure.contrib.string :as str-utils :only (join)])
  (:import [org.jblas DoubleMatrix Eigen])
  (:import (java.io BufferedReader FileReader)))

(defn year_str_of_line [line] (nth (.split line "\t") 1))

(defn match_year [line]
  (Integer. (.trim (nth (.split (year_str_of_line line) "/") 0))))

(defn match_between_dates? [line start_date end_date]
  (let [year_of_match (match_year line)](and
    (>= year_of_match start_date)
    (<= year_of_match end_date))))

(def countries
  [ "Australia"  "Bangladesh"  "England"  "India" 
    "New Zealand"  "Pakistan"  "South Africa" "Sri Lanka" 
    "West Indies"  "Zimbabwe" ])

(def countries_pattern (re-pattern (str-utils/join "|"  countries)))

(defn get-country-names [line] 
  (take 2 (re-seq countries_pattern line)))

(defn teams [line, playing_countries]
  (map #(playing_countries %) (get-country-names line)))

(defn winner [line, playing_countries]
  (or (playing_countries (.trim (nth (.split line "\t") 3))) (nth (teams line playing_countries) 0)))

(def score-pattern
  (re-pattern "([0-9]*)-([0-9]*) \\(([0-9]*)\\)"))

(defn scores [line]
  (let [wins (nth (re-seq score-pattern (nth (.split line "\t") 4)) 0)
        winner_score (Double. (nth wins 1))
        loser_score (Double. (nth wins 2))
        total_matches (Double. (nth wins 3))]
    [(/ winner_score (+ total_matches 1)) (/ loser_score (+ total_matches 1))]))


(defn populate_matrix [line d playing_countries]
  (let [teams (teams line playing_countries) scores (scores line) winner (winner line playing_countries)
        loser (if (== winner (nth teams 0)) (nth teams 1) (nth teams 0))]
    (.put d winner loser (+ (.get d winner loser) (nth scores 0)))
    (.put d loser winner ( + (.get d loser winner) (nth scores 1)))))

(defn to_matrix [file_name start_year end_year playing_countries]
  (let [d (DoubleMatrix/eye (count playing_countries))]
    (with-open [rdr (BufferedReader. (FileReader. file_name))]
      (doseq [line (filter #(match_between_dates? % start_year end_year) (line-seq rdr))]
        (println line)(populate_matrix line d playing_countries))) d))

(defn playing_countries [result_file start_year end_year]
   (let [country_names (set (flatten (flatten (for [line  (filter #(match_between_dates? % start_year end_year)
             (.split (slurp result_file) "\n"))] (get-country-names line ))))) indexes (range (count country_names))] (zipmap (sort country_names) indexes)))

(defn abs [x] (if (pos? x) x (- x)))

  (defn arg-max [coll] 
  (last (last (sort (seq (zipmap coll (range (count coll))))))))

(defn get_max_eigen_index [d len] 
  (arg-max (let [idxs (range len) vals (for [i idxs] (abs (.real (.get d i i))))] (println vals) vals))) 

(defn compute_scores_by_teams [result_file start_year end_year, playing_countries]
  (map abs (let [m (to_matrix result_file start_year end_year playing_countries)
        size (count playing_countries)
        ev (Eigen/eigenvectors m)
        eigen_vectors (get ev 0)
        eigen_values (get ev 1)
        top_eigen_value_index (get_max_eigen_index eigen_values size) 
        all_team_scores (.getColumn eigen_vectors top_eigen_value_index)]
    (for [team_id (range size)] (.getReal all_team_scores team_id)))))

(defn compute_ranks [result_file start_year end_year]  
  (let [playing_countries (playing_countries result_file start_year end_year)] 
    (zipmap (sort (keys playing_countries))
       (compute_scores_by_teams result_file start_year end_year playing_countries))))

(compute_ranks "/Users/karthik/workspace/clojure_matlab/td" 1950 1960)
