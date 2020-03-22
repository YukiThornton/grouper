(ns grouper.util.csv
  (:require [clojure.string :as str]))

(defn load-lines [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map #(str/split % #","))))

(defn- seq->line [coll]
  (str (str/join "," coll) \newline))

(defn seq->lines [coll]
  (str/join (map seq->line coll)))
