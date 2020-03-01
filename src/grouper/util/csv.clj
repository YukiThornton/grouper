(ns grouper.util.csv
  (:require [clojure.string :as str]))

(defn load-lines [csv-file]
  (->> (slurp csv-file)
       (str/split-lines)
       (map #(str/split % #","))))

(defn- to-line [coll]
  (str (str/join "," coll) \newline))

(defn to-lines [coll]
  (str/join (map to-line coll)))
