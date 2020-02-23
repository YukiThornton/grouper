
(ns grouper.core
  (:require [clojure.string :as str]
            [grouper.cli.group :as cli]
            [grouper.usecase.group :refer :all]))

(defn create-csv-line [coll]
  (str (str/join "," coll) \newline))

(defn create-csv-lines [coll]
  (reduce #(str %1 (create-csv-line %2)) "" coll))

(defn load-csv [csv-file]
  (->> (slurp csv-file)
       (str/split-lines)
       (map #(str/split % #","))))

(defn load-requests-and-blocks [request-csv block-csv]
  {:requests (load-csv request-csv)
   :blocks (load-csv block-csv)})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cli/write-groups-to-csv {:group-requests (first args) :block-requests (second args)})
  #_(->> (load-requests-and-blocks (first args) (second args))
       (create-sessions-with)
       (create-csv-lines)
       (spit "groups.csv")))
