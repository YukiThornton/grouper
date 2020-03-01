(ns grouper.gateway.history
  (:require [integrant.core :as ig]
            [clojure.string :as str]
            [grouper.util.csv :as csv]))

(defn current-dir-file-names []
  (.list (clojure.java.io/file ".")))

(defn get-history-files []
  (->> (current-dir-file-names)
       (filter #(str/ends-with? % ".csv"))
       (filter #(str/starts-with? % "groups-"))))

(defn to-history-from-csv [csv]
  (let [csv-lines (csv/load-lines csv)]
    (into #{} (map #(into #{} %) csv-lines))))

(defn do-load-history []
  (into #{} (mapcat to-history-from-csv (get-history-files))))

(defmethod ig/init-key ::load-history [_ _]
  (fn [] (do-load-history)))
