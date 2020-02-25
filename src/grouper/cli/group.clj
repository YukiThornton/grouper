(ns grouper.cli.group
  (:require [clojure.string :as str]
            [grouper.usecase.group :as usecase]))

(defn load-csv-lines [csv-file]
  (->> (slurp csv-file)
       (str/split-lines)
       (map #(str/split % #","))))

(defn remove-invalid-csv-data [data]
  (remove #(or (= "-" %) (= "invalid" %)) data))

(defn to-requests [csv]
  (let [csv-lines (load-csv-lines csv)
        keys (map first csv-lines)
        values (->> (map rest csv-lines)
                    (map remove-invalid-csv-data))]
    (zipmap keys values)))

(defn to-members [csv]
  (let [csv-lines (load-csv-lines csv)]
    (into #{} (map first csv-lines))))

(defn current-dir-file-names []
  (.list (clojure.java.io/file ".")))

(defn get-history-files []
  (->> (current-dir-file-names)
       (filter #(str/ends-with? % ".csv"))
       (filter #(str/starts-with? % "groups-"))))

(defn to-history-from-csv [csv]
  (let [csv-lines (load-csv-lines csv)]
    (into #{} (map #(into #{} %) csv-lines))))

(defn load-history []
  (into #{} (mapcat to-history-from-csv (get-history-files))))

(defn to-grouping-requirement [{:keys [group-requests]}]
  {:group-count 5
   :members (to-members group-requests)})

(defn to-grouping-request [{:keys [group-requests block-requests]}]
  {:group-requests (to-requests group-requests)
   :block-requests (to-requests block-requests)
   :history (load-history)})

(defn to-groups [param]
  (let [group-lot(usecase/highest-scored-group-lot (to-grouping-requirement param)
                                                   (to-grouping-request param))]
    (println (:groups group-lot))
    (println (:score group-lot))
    (map :members (:groups group-lot))))

(defn- to-csv-line [coll]
  (str (str/join "," coll) \newline))

(defn to-csv-lines [coll]
  (str/join (map to-csv-line coll)))

(defn unique-file-suffix []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-A")))

(defn output-file-name []
  (str "groups-" (unique-file-suffix) ".csv"))

(defn to-groups-in-csv [param]
  {:data (-> (to-groups param)
             (to-csv-lines))
   :file-name (output-file-name)})

(defn write-groups-to-csv [param]
  (let [{:keys [data file-name]} (to-groups-in-csv param)]
    (spit file-name data)))
