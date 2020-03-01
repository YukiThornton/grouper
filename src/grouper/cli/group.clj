(ns grouper.cli.group
  (:require [integrant.core :as ig]
            [grouper.util.csv :as csv]))

(defn remove-invalid-csv-data [data]
  (remove #(or (= "-" %) (= "invalid" %)) data))

(defn to-requests [csv]
  (let [csv-lines (csv/load-lines csv)
        keys (map first csv-lines)
        values (->> (map rest csv-lines)
                    (map remove-invalid-csv-data))]
    (zipmap keys values)))

(defn to-members [csv]
  (let [csv-lines (csv/load-lines csv)]
    (into #{} (map first csv-lines))))

(defn to-grouping-requirement [{:keys [group-count group-requests]}]
  {:group-count (Integer/parseInt group-count)
   :members (to-members group-requests)})

(defn to-grouping-request [{:keys [group-requests block-requests]}]
  {:group-requests (to-requests group-requests)
   :block-requests (to-requests block-requests)})

(defn to-groups [create-groups-fn param]
  (let [group-lot (create-groups-fn (to-grouping-requirement param)
                                    (to-grouping-request param))]
    (println (:groups group-lot))
    (println (:score group-lot))
    (map :members (:groups group-lot))))

(defn unique-file-suffix []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-A")))

(defn output-file-name []
  (str "groups-" (unique-file-suffix) ".csv"))

(defn spit-to-csv [content]
  (spit (output-file-name) content))

(defn write-groups-to-csv [create-groups-fn param]
  (-> (to-groups create-groups-fn param)
      (csv/to-lines)
      (spit-to-csv)))

(defmethod ig/init-key ::write-groups [_ {:keys [create-groups]}]
  (fn [param] (write-groups-to-csv create-groups param)))
