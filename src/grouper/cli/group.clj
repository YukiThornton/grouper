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

(defn create-group-lot [create-groups-fn param]
  (create-groups-fn (to-grouping-requirement param)
                   (to-grouping-request param)))

(defn log-group-lot [lot]
  (println (:groups lot))
  (println (:score lot))
  lot)

(defn to-group-members [lot]
  (->> lot
      (:groups)
      (map :members)))

(defn create-group-members-with-logging [create-groups-fn param]
  (-> (create-group-lot create-groups-fn param)
      (log-group-lot)
      (to-group-members)))

(defn unique-file-suffix []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-A")))

(defn output-file-name []
  (str "groups-" (unique-file-suffix) ".csv"))

(defn spit-to-csv [content]
  (spit (output-file-name) content))

(defn write-group-members-to-csv [create-groups-fn param]
  (-> (create-group-members-with-logging create-groups-fn param)
      (csv/to-lines)
      (spit-to-csv)))

(defmethod ig/init-key ::write-groups [_ {:keys [create-groups]}]
  (fn [param] (write-group-members-to-csv create-groups param)))
