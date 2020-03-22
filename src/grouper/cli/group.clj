(ns grouper.cli.group
  (:require [integrant.core :as ig]
            [grouper.domain.user-input :as user-input]
            [grouper.util.csv :as csv]))

(defn create-group-lot [create-group-fn {:keys [group-count group-requests block-requests]}]
  (create-group-fn (user-input/->user-input (csv/load-lines group-requests)
                                            (csv/load-lines block-requests)
                                            (Integer/parseInt group-count))))

(defn unique-file-suffix []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-A")))

(defn gen-unique-file-name [prefix extension]
  (str prefix (unique-file-suffix) extension))

(defmethod ig/init-key ::write-groups [_ {:keys [create-groups]}]
  (fn [param] (->> (create-group-lot create-groups param)
                  (csv/seq->lines)
                  (spit (gen-unique-file-name "groups-" ".csv")))))
