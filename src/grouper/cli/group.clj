(ns grouper.cli.group
  (:require [integrant.core :as ig]
            [grouper.domain.user-input :as user-input]
            [grouper.usecase.group :as usecase]
            [grouper.util.csv :as csv]))

(defn create-user-input [{:keys [group-count group-request-path block-request-path]}]
  (user-input/->user-input (csv/load-lines group-request-path)
                           (csv/load-lines block-request-path)
                           (Integer/parseInt group-count)))

(defn unique-file-suffix []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-A")))

(defn gen-unique-file-name [prefix extension]
  (str prefix (unique-file-suffix) extension))

(defmethod ig/init-key ::write-groups [_ dep]
  (fn [param]
    (->> (create-user-input param)
         (usecase/generate-lot-with-high-score dep)
         (csv/seq->lines)
         (spit (gen-unique-file-name "groups-" ".csv")))))

