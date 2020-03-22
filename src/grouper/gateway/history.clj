(ns grouper.gateway.history
  (:require [integrant.core :as ig]
            [clojure.string :as str]
            [grouper.util.csv :as csv]))

(defn file-names-on-current-dir []
  (.list (clojure.java.io/file ".")))

(defn history-file? [file-name]
  (and (str/ends-with? file-name ".csv")
       (str/starts-with? file-name "groups-")))

(defn available-history-files []
  (->> (file-names-on-current-dir)
       (filter history-file?)))

(defn csv->history [csv-lines]
  (->> (map set csv-lines)
       set))

(defmethod ig/init-key ::get-history [_ _]
  (fn [] (->> (available-history-files)
              (map csv/load-lines)
              (mapcat csv->history)
              set)))
