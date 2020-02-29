
(ns grouper.core
  (:require [clojure.string :as str]
            [grouper.cli.group :as cli]
            [grouper.usecase.group :refer :all]))

(defn -main [& args]
  (cli/write-groups-to-csv {:group-requests (first args)
                            :block-requests (second args)
                            :group-count (nth args 2)}))
