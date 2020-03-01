(ns grouper.core
  (:require [integrant.core :as ig]
            [clojure.string :as str]
            [grouper.cli.group]
            [grouper.usecase.group]))

(defn -main [& args]
  (let [conf (ig/read-string (slurp "resources/grouper/config.edn"))
        app (ig/init conf)
        f (:grouper.cli.group/write-groups app)]
    (f {:group-requests (first args)
        :block-requests (second args)
        :group-count (nth args 2)})))
