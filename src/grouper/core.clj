(ns grouper.core
  (:require [integrant.core :as ig]
            [clojure.string :as str]
            [grouper.cli.group]
            [grouper.usecase.group]
            [grouper.gateway.history]))

(defn -main [& args]
  (let [conf (ig/read-string (slurp "resources/grouper/config.edn"))
        app (ig/init conf)
        f (:grouper.cli.group/write-groups app)]
    (f {:group-request-path (first args)
        :block-request-path (second args)
        :group-count (nth args 2)})))
