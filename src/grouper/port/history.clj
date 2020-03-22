(ns grouper.port.history
  (:require [integrant.core :as ig]))

(defprotocol History
  (load-history [this]))

(defmethod ig/init-key ::history
  [_ {:keys [get-history]}]
  (reify History
    (load-history [this]
      (get-history))))

