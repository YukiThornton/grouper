(ns grouper.domain.picker)

(defn high-score-picker [score-keys]
  (fn [group-lots]
    (last (sort-by #(get-in % score-keys) group-lots))))
