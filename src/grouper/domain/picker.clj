(ns grouper.domain.picker)

(defn high-score-picker [score-keys group-lots]
  (last (sort-by #(get-in % score-keys) group-lots)))
