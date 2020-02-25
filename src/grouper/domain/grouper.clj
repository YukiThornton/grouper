(ns grouper.domain.grouper)

(defn group-sizes [total-count group-count]
  (let [minimum-group-size (int (/ total-count group-count))
        maximum-group-size (inc minimum-group-size)
        remainder (rem total-count group-count)]
    (concat (repeat remainder maximum-group-size)
            (repeat (- group-count remainder) minimum-group-size))))

(defn subvec-indeces [group-sizes]
  (rest (reduce
         #(let [last-index (second (last %1))]
           (conj %1 [last-index (+ last-index %2)]))
         [[0 0]]
         group-sizes)))

(defn random-grouper [{:keys [members group-count]}]
  (let [members-vec (if (vector? members) members (vec members))
        group-sizes (group-sizes (count members-vec) group-count)
        indeces (subvec-indeces group-sizes)]
    (fn [_]
      (let [shuffled (shuffle members-vec)]
        {:groups
         (map (fn [[start end]] {:members (subvec shuffled start end)}) indeces)}))))
