(ns grouper.domain.user-input)

(defn remove-invalid-requests [req]
  (->> (vals req)
       (map #(remove #{"-" "invalid"} %))
       (zipmap (keys req))))

(defn seq->requests [s]
  (let [keys (map first s)
        values (->> (map rest s))]
    (zipmap keys values)))

(defn seq->members [s]
  (into #{} (map first s)))

(defn seq->valid-requests [s]
  (-> (seq->requests s)
      (remove-invalid-requests)))

(defn ->user-input [group-request-seq block-request-seq group-count]
  {:requirement {:group-count group-count
                 :members (seq->members group-request-seq)}
   :request {:group-requests (seq->valid-requests group-request-seq)
             :block-requests (seq->valid-requests block-request-seq)}})
