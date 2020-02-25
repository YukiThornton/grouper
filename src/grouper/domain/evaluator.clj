(ns grouper.domain.evaluator)

(def scores {:mutual-request 40
             :oneway-request 20
             :mutual-block -60
             :oneway-block -30
             :conflict -20
             :history-block -70})


(defn- all-combinations-r [[first & rst] result]
  (if (empty? rst)
    result
    (recur rst (concat result (map #(set [first %]) rst)))))

(defn all-combinations [seq]
  (all-combinations-r seq []))

(defn pair-up [[elm elms]]
  (map #(set [elm %]) elms))

(defn map-value-based-on-occurence [seq single-val double-val]
  (let [occurence-map (frequencies seq)]
    (reduce-kv
     (fn [m k v] (if (= 1 v) (assoc m k single-val) (assoc m k double-val)))
     {}
     occurence-map)))

(defn score [type] {:type type :score (type scores)})

(defn score-by-occurence [requests single-score-type double-score-type]
  (let [pairs (flatten (map pair-up requests))]
    (map-value-based-on-occurence pairs (score single-score-type) (score double-score-type))))

(defn scored-combination [{:keys [group-requests block-requests history]}]
  (let [unique-history-pairs (into #{} (flatten (map all-combinations history)))
        history-scores (into {} (map (fn [pair] [pair (score :history-block)]) unique-history-pairs))
        g-request-scores (score-by-occurence group-requests :oneway-request :mutual-request)
        b-request-scores (score-by-occurence block-requests :oneway-block :mutual-block)
        request-scores (merge-with (fn [_ _] (score :conflict)) g-request-scores b-request-scores)]
    (merge request-scores history-scores)))

(defn score-group [group scored-combination score-key]
  (let [pairs (all-combinations (:members group))
        score
        (reduce
         (fn [sum pair]
           (if (contains? scored-combination pair)
             (+ sum (:score (get scored-combination pair)))
             sum))
         0 pairs)]
    (assoc group score-key {:value score})))

(defn score-groups [groups scored-combination score-key]
  (let [updated-groups (map #(score-group % scored-combination score-key) (:groups groups))
        total-score {:value (apply + (map :value (map score-key updated-groups)))}]
    (assoc (assoc groups :groups updated-groups) score-key total-score)))

(defn score-based-evaluator [request score-key]
  (let [combination (scored-combination request)]
    (fn [group-lot]
      (score-groups group-lot combination score-key))))
