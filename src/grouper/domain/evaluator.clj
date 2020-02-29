(ns grouper.domain.evaluator)

(def scores {:mutual-request 40
             :oneway-request 20
             :mutual-block -60
             :oneway-block -30
             :conflict -20
             :history-block -70})

(defn score-with-type [type] {:type type :score (type scores)})

(defn- all-pair-combinations-r [[first & rst] result]
  (if (empty? rst)
    result
    (recur rst (concat result (map #(set [first %]) rst)))))

(defn- all-pair-combinations [seq]
  (all-pair-combinations-r seq []))

(defn- pair-up [[elm elms]]
  (map #(set [elm %]) elms))

(defn- map-filled-with [keys v]
  (->> (repeat (count keys) v)
       (zipmap keys)))

(defn- filter-with-frequency [coll frequency]
  (->> (frequencies coll)
       (filter #(= (val %) frequency))
       (map first)))

(defn- request->pair&score-map [requests oneway-type mutual-type]
  (let [all-pairs (flatten (map pair-up requests))
        unique-pairs (into #{} all-pairs)
        mutual-pairs (filter-with-frequency all-pairs 2)
        unique-pairs&score-map (map-filled-with unique-pairs (score-with-type oneway-type))
        mutual-pairs&score-map (map-filled-with mutual-pairs (score-with-type mutual-type))]
    (merge unique-pairs&score-map mutual-pairs&score-map)))

(defn- group&block->pair&score-map [group-requests block-requests]
  (let [ps-map-from-group
        (request->pair&score-map group-requests :oneway-request :mutual-request)
        ps-map-from-block
        (request->pair&score-map block-requests :oneway-block :mutual-block)
        overwrite-with-conflict (fn [_ _] (score-with-type :conflict))]
    (merge-with overwrite-with-conflict ps-map-from-group ps-map-from-block)))

(defn- history->pair&score-map [history]
  (let [history-pairs (flatten (map all-pair-combinations history))
        unique-history-pairs (into #{} history-pairs)]
    (map-filled-with unique-history-pairs (score-with-type :history-block))))

(defn- create-pair&score-map [{:keys [group-requests block-requests history]}]
  (let [ps-map-from-history (history->pair&score-map history)
        ps-map-from-request (group&block->pair&score-map group-requests block-requests)]
    (merge ps-map-from-request ps-map-from-history)))

(defn score-group [group pair&score-map]
  (let [pairs (all-combinations (:members group))
        matching-scores (vals (select-keys pair&score-map pairs))
        score-list (map :score matching-scores)
        sum (apply + score-list)]
    (assoc group :score {:value sum})))

(defn- sum-scores [scores]
  (reduce (fn [sum score] (+ sum (:value score))) 0 scores))

(defn score-lot [lot pair&score-map]
  (let [scored-groups (map #(score-group % pair&score-map) (:groups lot))
        total-score-val (sum-scores (map :score scored-groups))]
    (-> (assoc lot :groups scored-groups)
        (assoc :score {:value total-score-val}))))

(defn score-based-evaluator [request]
  (let [pair&score-map (create-pair&score-map request)]
    (fn [lot]
      (score-lot lot pair&score-map))))
