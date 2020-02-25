(ns grouper.domain.pair)

(defn- create-pair [one the-other]
  #{one the-other})

(defn- requests->pairs [requests]
  (mapcat (fn [[member members]] (map #(create-pair member %) members)) requests))

(defn- pair-list->pair&count [pair-list]
  (reduce #(merge-with + %1 {%2 1}) {} pair-list))

(defn- keys-of-exact-value [value m]
  (->> (filter #(= value (val %)) m)
       (map first)))

(defn pairs-ordered-by-mutuality [requests]
  (let [pair-list (requests->pairs requests)
        pair&count (pair-list->pair&count pair-list)
        mutual-pair-list (keys-of-exact-value 2 pair&count)
        unshared-pair-list (keys-of-exact-value 1 pair&count)]
    (concat (shuffle mutual-pair-list) (shuffle unshared-pair-list))))
