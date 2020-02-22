
(ns grouper.core
  (:require [clojure.string :as str]))

(def group-count 5)
(def max-team-size 5)
(def block-threshold 0)





(defn remove-invalid-members [members]
  (remove #(or (= "-" %) (= "invalid" %)) members))

(defn create-pairs [member members]
  (map #(list member %) members))

(defn create-requested-pairs [csv]
  (->> csv
       (map #(create-pairs (first %) (remove-invalid-members (rest %))))
       (apply concat)))

(defn create-requested-pair-counts [pairs]
  (->> (map #(sort %) pairs)
       (reduce #(merge-with + %1 {%2 1}) {})))

(defn seq-contains? [coll v]
  (not (neg? (.indexOf coll v ))))

(defn remove-duplicate-members [target members]
  (if (empty? target)
    members
    (if (seq? target)
      (let [t1 (first target)
            t2 (second target)]
        (remove #(or (= % t1) (= % t2)) members))
      (remove #(= % target) members))))

(defn matched-blocks [keys m]
  (reduce
   #(if (contains? m %2)
      (concat %1 (get m %2))
      %1)
   '()
   keys))

(defn count-common-entries [coll1 coll2]
  (let [m1 (reduce #(assoc %1 %2 1) {} coll1)
        m2 (reduce #(assoc %1 %2 1) {} coll2)
        merged (merge-with + m1 m2)]
    (count (filter #(= 2 (val %)) merged))))

(defn sort-by-smallest-and-less-blocks [groups blocks]
  (->> (sort-by (fn [g] (count-common-entries g blocks)) groups)
       (sort-by count)))

(defn assign-mems-to-group [groups members block-m]
  (let [blocks (matched-blocks members block-m)
        sorted-groups (sort-by-smallest-and-less-blocks groups blocks)
        updated-group (flatten (conj (first sorted-groups) members))]
    (println members)
    (when (not (empty? blocks)) (println "not empty: " sorted-groups))
    (conj (rest sorted-groups) updated-group)))



(defn assign-mem-to-group-with [groups mem paired-mem block-m]
  (let [blocks (if (contains? block-m mem) (get block-m mem) (list))]
    (reduce
     #(if (and
           (seq-contains? %2 paired-mem)
           (> max-team-size (count %2))
           (> block-threshold (count-common-entries %2 blocks)))
        (conj %1 (conj %2 mem))
        (conj %1 %2))
     '()
     groups)))

(defn get-other [coll one]
  (when (and (= 2 (count coll)) (seq-contains? coll one))
    (let [fst (first coll)
          scd (second coll)]
      (if (= fst one) scd fst))))

(defn assign-members [groups target pair block-map]
  (if (empty? target)
    groups
    (if (= 1 (count target))
      (let [single-target (first target)]
        (assign-mem-to-group-with groups single-target (get-other pair single-target) block-map))
      (assign-mems-to-group groups target block-map))))

(defn seq-and [seq1 seq2]
  (let [m1 (reduce #(assoc %1 %2 1) {} seq1)
        m2 (reduce #(assoc %1 %2 1) {} seq2)
        merged (merge-with + m1 m2)]
    (map first (filter #(= 2 (val %)) merged))))

(defn count-in [nested-coll]
  (reduce #(+ %1 (count %2)) 0 nested-coll))

(defn create-groups-r
  ([groups pairs members block-map]
   (let [target-pair (first pairs)
         assignable-target (seq-and target-pair members)
         next-pairs (rest pairs)
         next-groups (assign-members groups assignable-target target-pair block-map)
         group-unchanged (= (count-in groups) (count-in next-groups))
         next-members (if group-unchanged  members (remove-duplicate-members assignable-target members))]
     (when (and (not (empty? assignable-target)) (not group-unchanged))
       (println
        "Assigned target with request: "
        assignable-target
        (if (= 1 (count assignable-target)) (str "->(" (get-other target-pair (first assignable-target)) ")") "")
        ", " (count next-members) " remained."))
     (if (empty? next-members)
       next-groups
       (if (empty? next-pairs)
         (create-groups-r next-groups next-members block-map)
         (create-groups-r next-groups next-pairs next-members block-map)))))
  ([groups members block-map]
   (let [target (first members)
         next-members (remove-duplicate-members target members)
         next-groups (assign-mems-to-group groups target block-map)]
     (println "Assigned target: " target ", " (count next-members) " remained.")
     (if (empty? next-members)
       next-groups
       (create-groups-r next-groups next-members block-map)))))

(defn create-groups [members requests block-map]
  (let [empty-groups (repeat group-count '())]
    (println "Session Input*************************************************")
    (println "Requested pairs: " requests)
    (println "Paris to avoid: " block-map)
    (create-groups-r empty-groups requests members block-map)))

(defn contains-all-in-nested-coll [coll nested-coll]
  (some #(.containsAll % coll) nested-coll))

(defn remove-fulfilled-requests [requests groups]
  (reduce #(if (contains-all-in-nested-coll %2 groups) %1 (conj %1 %2)) '() requests))

(defn assoc-in-seq [k seq-v m]
  (if (contains? m k)
    (assoc m k (conj (get m k) seq-v))
    (assoc m k (list seq-v))))

(defn create-block-map [blocks]
  (reduce (fn [m [fst scd]] (->> (assoc-in-seq fst scd m)
                                 (assoc-in-seq scd fst)))
          {}
          blocks))

(defn create-all-possible-pairs-r [group pairs]
  (let [mem (first group)
        mems (rest group)
        next-pairs (reduce #(conj %1 (list mem %2)) pairs mems)]
    (if (= 1 (count mems))
      next-pairs
      (recur mems next-pairs))))

(defn create-all-possible-pairs [groups]
  (reduce #(concat %1 (create-all-possible-pairs-r %2 '())) '() groups))

(defn create-two-sessions [members requests blocks]
  (let [first-block-map (create-block-map blocks)
        first-groups (create-groups members requests first-block-map)
        filtered-requests (remove-fulfilled-requests requests first-groups)
        updated-blocks (concat blocks (create-all-possible-pairs first-groups))
        second-block-map (create-block-map updated-blocks)
        second-groups (create-groups members filtered-requests second-block-map)]
    (concat first-groups second-groups)))

(defn filtered-keys [pred m]
  (->> (filter pred m)
       (map first)))

(defn create-sessions-with [{:keys [requests blocks]}]
  (let [members (map first requests)
        shuffled-members (shuffle members)
        request-map (->> (create-requested-pairs requests)
                         (create-requested-pair-counts))
        prior-pairs (filtered-keys #(= 2 (val %)) request-map)
        pairs (filtered-keys #(= 1 (val %)) request-map)
        shuffled-pairs (concat (shuffle prior-pairs) (shuffle pairs))]
    (println "App Input*************************************************")
    (println "Members: " members)
    (println "Matched requests: " prior-pairs)
    (println "Requests: " pairs)
    (println "Blocks: " blocks)
    (println "******************************************************")
    (println "")
    (create-two-sessions shuffled-members shuffled-pairs blocks)))

(defn create-csv-line [coll]
  (str (str/join "," coll) \newline))

(defn create-csv-lines [coll]
  (reduce #(str %1 (create-csv-line %2)) "" coll))

(defn load-csv [csv-file]
  (->> (slurp csv-file)
       (str/split-lines)
       (map #(str/split % #","))))

(defn load-requests-and-blocks [request-csv block-csv]
  {:requests (load-csv request-csv)
   :blocks (load-csv block-csv)})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (load-requests-and-blocks (first args) (second args))
       (create-sessions-with)
       (create-csv-lines)
       (spit "groups.csv")))
