(ns grouper.usecase.group
  (:require [integrant.core :as ig]
            [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]))

(defn combine-evaluators [evaluators]
  (fn [initial-group]
    (reduce (fn [group evaluator] (evaluator group)) initial-group evaluators)))

(defn combine-grouper&evaluators [grouper evaluators]
  (let [combined-evals (combine-evaluators evaluators)]
    (fn [request] (-> (grouper request)
                      (combined-evals)))))

(defn create-lot-generator [f request]
  (fn [] (f request)))

(defn map-times [count f]
  (map (fn [_] (f)) (repeat count nil)))

(defn highest-scored-group-lot [requirement request gen-count]
  (let [generator
        (-> (combine-grouper&evaluators (grouper/random-grouper requirement)
                                        [(evaluator/score-based-evaluator request)])
            (create-lot-generator request))
        lots (map-times gen-count generator)]
    ((picker/high-score-picker [:score :value]) lots)))

(defn log-group-lot [lot]
  (println (:groups lot))
  (println (:score lot))
  lot)

(defn group-lot->group-members [lot]
  (->> lot
       (:groups)
       (map :members)))

(defn request-with-history [requirement load-history-fn]
  (assoc requirement :history (load-history-fn)))

(defmethod ig/init-key ::highest-of-random [_ {:keys [load-history gen-count]}]
  (fn [{:keys [requirement request]}]
    (let [updated-request (request-with-history request load-history)]
      (-> (highest-scored-group-lot requirement updated-request gen-count)
          log-group-lot
          group-lot->group-members))))
