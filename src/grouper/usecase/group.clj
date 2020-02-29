(ns grouper.usecase.group
  (:require [grouper.domain.grouper :as grouper]
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

(defn highest-scored-group-lot [requirement request]
  (let [generator
        (-> (combine-grouper&evaluators (grouper/random-grouper requirement)
                                        [(evaluator/score-based-evaluator request)])
            (create-lot-generator request))
        lots (map-times 1000 generator)]
    ((picker/high-score-picker [:score :value]) lots)))
