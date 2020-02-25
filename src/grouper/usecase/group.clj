(ns grouper.usecase.group
  (:require [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]))

(defn create-evaluated-group-lot [{:keys [grouper request evaluators]}]
  (reduce (fn [map evaluator] (evaluator map)) (grouper request) evaluators))

(defn create-evaluated-group-lots [{:keys [lot-count] :as param}]
  (let [param-for-lot (dissoc param :lot-count)]
    (map
     (fn [_] (create-evaluated-group-lot param-for-lot))
     (repeat lot-count nil))))

(defn highest-scored-group-lot [requirement request]
  (let [lots (create-evaluated-group-lots {:grouper (grouper/random-grouper requirement)
                                 :evaluators [(evaluator/score-based-evaluator request :score)]
                                 :lot-count 100})]
    ((picker/high-score-picker [:score :value]) lots)))
