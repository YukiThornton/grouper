(ns grouper.usecase.group
  (:require [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]
            [grouper.port.history :as port]))

(defn highest-scored-group-lot [{:keys [requirement request]} gen-count]
  (let [grouper (grouper/create-random-grouper requirement)
        evaluator (evaluator/create-score-based-evaluator request)
        lots (repeatedly gen-count #(-> (grouper request)
                                        (evaluator)))]
    (picker/high-score-picker [:score :value] lots)))

(defn log-group-lot [lot]
  (println (:groups lot))
  (println (:score lot))
  lot)

(defn group-lot->group-members [lot]
  (->> (:groups lot)
       (map :members)))

(defn combine-input [{:keys [request] :as user-input} history]
  (->> (assoc request :history history)
       (assoc user-input :request)))

(defn generate-lot-with-high-score [{:keys [history-port gen-count]} user-input]
  (let [history (port/load-history history-port)
        input (combine-input user-input history)]
    (-> (highest-scored-group-lot input gen-count)
        log-group-lot
        group-lot->group-members)))
