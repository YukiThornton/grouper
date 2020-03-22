(ns grouper.usecase.group
  (:require [integrant.core :as ig]
            [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]
            [grouper.port.history :as port]))

(defn combine-grouper-and-evaluator [grouper evaluator]
  (fn [request] (-> (grouper request)
                    (evaluator))))

(defn scored-random-lot-generator [{:keys [requirement request]}]
  (let [grouper (grouper/random-grouper requirement)
        evaluator (evaluator/score-based-evaluator request)]
    (fn [request] (-> (grouper request)
                      (evaluator)))))

(defn map-times [count f param]
  (map (fn [p] (f p)) (repeat count param)))

(defn highest-scored-group-lot [{:keys [request] :as input} gen-count]
  (let [generator (scored-random-lot-generator input)
        lots (map-times gen-count generator request)]
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

(defmethod ig/init-key ::highest-of-random [_ {:keys [history-port gen-count]}]
  (fn [user-input]
    (let [history (port/load-history history-port)
          input (combine-input user-input history)]
      (-> (highest-scored-group-lot input gen-count)
          log-group-lot
          group-lot->group-members))))
