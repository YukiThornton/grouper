(ns grouper.usecase.group-test
  (:require [grouper.usecase.group :as sut]
            [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]
            [clojure.test :as t]))

(t/deftest test-create-evaluated-group-lot
  (t/testing "Creates a single lot with grouper and an evaluator"
    (let [expected :evaluated-lot
          request :request
          param {:request request
                 :grouper #(when (= request %) :lot)
                 :evaluators [#(when (= :lot %) :evaluated-lot)]}]
      (t/is (= expected (sut/create-evaluated-group-lot param)))))

  (t/testing "Creates a single lot with grouper and multiple evaluators"
    (let [expected :evaluated-lot3
          request :request
          param {:request request
                 :grouper #(when (= request %) :lot)
                 :evaluators [#(when (= :lot %) :evaluated-lot1)
                              #(when (= :evaluated-lot1 %) :evaluated-lot2)
                              #(when (= :evaluated-lot2 %) :evaluated-lot3)]}]
      (t/is (= expected (sut/create-evaluated-group-lot param))))))

(t/deftest test-create-evaluated-group-lots
  (t/testing "Creates specified amount of lots with grouper and evaluators"
    (let [expected [:lot :lot :lot]
          param {:grouper :grouper-fn
                 :request :request
                 :evaluators :evaluators
                 :lot-count 3}
          param-for-lot {:grouper :grouper-fn
             :request :request
             :evaluators :evaluators}]
      (with-redefs [sut/create-evaluated-group-lot
                    #(when (= param-for-lot %) :lot)]
        (t/is (= expected (sut/create-evaluated-group-lots param)))))))

(t/deftest test-highest-scored-group-lot
  (t/testing "Creates 100 lots of random groups and returns one with highest score"
    (let [expected :highest-scored-lot
          requirement :requirement
          request :request]
      (with-redefs [grouper/random-grouper
                    #(when (= requirement %)
                       :grouper-fn)
                    evaluator/score-based-evaluator
                    #(when (and (= request %1)
                                (= :score %2))
                       :evaluator-fn)
                    sut/create-evaluated-group-lots
                    #(when (and (= :grouper-fn (:grouper %))
                                (= :evaluator-fn (first (:evaluators %)))
                                (= 100 (:lot-count %)))
                       :group-lots)
                    picker/high-score-picker
                    #(when (= [:score :value] %)
                       (fn [group-lots] (when (= :group-lots group-lots) :highest-scored-lot)))]
        (t/is (= expected (sut/highest-scored-group-lot requirement request)))))))
