(ns grouper.domain.picker-test
  (:require [grouper.domain.picker :as sut]
            [clojure.test :as t]))

(t/deftest test-high-score-picker
  (t/testing "Returns group lot with highest score"
    (let [expected {:groups :groups2
                    :score {:value 98}}
          group-lots [{:groups :groups1
                       :score {:value -10}}
                      {:groups :groups2
                       :score {:value 98}}
                      {:groups :groups3
                       :score {:value 97}}]]
      (t/is (= expected (sut/high-score-picker [:score :value] group-lots))))))
