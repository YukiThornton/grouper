(ns grouper.domain.evaluator-test
  (:require [grouper.domain.evaluator :as sut]
            [clojure.test :as t]))

(t/deftest test-create-pair-score-map
  (t/testing "Returns map with combination set as key, score as value"
    (let [request {:group-requests {:a [:b :i]
                                    :b [:c :d]
                                    :c [:b]}
                   :block-requests {:a [:h :d]
                                    :d [:a :c :b]}
                   :history #{#{:h :a :i} #{:j :k}}}
          expected {#{:c :b} {:type :mutual-request
                               :score 40}
                    #{:a :b} {:type :oneway-request
                               :score 20}
                    #{:a :d} {:type :mutual-block
                               :score -60}
                    #{:d :c} {:type :oneway-block
                               :score -30}
                    #{:b :d} {:type :conflict
                               :score -20}
                    #{:a :i} {:type :history-block
                               :score -70}
                    #{:a :h} {:type :history-block
                              :score -70}
                    #{:h :i} {:type :history-block
                              :score -70}
                    #{:j :k} {:type :history-block
                               :score -70}}]
      (t/is (= expected (sut/create-pair-score-map request))))))

(t/deftest test-score-group
  (t/testing "Score group"
    (let [group {:members #{:a :b :c}
                 :other-eval :other-eval1}
          pair&score-map {#{:a :b} {:score 20}
                              #{:c :a} {:score -3}
                              #{:c :d} {:score 2}}
          expected {:members #{:a :b :c}
                    :score {:value 17}
                    :other-eval :other-eval1}]
      (t/is (= expected (sut/score-group group pair&score-map))))))

(t/deftest test-score-lot
  (t/testing "Score lot"
    (let [lot {:groups [{:members #{:a :b :c}}
                        {:members #{:c :d}}]
               :other-eval :other-eval1}
          pair&score-map {#{:a :b} {:score 20}
                              #{:c :a} {:score -3}
                              #{:c :d} {:score 2}}
          expected {:groups [{:members #{:a :b :c}
                              :score {:value 17}}
                             {:members #{:c :d}
                              :score {:value 2}}]
                    :score {:value 19}
                    :other-eval :other-eval1}]
      (t/is (= expected (sut/score-lot lot pair&score-map))))))

(t/deftest test-create-score-based-evaluator
  (t/testing "Returns score-based-evaluator that calculates score of given group lot"
    (let [lot {:groups [:group1 :group2]}
          expected {:groups [:scored-group1 :scored-group2]
                    :score :score1}]
      (with-redefs
        [sut/create-pair-score-map #(when (= :request %) :pair&score-map)
         sut/score-lot #(when (and (= lot %1) (= :pair&score-map %2)) expected)]
        (t/is (= expected ((sut/create-score-based-evaluator :request) lot)))))))
