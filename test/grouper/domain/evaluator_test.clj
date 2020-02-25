(ns grouper.domain.evaluator-test
  (:require [grouper.domain.evaluator :as sut]
            [clojure.test :as t]))

(t/deftest test-scored-combination
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
      (t/is (= expected (sut/scored-combination request))))))

(t/deftest test-score-group
  (t/testing "Score group"
    (let [group {:members #{:a :b :c}}
          scored-combination {#{:a :b} {:score 20}
                              #{:c :a} {:score -3}
                              #{:c :d} {:score 2}}
          expected {:members #{:a :b :c}
                    :score {:value 17}}]
      (t/is (= expected (sut/score-group group scored-combination :score))))))

(t/deftest test-score-groups
  (t/testing "Score groups"
    (let [group-lot {:groups [{:members #{:a :b :c}}
                              {:members #{:c :d}}]}
          scored-combination {#{:a :b} {:score 20}
                              #{:c :a} {:score -3}
                              #{:c :d} {:score 2}}
          expected {:groups [{:members #{:a :b :c}
                              :score {:value 17}}
                             {:members #{:c :d}
                              :score {:value 2}}]
                    :score {:value 19}}]
      (t/is (= expected (sut/score-groups group-lot scored-combination :score))))))

(t/deftest test-score-based-evaluator
  (t/testing "Returns function that calculates score of given group lot"
    (let [param {:groups [:group1 :group2]}
          expected {:groups [:scored-group1 :scored-group2]
                    :score-key :score}]
      (with-redefs
        [sut/scored-combination #(when (= :request %) :scored-comnination)
         sut/score-groups #(when (and (= param %1) (= :scored-comnination %2) (= :score-key %3)) expected)]
        (t/is (= expected ((sut/score-based-evaluator :request :score-key) param)))))))
