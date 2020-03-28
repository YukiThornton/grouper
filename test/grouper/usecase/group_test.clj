(ns grouper.usecase.group-test
  (:require [grouper.usecase.group :as sut]
            [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]
            [grouper.port.history :as port]
            [clojure.test :as t]))

(t/deftest test-highest-scored-group-lot
  (t/testing "Creates lots of random groups and returns one with highest score"
    (let [expected :highest-scored-lot
          input {:requirement :requirement-m :request :request-m}]
      (with-redefs [grouper/create-random-grouper
                    #(when (= :requirement-m %)
                       (fn [rqst] (when (= :request-m rqst) :random-lot)))
                    evaluator/create-score-based-evaluator
                    #(when (= :request-m %)
                       (fn [group] (when (= :random-lot group) :lot)))
                    picker/high-score-picker
                    #(when (and (= [:score :value] %1)
                                (= [:lot :lot] %2))
                       :highest-scored-lot)]
        (t/is (= expected (sut/highest-scored-group-lot input 2)))))))

(t/deftest test-log-group-lot
  (t/testing "Returns provided lot"
    (t/is (= {:groups []} (sut/log-group-lot {:groups []})))))

(t/deftest test-group-lot->group-members
  (t/testing "Creates groups from provided param"
    (let [lot {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]}
          expected [[:mem1 :mem2] [:mem3 :mem4]]]
      (t/is (= expected (sut/group-lot->group-members lot))))))

(t/deftest test-combine-input
  (t/testing "Load history and assoc all input"
    (let [user-input {:requirement {:requirement1 "requirement1"}
                      :request {:request1 "request1"}}
          history #{:history1}
          expected {:requirement {:requirement1 "requirement1"}
                    :request {:request1 "request1" :history #{:history1}}}]
      (t/is (= expected (sut/combine-input user-input history))))))

(t/deftest test-generate-lot-with-high-score
  (t/testing "Generates group lot with highest score"
    (let [dep {:gen-count 10
               :history-port :port-impl}
          user-input {:requirement :requirement1
                      :request {:req1 "req1"}}
          input {:requirement :requirement1
                 :request {:req1 "req1" :history #{:history1 :history2}}}
          expected [[:mem1 :mem2] [:mem3 :mem4]]]
      (with-redefs [port/load-history #(when (= :port-impl %) #{:history1 :history2})
                    sut/highest-scored-group-lot
                    #(when (and (= input %1)
                                (= 10 %2))
                       {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]})
                    sut/log-group-lot (fn [lot] lot)]
        (t/is (= expected (sut/generate-lot-with-high-score dep user-input)))))))
