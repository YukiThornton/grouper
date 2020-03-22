(ns grouper.domain.user-input-test
  (:require [grouper.domain.user-input :as sut]
            [clojure.test :as t]))

(t/deftest test-remove-invalid-requests
  (t/testing "Remove '-' and 'invalid' from given requests"
    (let [input {:a [:d "-" "invalid":c]
                 :b ["-" :c :a "invalid"]}
          expected {:a [:d :c]
                    :b [:c :a]}]
      (t/is (= expected (sut/remove-invalid-requests input))))))

(t/deftest test-seq->requests
  (t/testing "Creates requests from seq"
    (let [input [[:a :d :c]
                 [:b :c :a]]
          expected {:a [:d :c]
                    :b [:c :a]}]
      (t/is (= expected (sut/seq->requests input))))))

(t/deftest test-seq->members
  (t/testing "Creates member set from seq"
    (let [input [[:a] [:b :d] [:c]]
          expected #{:a :b :c}]
      (t/is (= expected (sut/seq->members input))))))

(t/deftest test-->user-input
  (t/testing "Creates user input from given param"
    (let [group-request-vec [[:a :b]
                             [:b :a :c]
                             [:c :a]]
          block-request-vec [[:a :c]
                             [:c :b]]
          expected {:requirement {:group-count 3
                                  :members #{:a :b :c}}
                    :request {:group-requests {:a [:b]
                                               :b [:a :c]
                                               :c [:a]}
                              :block-requests {:a [:c]
                                               :c [:b]}}}]
      (t/is (= expected (sut/->user-input group-request-vec block-request-vec 3)))))

  (t/testing "Skips invalid requests"
    (let [group-request-vec [[:a "-" :b]
                             [:b :a "invalid":c]
                             [:c :a "-" "invalid"]]
          block-request-vec [[:a "-" :c]
                             [:c :b "invalid"]]
          expected {:requirement {:group-count 3
                                  :members #{:a :b :c}}
                    :request {:group-requests {:a [:b]
                                               :b [:a :c]
                                               :c [:a]}
                              :block-requests {:a [:c]
                                               :c [:b]}}}]
      (t/is (= expected (sut/->user-input group-request-vec block-request-vec 3))))))
