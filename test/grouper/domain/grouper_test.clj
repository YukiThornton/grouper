(ns grouper.domain.grouper-test
  (:require [grouper.domain.grouper :as sut]
            [clojure.test :as t]))

(t/deftest test-group-sizes
  (t/testing "Returns sizes of group in vevtor"
    (t/is (= [4 4 4] (sut/group-sizes 12 3)))
    (t/is (= [4 4 3] (sut/group-sizes 11 3)))
    (t/is (= [4 3 3] (sut/group-sizes 10 3)))))

(t/deftest test-sbvec-sizes
  (t/testing "Returns [start end] indeces of provided group-sizes"
    (t/is (= [[0 5] [5 9] [9 11]] (sut/subvec-indeces [5 4 2])))))

(t/deftest test-create-random-grouper
  (t/testing "Returns grouper that creates specified amount of groups randomly"
    (let [expected {:groups [{:members [:sa :sb :sc :sd]}
                             {:members [:se :sf :sg]}]}
          members [:a :b :c :d :e :f :g]
          requirement {:members members
                       :group-count 2}
          shuffled-members [:sa :sb :sc :sd :se :sf :sg]]
      (with-redefs [shuffle #(when (= members %) shuffled-members)]
        (t/is (= expected ((sut/create-random-grouper requirement) {})))))))
