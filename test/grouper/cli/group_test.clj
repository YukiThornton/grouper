(ns grouper.cli.group-test
  (:require [grouper.cli.group :as sut]
            [grouper.util.csv :as csv]
            [integrant.core :as ig]
            [clojure.test :as t]))

(t/deftest test-to-requests
  (t/testing "Creates group requests from provided csv"
    (let [csv "abc.csv"
          csv-lines '(("a" "d" "invalid" "c" "-")
                      ("b" "c" "a"))
          expected {"a" '("d" "c")
                    "b" '("c" "a")}]
      (with-redefs [csv/load-lines (fn [c] (when (= csv c) csv-lines))]
        (t/is (= expected (sut/to-requests csv)))))))

(t/deftest test-to-members
  (t/testing "Creates member set from provided csv"
    (let [csv "abc.csv"
          csv-lines '(("a") ("b" "d") ("c"))
          expected #{"a" "b" "c"}]
      (with-redefs [csv/load-lines #(when (= csv %) csv-lines)]
        (t/is (= expected (sut/to-members csv)))))))

(t/deftest test-to-grouping-requirement
  (t/testing "Creates grouping requirement from provided input"
    (let [param {:group-count "7"
                 :group-requests :group-csv}
          expected {:group-count 7
                    :members "MEMBERS"}]
      (with-redefs [sut/to-members #(when (= :group-csv %) "MEMBERS")]
        (t/is (= expected (sut/to-grouping-requirement param)))))))

(t/deftest test-to-grouping-request
  (t/testing "Creates grouping request from provided input"
    (let [group-csv "group.csv"
          block-csv "block.csv"
          param {:group-requests group-csv
                 :block-requests block-csv}
          expected {:group-requests "GROUP_REQUESTS"
                    :block-requests "BLOCK_REQUESTS"}]
      (with-redefs [sut/to-requests
                    #(if (= group-csv %)
                       "GROUP_REQUESTS"
                       (when (= block-csv %) "BLOCK_REQUESTS"))]
        (t/is (= expected (sut/to-grouping-request param)))))))

(t/deftest test-create-group-lot
  (t/testing "Creates group lot from provided param"
    (let [expected {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]}
          create-groups-fn
          #(when (and (= :requirement %1) (= :request %2))
             expected)]
      (with-redefs [sut/to-grouping-requirement #(when (= :param %) :requirement)
                    sut/to-grouping-request #(when (= :param %) :request)]
        (t/is (= expected (sut/create-group-lot create-groups-fn :param)))))))

(t/deftest test-log-group-lot
  (t/testing "Returns provided lot"
    (t/is (= {:groups []} (sut/log-group-lot {:groups []})))))

(t/deftest test-to-group-members
  (t/testing "Creates groups from provided param"
    (let [lot {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]}
          expected [[:mem1 :mem2] [:mem3 :mem4]]]
      (t/is (= expected (sut/to-group-members lot))))))

(t/deftest test-create-group-members-with-logging
  (with-redefs
    [sut/create-group-lot #(when (and (= :create-groups-fn %1) (= :param %2)) :lot)
     sut/log-group-lot #(when (= :lot %) :lot)
     sut/to-group-members #(when (= :lot %) :group-members)]
    (t/is (= :group-members
             (sut/create-group-members-with-logging :create-groups-fn :param)))))

(t/deftest test-output-file-name
  (t/testing "Returns unique file name for output file"
    (with-redefs [sut/unique-file-suffix (fn [] "2020-02-23-88888")]
      (t/is (= "groups-2020-02-23-88888.csv" (sut/output-file-name))))))

(t/deftest test-write-group-members-to-csv
  (t/testing "Creates a csv file with groups"
    (with-redefs
      [sut/create-group-members-with-logging
       #(when (and (= :create-groups-fn %1) (= :param %2)) :group-members)
       csv/to-lines #(when (= :group-members %) :csv-string)
       sut/spit-to-csv #(when (= :csv-string %) :expected)]
      (t/is (= :expected (sut/write-group-members-to-csv :create-groups-fn :param))))))

(t/deftest test-write-groups
  (t/testing "Calls write-groups-to-csv with configuration"
    (let [f (ig/init-key :grouper.cli.group/write-groups {:create-groups :create-groups-fn})]
      (with-redefs [sut/write-group-members-to-csv #(when (and (= :create-groups-fn %1)
                                                        (= :param %2))
                                               :expected)]
        (t/is (= :expected (f :param)))))))
