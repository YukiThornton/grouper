(ns grouper.cli.group-test
  (:require [grouper.cli.group :as sut]
            [integrant.core :as ig]
            [clojure.test :as t]))

(t/deftest test-load-csv-lines
  (t/testing "Load lines of given csv"
    (let [csv "abc.csv"
          csv-string "aaa,bbb,ccc\nddd,eee,fff\n"
          expected '(("aaa" "bbb" "ccc")
                     ("ddd" "eee" "fff"))]
      (with-redefs [slurp #(when (= csv %) csv-string)]
        (t/is (= expected (sut/load-csv-lines csv)))))))

(t/deftest test-to-requests
  (t/testing "Creates group requests from provided csv"
    (let [csv "abc.csv"
          csv-lines '(("a" "d" "invalid" "c" "-")
                      ("b" "c" "a"))
          expected {"a" '("d" "c")
                    "b" '("c" "a")}]
      (with-redefs [sut/load-csv-lines (fn [c] (when (= csv c) csv-lines))]
        (t/is (= expected (sut/to-requests csv)))))))

(t/deftest test-to-members
  (t/testing "Creates member set from provided csv"
    (let [csv "abc.csv"
          csv-lines '(("a") ("b" "d") ("c"))
          expected #{"a" "b" "c"}]
      (with-redefs [sut/load-csv-lines #(when (= csv %) csv-lines)]
        (t/is (= expected (sut/to-members csv)))))))

(t/deftest test-get-history-files
  (t/testing "Returns collection of history file names"
    (with-redefs [sut/current-dir-file-names
                  (fn [] '("groups-1.csv" "invalid.csv" "groups-2.non-csv" "groups-3.csv"))]
      (t/is (= '("groups-1.csv" "groups-3.csv") (sut/get-history-files))))))

(t/deftest test-to-history-from-csv
  (t/testing "Creates history from provided csv"
    (let [csv "abc.csv"
          csv-lines '(("a" "b" "c") ("d" "e" "f"))
          expected #{#{"a" "b" "c"} #{"d" "e" "f"}}]
      (with-redefs [sut/load-csv-lines #(when (= csv %) csv-lines)]
        (t/is (= expected (sut/to-history-from-csv csv)))))))

(t/deftest test-load-history
  (t/testing "Creates history with loaded csv files"
    (with-redefs
      [sut/get-history-files (fn [] '("a.csv" "b.csv"))
       sut/to-history-from-csv
       #(if (= "a.csv" %)
          #{:a1 :a2}
          (when (= "b.csv" %) #{:b1 :a1}))]
      (t/is (= #{:a1 :a2 :b1} (sut/load-history))))))

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
                    :block-requests "BLOCK_REQUESTS"
                    :history "HISTORY"}]
      (with-redefs [sut/to-requests
                    #(if (= group-csv %)
                       "GROUP_REQUESTS"
                       (when (= block-csv %) "BLOCK_REQUESTS"))
                    sut/load-history (fn [] "HISTORY")]
        (t/is (= expected (sut/to-grouping-request param)))))))

(t/deftest test-to-groups
  (t/testing "Creates groups from provided param"
    (let [create-groups-fn
          #(when (and (= :requirement %1) (= :request %2))
             {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]})
          expected [[:mem1 :mem2] [:mem3 :mem4]]]
      (with-redefs [sut/to-grouping-requirement #(when (= :param %) :requirement)
                    sut/to-grouping-request #(when (= :param %) :request)]
        (t/is (= expected (sut/to-groups create-groups-fn :param)))))))

(t/deftest test-to-csv-lines
  (t/testing "Returns csv string of provided collection"
    (t/is (= "a,b,c\nd,e,f\n" (sut/to-csv-lines '(("a" "b" "c") ("d" "e" "f")))))))

(t/deftest test-output-file-name
  (t/testing "Returns unique file name for output file"
    (with-redefs [sut/unique-file-suffix (fn [] "2020-02-23-88888")]
      (t/is (= "groups-2020-02-23-88888.csv" (sut/output-file-name))))))

(t/deftest test-write-groups-to-csv
  (t/testing "Creates a csv file with groups"
    (with-redefs
      [sut/to-groups #(when (and (= :create-groups-fn %1) (= :param %2)) :groups)
       sut/to-csv-lines #(when (= :groups %) :csv-string)
       sut/spit-to-csv #(when (= :csv-string %) :expected)]
      (t/is (= :expected (sut/write-groups-to-csv :create-groups-fn :param))))))

(t/deftest test-write-groups
  (t/testing "Calls write-groups-to-csv with configuration"
    (let [f (ig/init-key :grouper.cli.group/write-groups {:create-groups :create-groups-fn})]
      (with-redefs [sut/write-groups-to-csv #(when (and (= :create-groups-fn %1)
                                                        (= :param %2))
                                               :expected)]
        (t/is (= :expected (f :param)))))))
