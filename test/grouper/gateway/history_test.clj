(ns grouper.gateway.history-test
  (:require  [clojure.test :as t]
             [integrant.core :as ig]
             [grouper.gateway.history :as sut]
             [grouper.util.csv :as csv]))

(t/deftest test-available-history-files
  (t/testing "Returns collection of available history file names"
    (with-redefs [sut/file-names-on-current-dir
                  (fn [] '("groups-1.csv" "invalid.csv" "groups-2.non-csv" "groups-3.csv"))]
      (t/is (= '("groups-1.csv" "groups-3.csv") (sut/available-history-files))))))

(t/deftest test-csv->history
  (t/testing "Creates history from provided csv"
    (let [csv-lines [["a" "b" "c"] ["d" "e" "f"]]
          expected #{#{"a" "b" "c"} #{"d" "e" "f"}}]
      (t/is (= expected (sut/csv->history csv-lines))))))

(t/deftest test-load-history
  (t/testing "Calls do-load-history with configuration"
    (let [f (ig/init-key :grouper.gateway.history/get-history {})
          csv-lines1 [["a" "b" "c"] ["d" "e" "f"]]
          csv-lines2 [["e" "a" "b"] ["f" "c" "d"]]
          expected #{#{"a" "b" "c"} #{"d" "e" "f"} #{"e" "a" "b"} #{"f" "c" "d"}}]
      (with-redefs [sut/file-names-on-current-dir
                    (fn [] ["groups-1.csv" "invalid.csv" "groups-2.csv"])
                    csv/load-lines #(case % "groups-1.csv" csv-lines1
                                            "groups-2.csv" csv-lines2)]
        (t/is (= expected (f)))))))


