(ns grouper.gateway.history-test
  (:require  [clojure.test :as t]
             [integrant.core :as ig]
             [grouper.gateway.history :as sut]
             [grouper.util.csv :as csv]))

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
      (with-redefs [csv/load-lines #(when (= csv %) csv-lines)]
        (t/is (= expected (sut/to-history-from-csv csv)))))))

(t/deftest test-do-load-history
  (t/testing "Creates history with loaded csv files"
    (with-redefs
      [sut/get-history-files (fn [] '("a.csv" "b.csv"))
       sut/to-history-from-csv
       #(if (= "a.csv" %)
          #{:a1 :a2}
          (when (= "b.csv" %) #{:b1 :a1}))]
      (t/is (= #{:a1 :a2 :b1} (sut/do-load-history))))))

(t/deftest test-load-history
  (t/testing "Calls do-load-history with configuration"
    (let [f (ig/init-key :grouper.gateway.history/load-history {})]
      (with-redefs [sut/do-load-history (fn [] :history)]
        (t/is (= (f) :history))))))

