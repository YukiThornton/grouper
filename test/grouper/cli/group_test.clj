(ns grouper.cli.group-test
  (:require [grouper.cli.group :as sut]
            [grouper.util.csv :as csv]
            [grouper.domain.user-input :as user-input]
            [integrant.core :as ig]
            [clojure.test :as t]))

(t/deftest test-create-group-lot
  (t/testing "Creates group lot from provided param"
    (let [param {:group-count "3"
                 :group-requests :group-csv
                 :block-requests :block-csv}
          expected {:groups [{:members [:mem1 :mem2]} {:members [:mem3 :mem4]}]}
          create-groups-fn #(when (= :user-input %) expected)]
      (with-redefs [csv/load-lines #(if (= :group-csv %)
                                      :group-csv-lines
                                      (when (= :block-csv %) :block-csv-lines))
                    user-input/->user-input
                    #(when (and (= :group-csv-lines %1)
                                (= :block-csv-lines %2)
                                (= 3 %3))
                       :user-input)]
        (t/is (= expected (sut/create-group-lot create-groups-fn param)))))))

(t/deftest test-gen-unique-file-name
  (t/testing "Returns unique file name for output file"
    (with-redefs [sut/unique-file-suffix (fn [] "2020-02-23-88888")]
      (t/is (= "groups-2020-02-23-88888.csv" (sut/gen-unique-file-name "groups-" ".csv"))))))

(t/deftest test-write-groups
  (t/testing "Calls write-groups-to-csv with configuration"
    (let [f (ig/init-key :grouper.cli.group/write-groups {:create-groups :create-groups-fn})]
      (with-redefs [sut/create-group-lot
                    #(when (and (= :create-groups-fn %1) (= :param %2)) :group-members)
                    csv/to-lines #(when (= :group-members %) :csv-string)
                    sut/gen-unique-file-name #(when (and (= "groups-" %1)
                                                         (= ".csv" %2))
                                                :file-name)
                    spit #(when (and (= :file-name %1)
                                     (= :csv-string %2))
                            :expected)]
        (t/is (= :expected (f :param)))))))
