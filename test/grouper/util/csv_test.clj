(ns grouper.util.csv-test
  (:require  [clojure.test :as t]
             [grouper.util.csv :as sut]))

(t/deftest test-load-lines
  (t/testing "Load lines of given csv"
    (let [csv "abc.csv"
          csv-string "aaa,bbb,ccc\nddd,eee,fff\n"
          expected '(("aaa" "bbb" "ccc")
                     ("ddd" "eee" "fff"))]
      (with-redefs [slurp #(when (= csv %) csv-string)]
        (t/is (= expected (sut/load-lines csv)))))))

(t/deftest test-seq->csv-lines
  (t/testing "Returns csv string of provided collection"
    (t/is (= "a,b,c\nd,e,f\n" (sut/seq->lines '(("a" "b" "c") ("d" "e" "f")))))))
