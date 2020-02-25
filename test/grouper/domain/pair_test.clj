(ns grouper.domain.pair-test
  (:require [clojure.test :as t]
             [grouper.domain.pair :as sut]))

(t/deftest test-pairs-ordered-by-mutuality
  (t/testing "Creates list of pairs ordered by mutuality of requests"
    (let [requests {:a '(:c :b :d)
                    :b '(:c :d)
                    :d '(:b :a)}
          expected '(#{:m1} #{:m2} #{:m3} #{:s1} #{:s2} #{:s3})]
      (with-redefs
        [shuffle
         #(let [target-set (set %)]
            (cond
              (= #{#{:a :d} #{:b :d}} target-set) '(#{:m1} #{:m2} #{:m3})
              (= #{#{:a :c} #{:a :b} #{:b :c}} target-set) '(#{:s1} #{:s2} #{:s3})
              :else (t/is false "shuffle with invalid param")))]
        (t/is (= expected (sut/pairs-ordered-by-mutuality requests)))))))
