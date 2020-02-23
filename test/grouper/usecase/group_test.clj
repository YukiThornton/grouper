(ns grouper.usecase.group-test
  (:require [grouper.usecase.group :as sut]
            [clojure.test :as t]))


#_(t/deftest test-create-pair-list
  (t/testing "Returns a list of pairs"
    (let [input {:a '(:d :b :e)
                 :b '(:c :a :d)
                 :c '(:b)}
          expected '(#{:b :c} #{:a :d} #{:a :e} #{:a :b})])))

(t/deftest test-make-groups
  (t/testing "Returns groups"
    (let [param {:group-count 2
                 :members #{:a :b :c :d :e}
                 :group-requests {:a '(:d) :b '(:c)}
                 :block-requests {:a '(:b :c)}
                 :history #{#{:a :b :c} #{:d :e}}}
          migrated-param {:groups '(() () () () ())
                          :pairs '((:a :d) (:b :c))
                          :members '(:a :b :c :d)
                          :blocks {:a '(:b :c)}}
          created-groups #{#{:a :d} #{:b :c}}]
      (with-redefs
        [sut/migrate-param (fn [p] (when (= param p) migrated-param))
         sut/create-groups-r
         (fn [groups pairs members block]
           (when (and (= (:groups migrated-param) groups)
                      (= (:pairs migrated-param) pairs)
                      (= (:members migrated-param) members)
                      (= (:block migrated-param) block))
             created-groups))]
        (t/is (= created-groups (sut/make-groups param)))))))
