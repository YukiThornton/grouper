(ns grouper.usecase.group-test
  (:require [grouper.usecase.group :as sut]
            [grouper.domain.grouper :as grouper]
            [grouper.domain.evaluator :as evaluator]
            [grouper.domain.picker :as picker]
            [integrant.core :as ig]
            [clojure.test :as t]))

(t/deftest test-combine-evaluators
  (t/testing "Combine evaluators to create a function"
    (let [evaluator1 (fn [g] (when (= :group1 g) :group2))
          evaluator2 (fn [g] (when (= :group2 g) :group3))
          evaluator3 (fn [g] (when (= :group3 g) :group4))
          combined-fn (sut/combine-evaluators [evaluator1 evaluator2 evaluator3])]
      (t/is (= :group4 (combined-fn :group1))))))

(t/deftest test-combine-grouper&evaluators
  (t/testing "Combine grouper and evaluators to create a function"
    (let [request {:request {}}
          grouper (fn [r] (when (= request r) :group1))
          combined-evaluators (fn [g] (when (= :group1 g) :group2))
          evaluators [:evaluator-fn1 :evaluator-fn2]]
      (with-redefs [sut/combine-evaluators (fn [evals] (when (= evaluators evals) combined-evaluators))]
        (let [combined-fn (sut/combine-grouper&evaluators grouper evaluators)]
          (t/is (= :group2 (combined-fn request))))))))

(t/deftest test-create-lot-generator
  (t/testing "Creates a function that returns a single lot"
    (let [expected :lot
          fn1 (fn [req] (when (= :request req) :lot))
          target-fn (sut/create-lot-generator fn1 :request)]
      (t/is (= expected (target-fn))))))

(t/deftest test-map-times
  (t/testing "Invoke function specified times and map return values"
    (t/is (= [:a :a :a] (sut/map-times 3 (fn [] :a))))))

(t/deftest test-highest-scored-group-lot
  (t/testing "Creates lots of random groups and returns one with highest score"
    (let [expected :highest-scored-lot
          requirement :requirement
          request :request]
      (with-redefs [grouper/random-grouper
                    #(when (= requirement %)
                       :grouper-fn)
                    evaluator/score-based-evaluator
                    #(when (= request %)
                       :evaluator-fn)
                    sut/combine-grouper&evaluators
                    #(when (and (= :grouper-fn %1)
                                (= :evaluator-fn (first %2)))
                       :combined-f)
                    sut/create-lot-generator
                    #(when (and (= :combined-f %1)
                                (= :request %2))
                       :generator-f)
                    sut/map-times
                    #(when (and (= 1000 %1)
                                (= :generator-f %2))
                       :group-lots)
                    picker/high-score-picker
                    #(when (= [:score :value] %)
                       (fn [group-lots] (when (= :group-lots group-lots) :highest-scored-lot)))]
        (t/is (= expected (sut/highest-scored-group-lot requirement request 1000)))))))

(t/deftest test-highest-of-random
  (t/testing "Call highest-scored-group-lot with configuration"
    (let [f (ig/init-key :grouper.usecase.group/highest-of-random {:gen-count 10})]
      (with-redefs [sut/highest-scored-group-lot #(when (and (= :requirement %1)
                                                             (= :request %2)
                                                             (= 10 %3))
                                                    :expected)]
        (t/is (= :expected (f :requirement :request)))))))
