(ns mantura.core-test
  (:require [clojure.test :refer :all]
            [mantura.core :refer :all]))

(deftest test-success
  (testing "success"
    (is (-> 0 success (run "") (= {:state :success :content 0 :remaining nil})))))

(deftest test-fail
  (testing "fail"
    (is (-> 0 fail (run "") (= {:state :failure})))))

(deftest test-bind
  (testing "bind"
    (is (-> 48
            success
            (bind (fn [x] (success (char x))))
            (run "yeet")
            (= {:state :success :content \0 :remaining '(\y \e \e \t)})))
    (is (-> 48
            success
            (bind (fn [x] (fail (char x))))
            (run "yeet")
            (= {:state :failure})))
    (is (-> (fail)
            (bind (fn [x] (success (char x))))
            (run "yeet")
            (= {:state :failure})))))
