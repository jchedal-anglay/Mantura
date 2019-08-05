(ns mantura.core-test
  (:require [clojure.test :refer :all]
            [mantura.core :refer :all]))

(deftest test-success
  (testing "success"
    (is (-> 0 success (run "") (= {:state :success :content 0 :remaining ()})))))

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
            (run "anything")
            (= {:state :failure})))
    (is (-> (fail)
            (bind (fn [x] (success (char x))))
            (run "anything")
            (= {:state :failure})))))

(deftest test-lift
  (testing "lift"
    (is (-> 0
            fail
            (lift #(%))
            (run "anything")
            (= {:state :failure})))
    (is (-> 0
            success
            (lift #(+ 2 %))
            (run "anything")
            (= (run (success 2) "anything"))))))

(deftest test-token
  (testing "token"
    (is (-> \X
            token
            (run "Xshouldremain")
            (= {:state :success :content \X :remaining (seq "shouldremain")})))
    (is (-> \X
            token
            (run "Yinvalid")
            (= {:state :failure})))))

(deftest test-tokens
  (testing "tokens"
    (is (-> (tokens "foo")
            (run "foobar")
            (= {:state :success :content (seq "foo") :remaining (seq "bar")})))
    (is (-> (tokens "")
            (run "anything")
            (= {:state :success :content () :remaining (seq "anything")})))
    (is (-> (tokens "abc")
            (run "foo")
            (= {:state :failure})))))

(deftest test-many
  (testing "many"
    (is (-> \a
            token
            many
            (run "aaaaab")
            (= {:state :success :content (seq "aaaaa") :remaining '(\b)})))
    (is (-> \a
            token
            many
            (run "aaa")
            (= {:state :success :content (seq "aaa") :remaining ()})))
    (is (-> \a
            token
            many
            (run "")
            (= {:state :success :content () :remaining ()})))
    ))
