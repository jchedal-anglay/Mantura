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
    (is (-> \a
            token
            many
            (run "bbbaa")
            (= {:state :success :content () :remaining (seq "bbbaa")})))))

(deftest test-many1
  (testing "many1"
    (is (-> \a
            token
            many1
            (run "aaaaab")
            (= {:state :success :content (seq "aaaaa") :remaining '(\b)})))
    (is (-> \a
            token
            many1
            (run "a")
            (= {:state :success :content (seq "a") :remaining ()})))
    (is (-> \a
            token
            many1
            (run "")
            (= {:state :failure})))
    (is (-> \a
            token
            many1
            (run "bbbaa")
            (= {:state :failure})))))

(deftest test-take
  (testing "take"
    (is (-> 0
            take
            (run "")
            (= {:state :success :content () :remaining ()})))
    (is (-> 0
            take
            (run "foo")
            (= {:state :success :content () :remaining (seq "foo")})))
    (is (-> 3
            take
            (run "foobar")
            (= {:state :success :content (seq "foo") :remaining (seq "bar")})))
    (is (-> 1
            take
            (run "")
            fail?))
    (is (-> 10
            take
            (run "abc")
            fail?))))
