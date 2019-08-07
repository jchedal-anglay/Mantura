(ns mantura.core-test
  (:require [clojure.test :refer :all]
            [mantura.core :refer :all]
            [mantura.parser :refer :all]
            [mantura.combinator :refer :all]))

(deftest test-success
  (testing "success"
    (is (-> 0 success (run "") (= {:state :success :content 0 :remaining ()})))))

(deftest test-fail
  (testing "fail"
    (is (-> 0 fail (run "") fail?))))

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
            fail?))
    (is (-> (fail)
            (bind (fn [x] (success (char x))))
            (run "anything")
            fail?))))

(deftest test-lift
  (testing "lift"
    (is (-> 0
            fail
            ((fn [p] (lift #(+ 2 %) p)))
            (run "anything")
            fail?))
    (is (-> 0
            success
            ((fn [p] (lift #(+ 2 %) p)))
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
            fail?))))

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
            fail?))))

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

(deftest test-some
  (testing "some"
    (is (-> \a
            token
            some
            (run "aaaaab")
            (= {:state :success :content (seq "aaaaa") :remaining '(\b)})))
    (is (-> \a
            token
            some
            (run "a")
            (= {:state :success :content (seq "a") :remaining ()})))
    (is (-> \a
            token
            some
            (run "")
            fail?))
    (is (-> \a
            token
            some
            (run "bbbaa")
            fail?))))

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

(deftest test-take-while
  (testing "take-while"
    (is (->
         (run (take-while #(= \0 %)) "0000b")
         (= {:state :success :content (seq "0000") :remaining (seq "b")})))
    (is (->
         (run (take-while #(= \0 %)) "a0000b")
         (= {:state :success :content () :remaining (seq "a0000b")})
         ))))

(deftest test-take-until
  (testing "take-until"
    (is (->
         (run (take-until #(not= \0 %)) "0000b")
         (= {:state :success :content (seq "0000") :remaining (seq "b")})))
    (is (->
         (run (take-until #(not= \0 %)) "a0000b")
         (= {:state :success :content () :remaining (seq "a0000b")})
         ))))

(deftest test-skip
  (testing "skip"
    (is (-> 0
            skip
            (run "")
            (= {:state :success :content nil :remaining ()})))
    (is (-> 0
            skip
            (run "foo")
            (= {:state :success :content nil :remaining (seq "foo")})))
    (is (-> 3
            skip
            (run "foobar")
            (= {:state :success :content nil :remaining (seq "bar")})))
    (is (-> 1
            skip
            (run "")
            fail?))
    (is (-> 10
            skip
            (run "abc")
            fail?))))

(deftest test-skip-while
  (testing "skip-while"
    (is (->
         (run (skip-while #(= \0 %)) "0000b")
         (= {:state :success :content nil :remaining (seq "b")})))
    (is (->
         (run (skip-while #(= \0 %)) "a0000b")
         (= {:state :success :content nil :remaining (seq "a0000b")})
         ))))

(deftest test-skip-until
  (testing "skip-until"
    (is (->
         (run (skip-until #(not= \0 %)) "0000b")
         (= {:state :success :content nil :remaining (seq "b")})))
    (is (->
         (run (skip-until #(not= \0 %)) "a0000b")
         (= {:state :success :content nil :remaining (seq "a0000b")})
         ))))
