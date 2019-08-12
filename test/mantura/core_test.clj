(ns mantura.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [mantura.core :as core]))

(defspec test-success
  20
  (prop/for-all
   [content gen/any
    input (gen/list gen/any)]
   (= (core/run (core/success content) input) {:state :success :content content :remaining input})))

(defspec test-return
  20
  (prop/for-all
   [content gen/any
    input (gen/list gen/any)]
   (= (core/run (core/return content) input) {:state :success :content content :remaining input})))

(defspec test-fail
  20
  (prop/for-all
   [content gen/any
    input (gen/list gen/any)]
   (= (core/run (core/fail content) input) {:state :failure})))
