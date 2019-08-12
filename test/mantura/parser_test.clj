(ns mantura.core-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [mantura.core :as core]
            [mantura.parser :as parser]))

(defspec test-peek
  20
  (prop/for-all
   [input (gen/list gen/any)]
   (= (core/run parser/peek input)
      (if (empty? input)
        {:state :failure}
        {:state :success :content (first input) :remaining input}))))

(defspec test-anything
  20
  (prop/for-all
   [input (gen/list gen/any)]
   (= (core/run parser/anything input)
      (if (empty? input)
        {:state :failure}
        {:state :success :content (first input) :remaining (rest input)}))))

(defspec test-token
  20
  (prop/for-all
   [input (gen/list (gen/elements [0 1 2 3 4]))
    tok (gen/elements [0 1 2 3 4])]
   (= (core/run (parser/token tok) input)
      (if (and (not-empty input) (= (first input) tok))
        {:state :success :content tok :remaining (rest input)}
        {:state :failure}))))
