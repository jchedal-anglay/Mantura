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
        {:state :success :content (first input) :remaining (seq input)}))))
