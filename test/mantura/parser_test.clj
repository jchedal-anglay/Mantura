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

;; TODO: test-tokens

(defspec test-take
  20
  (prop/for-all
   [[input size]
    (gen/let
        [input (gen/list gen/any)
         size (gen/fmap #(if (= 0 (count input)) 0 (+ 1 (mod % (count input)))) gen/int)]
      [input size])]
   (let [size-too-big (+ 1 (count input))]
     (and

      ;; Check (take 0)'s idempotence
      (= (core/run (parser/take 0) input)
         (core/run (parser/take 0) (:remaining (core/run (parser/take 0) input))))

      (= (core/run (parser/take size-too-big) input)
         {:state :failure})
      (= (core/run (parser/take size) input)
         {:state :success :content (take size input) :remaining (drop size input)})))))
