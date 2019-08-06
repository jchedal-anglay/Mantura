(ns mantura.parser
  (:require [mantura.core :as mantura]
            [mantura.combinator]))

(defn peek
  "Return the first char in the input, doesn't advance the stream
  Used for lookaheads"
  [input]
  (if (empty? input)
    {:state :failure}
    {:state :success :content (first input) :remaining input}))

(defn anything
  "Return the first char in the input, consumes the stream"
  [input]
  (if (empty? input)
    {:state :failure}
    {:state :success :content (first input) :remaining (rest input)}))

(defn token
  "Return a parser that simply checks for equality with the first element of input"
  [tok]
  (fn [input]
    (if (= tok (first input))
      {:state :success :content tok :remaining (rest input)}
      {:state :failure})))

(defn tokens
  "Return a parser that checks if the input starts with all the parameters"
  [toks]
  (apply mantura.combinator/sequence (map token (or (seq toks) ()))))

(defn take
  "Take exactly n elements of input"
  [n]
  (mantura.combinator/n-times n anything))
