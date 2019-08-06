(ns mantura.parsers
  (:require [mantura.core :as mantura]))

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

(defn choice
  "Return the first succeeding parser in the list or fails"
  ([]
   (fn [_] {:state :failure}))
  ([parser & rest]
   (fn [input]
     (let [{state :state :as -p} (parser input)]
       (if (= :success state)
         -p
         ((apply choice rest) input))))))

(defn token
  "Return a parser that simply checks for equality with the first element of input"
  [tok]
  (fn [input]
    (if (= tok (first input))
      {:state :success :content tok :remaining (rest input)}
      {:state :failure})))

(defn ^:private -sequence [[parser & rest :as parsers] input]
  (if (empty? parsers)
    ()
    (let [{remaining :remaining :as parsed} (run parser input)]
      (cons parsed (if (fail? parsed) () (-sequence rest remaining))))))

(defn sequence
  "Apply the parsers in order"
  [& parsers]
  (fn [input]
    (let [seq (-sequence parsers input)]
      (if (some fail? seq)
        {:state :failure}
        {:state :success
         :content (map :content seq)
         :remaining (if (not-empty seq) (-> seq last :remaining) input)}))))

(defn n-times
  "Run a parser n times"
  [n parser]
  (apply sequence (take n (cycle [parser]))))

(defn take
  "Take exactly n elements of input"
  [n]
  (n-times n anything))

(defn tokens
  "Return a parser that checks if the input starts with all the parameters"
  [toks]
  (apply sequence (map token (seq toks))))

(defn many
  "Apply a parser until it fails"
  [parser]
  (fix (fn [self] (choice (lift2 cons parser (choice self (success ()))) (success ())))))

(defn many1
  "Apply a parser until it fails, fails if there is 0 elements"
  [parser]
  (fix (fn [self] (lift2 cons parser (choice self (success ()))))))
