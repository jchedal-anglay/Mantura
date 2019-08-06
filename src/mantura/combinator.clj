(ns mantura.combinator
  (:require [mantura.core :as mantura]))

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

(defn ^:private -sequence [[parser & rest :as parsers] input]
  (if (empty? parsers)
    ()
    (let [{remaining :remaining :as parsed} (parser input)]
      (cons parsed (if (mantura/fail? parsed) () (-sequence rest remaining))))))

(defn sequence
  "Apply the parsers in order"
  [& parsers]
  (fn [input]
    (let [seq (-sequence parsers input)]
      (if (some mantura/fail? seq)
        {:state :failure}
        {:state :success
         :content (map :content seq)
         :remaining (if (not-empty seq) (-> seq last :remaining) input)}))))

(defn many
  "Apply a parser until it fails"
  [parser]
  (mantura/fix
   (fn [self] (choice
               (mantura/lift2 cons parser (choice self (mantura/success ())))
               (mantura/success ())))))

(defn many1
  "Apply a parser until it fails, fails if there is 0 elements"
  [parser]
  (mantura/fix (fn [self] (mantura/lift2 cons parser (choice self (mantura/success ()))))))

(defn n-times
  "Run a parser n times"
  [n parser]
  (apply sequence (take n (cycle [parser]))))
