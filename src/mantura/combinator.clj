(ns mantura.combinator
  (:require [mantura.core :as mantura]))

(defn predicate
  "Apply a parser, return it with original input"
  [parser]
  (fn [input]
    (let [{state :state content :content :as parsed} (parser input)]
      (if (= state :failure)
        parsed
        {:state state :content nil :remaining input}))))

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

(defn some
  "Apply a parser until it fails, fails if there is 0 elements"
  [parser]
  (mantura/lift2 cons parser (many parser)))

(defn times
  "Run a parser n times"
  [n parser]
  (apply sequence (take n (cycle [parser]))))
