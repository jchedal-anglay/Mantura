(ns mantura.core)

(defn ^:private seq [sequence] (or (clojure.core/seq sequence) ()))

(defn run
  "Run a parser on a `seq`able input"
  [parser input]
  (parser (seq input)))

(defn success
  "Return a parser that always succeeds with its argument as content"
  [content]
  (fn [input]
    {:state :success :content content :remaining input}))

(defn fail
  "Return a parser that always fails"
  [& rest]
  (fn [_]
    {:state :failure}))

(defn success?
  "Return true if the parsed result succeeded"
  [parsed]
  (-> parsed :state (= :success)))

(defn fail?
  "Return true if the parsed result failed"
  [parsed]
  (-> parsed :state (= :failure)))

(defmacro ^:private with-parser
  "Helper macro, run body if parser succeeds"
  [parser input binding-map & body]
  `(let [parser# (run ~parser ~input)]
     (if (fail? parser#)
       parser#
       (let [~binding-map parser#]
         ~@body))))

(defn ^:private -bind
  [parser f]
  (fn [input]
    (with-parser parser input {content :content remaining :remaining}
      ((f content) remaining))))

(defn bind
  "Monadic bind, apply a function on the content of a succeeding parser
  Otherwise do nothing
  The function must take an argument with the type of the content of the parser and return a new parser"
  ([]
   (fn [_] {:state :failure}))
  ([parser & fs]
   (reduce -bind parser fs)))

(defn ^:private -lift
  [parser f]
  (bind parser #(-> % f success)))

(defn lift
  "Functor lift, simply apply a function on the content of a succeeding parser
  Otherwise do nothing"
  ([]
   (fn [_] {:state :failure}))
  ([parser & fs]
   (reduce -lift parser fs)))

(defn peek
  "Return the first char in the input, doesn't advance the stream
  Used for lookaheads"
  [input]
  (if (empty?)
    {:state :failure}
    {:state :success :content (first input) :remaining input}))

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

(defn tokens
  "Return a parser that checks if the input starts with all the parameters"
  [toks]
  (apply sequence (map token (seq toks))))

(defn ^:private -many [parser input]
  (let [{remaining :remaining :as parsed} (parser input)]
    (if (fail? parsed)
      ()
      (cons parsed (-many parser remaining)))))

(defn many
  "Apply a parser until it fails"
  [parser]
  (fn [input]
    (let [seq (-many parser input)]
      {:state :success
       :content (map :content seq)
       :remaining (if (not-empty seq) (-> seq last :remaining) input)})))

(defn many1
  "Apply a parser until it fails, return a failure if there is no match"
  [parser]
  (fn [input]
    (let [seq (-many parser input)]
      (if (empty? seq)
        {:state :failure}
        {:state :success
         :content (map :content seq)
         :remaining (if (not-empty seq) (-> seq last :remaining) input)}))))
