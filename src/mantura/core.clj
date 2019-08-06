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

(def return success)

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

(defn lift
  [f p]
  (fn [input]
    (with-parser p input {content :content remaining :remaining}
        {:state :success
         :content (f content)
         :remaining remaining})))

(defn lift2
  [f p q]
  (fn [input]
    (with-parser p input {content :content remaining :remaining}
      (with-parser q remaining {content' :content remaining :remaining}
        {:state :success
         :content (f content content')
         :remaining remaining}))))

(defn fix
  [f]
  (declare --r)
  (defn ^:private --p [input] ((f --r) input))
  (defn ^:private --r [input]
    (--p input))
  --r)
