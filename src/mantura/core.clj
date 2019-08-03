(ns mantura.core)

(defn run [parser input]
  (parser (seq input)))

(defmacro ^:private with-parser
  [parser input binding-map & body]
  `(let [parser# (run ~parser ~input)]
     (if (-> parser# :state (= :failure))
       parser#
       (let [~binding-map parser#]
         ~@body))))

(defn success [content]
  (fn [input]
    {:state :success :content content :remaining input}))

(defn fail [& rest]
  (fn [_]
    {:state :failure}))

(defn ^:private -bind
  [parser f]
  (fn [input]
    (with-parser parser input {content :content remaining :remaining}
      (with-parser (f content) remaining {content- :content remaining- :remaining}
        {:state :success :content content- :remaining remaining-}))))

(defn bind
  ([]
   (fn [_] {:state :failure}))
  ([parser & fs]
   (reduce -bind parser fs)))

(defn ^:private -lift
  [parser f]
  (bind parser #(-> % f success)))

(defn lift
  ([]
   (fn [_] {:state :failure}))
  ([parser & fs]
   (reduce -lift parser fs)))

(defn or
  ([]
   (fn [_] {:state :failure}))
  ([parser & rest]
   (fn [input]
     (let [{state :state :as -p} (run parser input)]
       (if (= :success state)
         -p
         (run (apply or rest) input))))))

(defn token
  [tok]
  (fn [input]
    (if (= tok (first input))
      {:state :success :content tok :remaining (rest input)}
      {:state :failure})))
