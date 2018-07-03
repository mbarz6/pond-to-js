(ns lexer.alt-core
  (:require [clojure.string :as str]
            [goog.string :refer [format]]))

(defn in?
  "Checks if a collection contains an element."
  [e collection]
  (some #{e} collection)
)

(defn reduce-indexed
  "Reduce with index, expects a function with arity [acc index x]"
  [f init coll]
  (reduce-kv f init (vec coll))
)

(defn split-string [re string]
  (nth
    (reduce 
      (fn [[string-acc list] v] (
        if (or (re-find re string-acc) (re-find re v)) 
        [(if (str/blank? v) "" v) (conj list string-acc)] 
        [(str string-acc v) list]
      ))
      ["" []] 
      (str string " ")
    )
    1
  )
)

(defn filter-empty [sequence]
  (filter (complement empty?) sequence)
)

(def operators [
    "==" "<" ">" "<=" ">="
    "+" "-" "*" "/" "^" "%"
  ]
)

(def seperators [
    "(" ")" ","
  ]
)

(def all-symbols (flatten [operators seperators]))

(def symbol-regex (->> all-symbols
  (map (partial interleave (repeat \\)))
  (map str/join)
  (str/join "|")
  (str "\\s|")
  re-pattern
))

(defn make-lexemes [source]
  (let [lines (filter-empty (str/split-lines source))
        indent (some (partial re-find #"^\s*") lines)
        indent-regex (some-> indent
                        (#(format" ^%s|%s(?=%s)|%s(?=[^\\s])" % % % %))
                        re-pattern
                      )]
    (->> lines
      (map (fn [line]
        (concat
          (if indent-regex
            (-> indent-regex
              (re-seq line)
              count
              vector
            )
            [0]
          )
          (->> line
            (split-string symbol-regex)
            filter-empty
          )
        )
      ))
    )
  )
)

(defn nest
  [lexemes]
  (nest lexemes 0)

  [lexemes default-indent]
  (nth (reduce-indexed (fn [[last-indent indented-at result] [indent :as all] i] 
    (if indented-at
      (let [[next-indent] (nth lexemes (+ 1 i) nil)]
        (if
          (or (= next-indent default-indent) (nil? next-indent))
          [indent nil (conj result (nest (subvec indented-at i) (+ 1 default-indent)))]
          [indent indented-at result]
        )
      ) 

      (cond
        (>= indent (+ default-indent 2))
        (throw (Exception. (format "You can't indent %i times! Only one at a time, please." (- indent default-indent))))

        (= indent (+ default-indent 1))
        [indent i result]

        :else
        [indent indented-at (conj result all)]
      )
    )
  ) [0 nil []] lexemes) 2)
)

(defn make-tokens 
  [lexemes]
  (make-tokens lexemes { :type :expression [] } )

  [lexemes token]
  (reduce 
    (fn []

    )
    lexemes
  )
)

(defn transpile [tokens]
  (tokens)
)

(defn main [source]
  (-> source
    make-lexemes
    nest
    make-tokens
    transpile
  )
)
