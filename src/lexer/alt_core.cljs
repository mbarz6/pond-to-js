(ns lexer.alt-core
  (:require [clojure.string :as str]
            [goog.string :refer [format]]))

(defn in?
  "Checks if a collection contains an element."
  [e collection]
  (some #{e} collection)
)

(defn split-string [re string]
  (nth
    (reduce 
      (fn [[string-acc list] v] (
        if (re-find re v) 
        [(if (str/blank? v) "" v) (conj list string-acc)] 
        [(str string-acc v) list]
      ))
      ["" []] 
      (str string " ")
    )
    1
  )
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
  (let [lines (str/split-lines source)
        indent (some (partial re-find #"^\s") lines)
        indent-regex (some->> indent
                       (format "^%1$s|%1$s(?=%1$s)|%1$s(?=[^\\s])")
                        re-pattern
                      )]
    (->> lines
      (map (fn [line]
        (concat
          (some-> indent-regex
            (re-seq line)
            count
            (repeat :indent)
          )
          (->> line
            (split-string symbol-regex)
            (filter (complement empty?))
          )
        )
      ))
    )
  )
)

(defn make-tokens [lexemes]
  (lexemes)
)

(defn transpile [tokens]
  (tokens)
)

(defn main [source]
  (-> source
    make-lexemes
    make-tokens
    transpile
  )
)