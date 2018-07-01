(ns lexer.parser
  (:require [lexer.core :refer [in?]
             clojure.string :as str]))

(defn parens 
  [string]
  (str "(" string ")")
)

(defn parse
  [token]
  (let [{:keys [type]} token]
    (condp in? type
      [:if :for :while :function-define]
      (str 
        (->> (case type
               :if ["if " (->> token :condition parse parens)] 
               :for ["for (const " (->> token :variable) " of " (->> token :iterator parse) ")"]
               :while ["while " (->> token :condition parse parens)]
               :function-define ["function " (->> token :name) (->> token :params (str/join ", ") parens)]
             )
          str/join
        )
        "{\n"
        (->> token 
          :body
          (mapv parse) 
          (str/join ";\n")
        )
        "}\n"
      )

      [:function-call :variable-define]
      (str
        (->> (case type
               :function-call [(->> token :name) (->> token :arguments (str/join ", ") parens)]
               :variable-define [(->> token :name) " = " (->> token :value parse)]
             )
          str/join
        )
        ";\n"
      )

      [:expression]
      (->> token :arguments (str/join ", ") parens)

      [:value]
      (->> token :body)

      [:operator]
      (->> token :body)
    )
  )
)
