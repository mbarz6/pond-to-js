(ns lexer.parser
  (:require [lexer.core :refer [in?]
             clojure.string :as str]))

(defn parse
  [token]
  (let [{:keys [type]} token]
    (condp in? type
      [:if :for :while :function-define]
      (str 
        (->> (case type
               :if ["if (" (->> token :condition parse) ")"] 
               :for ["for (const " (->> token :variable) " of " (->> token :iterator parse) ")"]
               :while ["while (" (->> token :condition parse) ")"]
               :function-define ["function " (->> token :name) "(" (->> token :params (str/join ", ")) ")"]
             )
          (str/join)
        )
        "{\n"
        (->> token 
          :body
          (mapv parse) 
          (str/join ";\n")
        )
        "}\n"
      )
    )
  )
)
