(ns lexer.core
  (:require [clojure.string :as str]))

(def keywords ["if" "elif" "else"])
(def operators ["+" "-" "==" "=" "<" "<="])

;; used for division manager
;; trigger-division is list of chars which trigger division
(def all-symbols (concat operators ["(" ")" ","]))
(def trigger-division 
  ; generated from parens + first chars of operators
  (distinct (map #(nth % 0) all-symbols)))

	
(defrecord Lexeme [type data line])

(defn in?
  "Checks if a collection contains an element."
  [e collection]  
  (some #{e} collection))

(defn make-lexeme
  [type data line]
  (Lexeme. type data line))
	
(defn lexeme-from-source
  "Takes one word from a line and creates a lexeme from it.
   It assumes () have been filtered already."
  [word line-number]
  (cond 
    (in? word keywords) (make-lexeme :keyword word line-number)
    (in? word operators) (make-lexeme :operator word line-number)
    (= ")" word) (make-lexeme :paren :closed line-number)
	(= "(" word) (make-lexeme :paren :open line-number)
    (= "," word) (make-lexeme :comma nil line-number)
    (empty? word) nil
    :else (make-lexeme :identifier word line-number)))

(defn find-first 
  "Given a string and some chars, finds the index of first char which occurs. 
   If none are found, nil is returned."
  [string chars]
  (let [length (count string)] 
    (loop [i 0]
      (if (>= i length)
        nil ; not found
        (if (in? (nth string i) chars)
          i
          (recur (inc i)))))))			
					
(defn find-longest-at-start
  "Given a string and a list of strings, it returns the largest string in
   the list which is also a substring, starting at index 0, of the large string.
   It assumes at least one of the substrings works."
  [word substrings]
  (defn inner-recursive 
     "Does the work of find-longest-at-start, but this lets us avoid sorting every time."
     [word substrings]
	 (let [longest (nth substrings 0)
       longest-length (count longest)]
       (if (and (>= (count word) longest-length) (= longest (subs word 0 (count longest))))
         longest
         (inner-recursive word (rest substrings)))))
  (inner-recursive word (sort #(> (count %1) (count %2)) substrings)))
	
(defn divide-word
  "Divides a word like foo() into a list of words, each of which can be made a lexeme."
  [word]
  ; find index of first char which triggers a division
  (let [index (find-first word trigger-division)]
    (if index
      (if (= 0 index)
        ; if the first char is a symbol, than (subs word 0 0) would be "", uh oh! 
        (let [operator-length (count (find-longest-at-start word all-symbols))]
          (cons 
            (subs word 0 operator-length)
            (divide-word (subs word operator-length)))) 
              (cons 
                (subs word 0 index) ; part before symbol
                ; find the length of the largest matching operator
                  (let [operator-length (count (find-longest-at-start (subs word index) all-symbols))]
                  (cons
                    (subs word index (+ index operator-length)) ; the symbol
                    (divide-word (subs word (+ index operator-length))))))) ; rest of word
      (if (empty? word)
        nil
        ; this way, we get a list containing word instead of just a string
       (cons word nil)))))

(defn lex-word
  "Lexes a word, aka string with no whitespace, and returns list of lexemes."
  [word line-number]
  (defn lex-word-recursive
    "Recursively lexes a divided word."
    [word-divisions]
    (if (empty? word-divisions)
      ()
      ; otherwise, lex first division, than lex the rest of the word
      (cons 
        (lexeme-from-source (first word-divisions) line-number) 
		(lex-word-recursive (rest word-divisions)))))
  (lex-word-recursive (divide-word word)))

(defn lex-line-parens
  "Lexes a line of source code, returning a list of lexemes including parens."
  [line line-number]
  ; split string into words
  ; lex the words
  ; combine all the lists of lexemes into very big list of lexemes
  (reduce concat 
    (map #(lex-word % line-number) 
      (str/split line #" "))))

(defn open-paren
  "Finds index of first open paren in list of lexemes, returning nil if no ( is found."
  [lexemes]
  (if (empty? lexemes)
    nil
    (let [front (first lexemes)] 
      (if (and (= :paren (:type front)) (= :open (:data front)))
        0 ; if it's a paren, we're done
        (+ 1 (open-paren (rest lexemes)))))))
