(ns lexer.core)
(require '[clojure.string :as str])

(def keywords ["if" "elif" "else"])
(def operators ["+" "-"])

(defrecord Lexeme [type data line])

(defn in? 
  "Checks if a collection contains an element."
  [e collection]  
  (some #(= e %) collection))

(defn make-lexeme
	[type data line]
	(Lexeme. type data line))

(defn paren-lexeme
	"Takes :open or :closed and a line number and returns a ( lexeme or a ) lexeme."
	[type line-number]
	(make-lexeme :paren type line-number))
	
(defn lexeme-from-source
	"Takes one word from a line and creates a lexeme from it.
	 It assumets () have been filtered already."
	[word line-number]
	(cond 
		(in? word keywords) (make-lexeme :keyword word line-number)
		(in? word operators) (make-lexeme :operator word line-number)
		(= ")" word) (make-lexeme :paren :closed line-number)
		(= "(" word) (make-lexeme :paren :open line-number)
		(= 0 (count word)) nil
		:else (make-lexeme :identifier word line-number)))

(defn find-first 
	"Given a string and some chars, finds the index of first char which occurs. 
	If none are found, nil is returned."
	[string & rest]
	(let [length (count string)] 
		(loop [i 0]
			(if (>= i length)
				nil ; not found
				(if (in? (nth string i) rest)
					i
					(recur (inc i)))))))
		
(defn lex-word
	"Lexes a word like foo() into a list of lexemes."
	[word line-number]
	(let [index (find-first word "(" ")")]
		(if index
			(if (= index 0)
				(cons
					(lexeme-from-source (subs word 0 1) line-number)
					(lex-word (subs word 1) line-number))
				(cons ; lex up to first paren
					(lexeme-from-source (subs word 0 index) line-number)
					(lex-word (subs word index) line-number)))
			(lexeme-from-source word line-number))))  