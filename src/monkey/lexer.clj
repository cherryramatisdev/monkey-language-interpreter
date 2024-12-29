(ns monkey.lexer
  (:require
   [monkey.token :refer [TOKENS]]))

(defn- read-char [lexer]
  (let [lexer-input (:input lexer)
        lexer-read-pos (:readPosition lexer)
        lexer (if (>= lexer-read-pos (count lexer-input))
                (assoc lexer :ch 0)
                (assoc lexer :ch (nth lexer-input lexer-read-pos)))]
    (assoc lexer :position lexer-read-pos :readPosition (+ lexer-read-pos 1))))

(defn init
  "This function should receive an 'input from type string and return a map containing all the info for the lexer"
  [input]
  (read-char {:input input :position 0 :readPosition 0 :ch nil :token {:type nil :literal nil}}))

(defn- new-token [token-type ch]
  {:type token-type :literal (str ch)})

(defn next-token
  "This function receives the lexer from 'init and return the next token in a parsed form"
  [lexer]
  (read-char (case (:ch lexer)
               \= (assoc lexer :token (new-token :assign (:assign TOKENS)))
               \; (assoc lexer :token (new-token :semicolon (:semicolon TOKENS)))
               \( (assoc lexer :token (new-token :lparen (:lparen TOKENS)))
               \) (assoc lexer :token (new-token :rparen (:rparen TOKENS)))
               \, (assoc lexer :token (new-token :comma (:comma TOKENS)))
               \+ (assoc lexer :token (new-token :plus (:plus TOKENS)))
               \{ (assoc lexer :token (new-token :lbrace (:lbrace TOKENS)))
               \} (assoc lexer :token (new-token :rbrace (:rbrace TOKENS)))
               0 (assoc lexer :token (new-token :eof (:eof TOKENS))))))

(comment
  (->> "=+(){},;"
       init
       next-token)
  (new-token :rbrace (:rbrace TOKENS))
  (nth "teste" 1))
