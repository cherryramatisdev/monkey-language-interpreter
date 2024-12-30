(ns monkey.lexer
  (:require
   [monkey.lexer :as l]
   [monkey.token :refer [TOKENS lookup-ident]]))

(defn- read-char [lexer]
  (let [lexer-input (:input lexer)
        lexer-read-pos (:readPosition lexer)
        lexer (if (>= lexer-read-pos (count lexer-input))
                (assoc lexer :ch nil)
                (assoc lexer :ch (nth lexer-input lexer-read-pos)))]
    (assoc lexer :position lexer-read-pos :readPosition (+ lexer-read-pos 1))))

(defn init
  "This function should receive an 'input from type string and return a map containing all the info for the lexer"
  [input]
  (read-char {:input input :position 0 :readPosition 0 :ch nil :token {:type nil :literal nil}}))

(defn- new-token [token-type ch]
  {:type token-type :literal (str ch)})

(defn- is-letter [ch]
  (or (and (<= (int \a) (int ch)) (<= (int ch) (int \z))) ;; Lowercase 'a' to 'z'
      (and (<= (int \A) (int ch)) (<= (int ch) (int \Z))) ;; Uppercase 'A' to 'Z'
      (= ch \_)))

(defn- is-digit [ch]
  (and (>= (int ch) (int \0)) (<= (int ch) (int \9))))

(defn- read-token-literal
  "For a validator function that returns boolean like 'is-letter, we forward the input and register the slice between."
  [validator-fn lexer]
  (let [pos (:position lexer)]
    (loop [l lexer]
      (if (validator-fn (:ch l))
        (recur (read-char l))
        (assoc-in l [:token :literal] (subs (:input l) pos (:position l)))))))

(defn- skip-whitespace [lexer]
  (loop [l lexer]
    (if (or (= (:ch l) \space) (= (:ch l) \tab) (= (:ch l) \newline) (= (:ch l) \return))
      (recur (read-char l))
      l)))

(comment
  (-> "5;"
      init
      next-token))

(defn next-token
  "This function receives the lexer from 'init and return the next token in a parsed form"
  [lexer]
  (let [l (skip-whitespace lexer)]
    (case (:ch l)
      \= (read-char (assoc l :token (new-token :assign (:assign TOKENS))))
      \; (read-char (assoc l :token (new-token :semicolon (:semicolon TOKENS))))
      \( (read-char (assoc l :token (new-token :lparen (:lparen TOKENS))))
      \) (read-char (assoc l :token (new-token :rparen (:rparen TOKENS))))
      \, (read-char (assoc l :token (new-token :comma (:comma TOKENS))))
      \+ (read-char (assoc l :token (new-token :plus (:plus TOKENS))))
      \{ (read-char (assoc l :token (new-token :lbrace (:lbrace TOKENS))))
      \} (read-char (assoc l :token (new-token :rbrace (:rbrace TOKENS))))
      nil (read-char (assoc l :token (new-token :eof (:eof TOKENS))))
      (cond
        (is-letter (:ch l)) (let [lexer (read-token-literal is-letter l)]
                              (assoc-in lexer [:token :type] (lookup-ident (:literal (:token lexer)))))
        (is-digit (:ch l)) (let [lexer (read-token-literal is-digit l)]
                             (assoc-in lexer [:token :type] :int))
        :else (assoc l :token (new-token :illegal (:ch lexer)))))))

(comment
  (->> "     let x = 0;"
       init
       next-token
       next-token)
  (new-token :rbrace (:rbrace TOKENS))
  (Character/isLetter "a")
  (nth "teste" 1))
