(ns monkey.lexer-test
  (:require
   [clojure.test :as t]
   [monkey.lexer :as l]
   [monkey.token :refer [TOKENS]]))

(defn- string-to-lexers [input]
  (let [lexer (l/init input)]
    (loop [l lexer
           acc-lexers []]
      (if (not= (get-in l [:token :type]) :eof)
        (let [next-lexer (l/next-token l)] (recur next-lexer (conj acc-lexers next-lexer)))
        acc-lexers))))

(defn- lexers-to-tokens [lexers]
  (map (fn [l] (let [token (get l :token)] [(:type token) (:literal token)])) lexers))

(comment
  (lexers-to-tokens (string-to-lexers "=+(){},;")))

(t/deftest test-next-token
  (let [tokens (lexers-to-tokens (string-to-lexers "=+(){},;"))
        expected-tokens [[:assign (:assign TOKENS)]
                         [:plus (:plus TOKENS)]
                         [:lparen (:lparen TOKENS)]
                         [:rparen (:rparen TOKENS)]
                         [:lbrace (:lbrace TOKENS)]
                         [:rbrace (:rbrace TOKENS)]
                         [:comma (:comma TOKENS)]
                         [:semicolon (:semicolon TOKENS)]
                         [:eof (:eof TOKENS)]]]
    (t/is (= tokens expected-tokens))))

(t/deftest test-next-token-whole-function
  (let [expected-tokens [[:let "let"]
                         [:ident "five"]
                         [:assign "="]
                         [:int "5"]
                         [:semicolon ";"]
                         [:let "let"]
                         [:ident "ten"]
                         [:assign "="]
                         [:int "10"]
                         [:semicolon ";"]
                         [:let "let"]
                         [:ident "add"]
                         [:assign "="]
                         [:function "fn"]
                         [:lparen "("]
                         [:ident "x"]
                         [:comma ","]
                         [:ident "y"]
                         [:rparen ")"]
                         [:lbrace "{"]
                         [:ident "x"]
                         [:plus "+"]
                         [:ident "y"]
                         [:semicolon ";"]
                         [:rbrace "}"]
                         [:semicolon ";"]
                         [:let "let"]
                         [:ident "result"]
                         [:assign "="]
                         [:ident "add"]
                         [:lparen "("]
                         [:ident "five"]
                         [:comma ","]
                         [:ident "ten"]
                         [:rparen ")"]
                         [:semicolon ";"]
                         [:eof "EOF"]]
        tokens (lexers-to-tokens (string-to-lexers (slurp "test/monkey/examples/add.monkey")))]
    (t/is (= tokens expected-tokens))))
