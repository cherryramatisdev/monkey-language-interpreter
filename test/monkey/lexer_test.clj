(ns monkey.lexer-test
  (:require [clojure.test :as t]
            [monkey.lexer :as l]
            [monkey.token :refer [TOKENS]]))

(t/deftest test-next-token
  (let [input "=+(){},;"
        lexer (l/init input)
        tests [[:assign (:assign TOKENS)]
               [:plus (:plus TOKENS)]
               [:lparen (:lparen TOKENS)]
               [:rparen (:rparen TOKENS)]
               [:lbrace (:lbrace TOKENS)]
               [:rbrace (:rbrace TOKENS)]
               [:comma (:comma TOKENS)]
               [:semicolon (:semicolon TOKENS)]
               [:eof (:eof TOKENS)]]]
    (reduce (fn [current-lexer [expected-type expected-literal]]
              (let [token (l/next-token current-lexer)
                    {:keys [type literal]} (:token token)]
                (t/is (= type expected-type))
                (t/is (= literal expected-literal))
                token)) lexer tests)))

(comment
  (str \c)
  (let [input "=+(){},;"
        lexer (l/init input)
        tests [[:assign (:assign TOKENS)]
               [:plus (:plus TOKENS)]
               [:lparen (:lparen TOKENS)]
               [:rparen (:rparen TOKENS)]
               [:lbrace (:lbrace TOKENS)]
               [:rbrace (:rbrace TOKENS)]
               [:comma (:comma TOKENS)]
               [:semicolon (:semicolon TOKENS)]
               [:eof (:eof TOKENS)]]]
    (reduce (fn [current-lexer [expected-type [expected-literal]]]
              (let [token (l/next-token current-lexer)
                    {:keys [type literal]} (:token token)]
                (println type expected-type literal expected-literal)
                token)) lexer tests)))
