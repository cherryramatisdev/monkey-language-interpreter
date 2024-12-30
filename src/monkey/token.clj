(ns monkey.token)

(def TOKENS {:illegal "ILLEGAL"
             :eof "EOF"
             :ident "IDENT"
             :int "INT"
             :assign "="
             :plus "+"
             :comma ","
             :semicolon ";"
             :lparen "("
             :rparen ")"
             :lbrace "{"
             :rbrace "}"
             :function "FUNCTION"
             :let "LET"})

(def KEYWORDS {"fn" :function
               "let" :let})

(defn lookup-ident [ident]
  (if (contains? KEYWORDS ident)
    (get KEYWORDS ident)
    :ident))

(comment
  (lookup-ident "aa"))
