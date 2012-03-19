(use '[clarsec])

(def input
       "Year,Make,Model,Length
       1997,Ford,Model-350,234
       2000,Mercury,\"Model 800\",238")

(def cell (lexeme (<|> string-literal
             (stringify (many (none-of ",\n"))))))

(def line (sep-by-1 cell comma))

(def csv (sep-by line eol))

(parse$ csv input)

