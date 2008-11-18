module Text.Parser.Bibtex where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

-- Just to show it works
-- main = (putStrLn . show . product) [1..10]

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "; print err}
            Right x  -> print x

word    :: Parser String
word    = many1 alphaNum <?> "word"

word'    :: Parser String
word'    = do {c <- letter;
               do cs <- word'
                  return (c:cs) 
               <|> return [c]
              }

lexer   = makeTokenParser haskellDef

quo p = do {char '"'; res <- p; char '"'; return res} 

white :: Parser ()
white = skipMany (space <|> newline);

fval :: String -> Parser String
fval s = do
  c <- noneOf s
  case c of
    '{'  -> do co <- fval "}"
               char '}'
               cs <- fval s
               return $ co++cs
    '"'  -> do co <- fval "\""
               char '"'
               cs <- fval s
               return $ co++cs
    '\\' -> do { try $ char '"'; cs <- fval s; return ('"':cs)}
           <|> do {cs <- fval s; return (c:cs)}
    _    -> do {cs <- fval s; return (c:cs)}
  <|> return ""
     
field :: Parser (String,String)
field  = do 
  white
  key <- many1 alphaNum; skipMany space;
  char '='; skipMany space
  val <- fval ",\n}"
  white
  return (key,val)

fields :: Parser [(String,String)]
fields = do 
  key <- many1 (alphaNum <|> oneOf "/_:-")
  white; char ','; white
  rest <- sepBy1 field (char ',')
  return $ ("key",key):rest          

entry :: Parser (String,[(String,String)])
entry = do 
  char '@'; title <- word; white
  cont <- braces lexer fields
  return (title,cont)

bibfile = do {white; sepBy1 entry white}

test = parseFromFile bibfile "/home/cf/papers/papers.bib"