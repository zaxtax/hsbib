module BibParse where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

type Key = String
type Value = String
type Field  = (Key,Value)
-- data Field = Field Key Value
type Entry = (String,[Field])
-- data Entry = Entry Key [Field]

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "; print err}
            Right x  -> print x

word    :: Parser String
word    = many1 alphaNum <?> "word"

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
  val <- fval ",}"
  white
  return (key,val)

fields :: Parser (String,[(String,String)])
fields = do 
  key <- many1 (alphaNum <|> oneOf "/_:-")
  white; char ','; white
  rest <- sepEndBy field (char ',')
  return $ (key,rest)          

entry :: Parser Entry
entry = do 
  char '@'; title <- word; white
  (key,cont) <- braces lexer fields
  return (key,("doctype",title):cont)

bibfile = do {white; sepBy1 entry white}