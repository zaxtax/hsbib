module BibParse where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

import Data.Char

type Key = String
type Field  = (String,String)
-- data Field = Field Key Value
-- type Entry = (String,[Field])
data Entry = Entry Key [Field]

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
  key <- many1 (alphaNum <|> oneOf "/_:-?")
  skipMany space
  char '='; skipMany space
  val <- fval ",}"
  return (map toLower key,val)

fields :: Parser (String,[Field])
fields = do 
  key <- many1 (alphaNum <|> oneOf "/_:.?-")
  white; char ','; white
  rest <- sepEndBy field (do white; string ","; white)
  return (key,rest)

entry :: Parser Entry
entry = do 
  char '@'; title <- word; white
  (key,cont) <- braces lexer fields
  return $ Entry key (("doctype",title):cont)

bibfile = do {white; sepBy1 entry white}

parseIt :: String -> IO [Entry]
parseIt file = do
  parsed <-  parseFromFile bibfile file
  case parsed of
    Left err -> print err >> return []
    Right table -> return table