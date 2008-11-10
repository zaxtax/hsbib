module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.ReadPrec
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

-- Just to show it works
main = (putStrLn . show . product) [1..10]

parens2  :: Parser ()
parens2  = do{ char '('
            ; parens2
            ; char ')'
            ; parens2
            }
        <|> return ()

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
fval s = braces lexer (fval "}") <|>
         quo (fval "\"") <|>
         many1 (try (do char '\\'; char '"') 
                     <|> (noneOf s))
     
field :: Parser (String,String)
field  = do { white;
              key <- many1 alphaNum; skipMany space;
              char '='; skipMany space;
              val <- fval ",}\"";
              white;
              return (key,val)
            }

fields :: Parser [(String,String)]
fields = do {key <- many1 (alphaNum <|> oneOf ":-"); 
             white; char ','; white; 
             rest <- sepBy1 field (char ',');
             return $ ("key",key):rest
            }

entry :: Parser (String,[(String,String)])
entry = do { char '@'; title <- word; white;
             cont <- braces lexer fields; return (title,cont)}

bibfile = sepBy1 entry white

test = parseFromFile bibfile "/home/cf/papers/papers.bib"