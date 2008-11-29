module BibTest where 

import Text.ParserCombinators.Parsec

import BibParse
import BibPrint

main = do
  test
  test2
  testPretty

test = parseFromFile bibfile "/home/cf/papers/papers.bib"

test2 = do
  parsed <- test
  case parsed of 
    Left err -> print err                   -- there was a parse error
    Right table -> print (doLookups table)  -- do something with the correctly parsed lookup table

doLookups table = lookup "lecun-06" table >>= lookup "src" 

testPretty = do 
  out <- test
  case out of
    Left err -> print err
    Right table -> putStr (foldr (++) "" (map entryToStr table))



