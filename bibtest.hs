module Main where 

import Text.ParserCombinators.Parsec

import BibParse
import BibPrint
import Data.List
import Maybe
import Char
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


type EntryKey = String
type FieldKey = String
type Table    = [Entry]
findEntry :: Table->EntryKey->Maybe Entry
findEntry t ek = find (\(ek',_) -> ek' == (map toLower ek) ) t 

lookupKeyValue_ :: Entry->Key->Maybe Value
lookupKeyValue_ (n,fs) k = case (find (\(k',_) -> k' == (map toLower k) ) fs) of
                             Just kv -> Just (snd kv)
                             Nothing -> Nothing

lookupKeyValue :: Table->EntryKey->Key-> Maybe Value
lookupKeyValue t ek k = case (find ( \(ek',fs) -> ek' == (map toLower ek) ) t ) of 
                          Just e -> lookupKeyValue_ e k
                          Nothing -> Nothing
getWithTag :: Table->String->Table
getWithTag t s = filter (\e -> hasTag e s) t 
hasTag :: Entry->String->Bool
hasTag e s = case lookupKeyValue_ e "tag" of
               Nothing -> False
               Just s' -> isInfixOf (map toLower s ) (map toLower s')
