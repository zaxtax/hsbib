module Main where 

import Text.ParserCombinators.Parsec

import Parse
import Print
import CFG
import Data.List
import Maybe
import Char
import Input
main = do
  test
  --test2
  testPretty

test = parseFromFile bibfile "/home/javirosa/papers/papers.bib"

{-test2 = do
  parsed <- test
  case parsed of 
    Left err -> print err                   -- there was a parse error
    Right table -> print (doLookups table)  -- do something with the correctly parsed lookup table
-}
doLookups table = lookup "lecun-06" table >>= lookup "src" 

testPretty = do 
  out <- test
  case out of
    Left err ->  print err
    Right table -> do putStr (foldr (++) "\n" (map show table))

testGetEntry = do
	out <- test
	case out of
		Left err -> print err
		Right table -> printIE $ getIEntry table

printIE entryM = do 
	x <- entryM
	print entryM
	return ()
	
	
								
{-
testCFG = do
   l <- loadDefaultCFGFile
	let g = getDocumentAssociations l
	print g
	print lookupDocumentViewer g "foo.pdf"
	print lookupDocumentViewer g "foo.noextensionmatches"
   putStr "testCFGDone"
	1
-}
test3 file = do
  parsed <-  parseFromFile bibfile file
  case parsed of
    Left err -> print err
    Right table -> print table

test4 file = do
  parsed <- test
  case parsed of
    Left err -> print err >> return 0
    Right table -> return $ length table

