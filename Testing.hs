module Main where 

import Parse
import Print
import Utils
import CFG

import System.Environment
import Data.List
import Maybe
import Char

-- import Input

main = do
  [args] <- getArgs
  test args
  test2 args
  testPretty args

test = parseIt

test2 file = do
  table <- test file
  print (doLookups table)  -- do something with the correctly parsed lookup table

doLookups table = lookupKeyValue table "lecun-06" "src" 

testPretty file = do 
  table <- test file
  putStr (foldr (++) "\n" (map show table))
						
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
  table <-  parseIt file
  print table

test4 file = do
  table <- test file
  return $ length table

