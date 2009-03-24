module Main where 

import Parse
import Print
import Utils
import Cfg

import System.Environment
import Data.List
import Maybe
import Char

main = do
  [args] <- getArgs
  test args
  test2 args
  -- testPretty args
  testCFG

test = parseIt

test2 file = do
  table <- test file
  print (doLookups table)  -- do something with the correctly parsed lookup table

doLookups table = lookupKeyValue table "src" "lecun-06" 

testPretty file = do 
  table <- test file
  putStr (foldr (++) "\n" (map show table))
						
testCFG = do
   l <- loadDefaultCFGFile
   g <- return $ getDocumentAssociations l
   print g
   print $ lookupDocumentViewer g "foo.pdf"
   print $ lookupDocumentViewer g "foo.noextensionmatches"
   putStrLn "testCFGDone"

test3 file = parseIt file >>= print

test4 file = do
  table <- test file
  return $ length table

