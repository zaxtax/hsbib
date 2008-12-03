{-# OPTIONS_GHC -XPatternGuards #-} 
module Main where

import Data.List (isInfixOf, isPrefixOf, transpose)
import Control.Monad
-- import Control.Monad.State -- we should use this at point
import System.Console.Readline hiding (Entry)
import System.Environment

import Control.Exception
import System.IO
import Text.Printf (printf)
import System.Posix.Internals (setNonBlockingFD)
import System.Process

import System.Directory
import System.FilePath

import BibParse
import BibPrint
import BibUtils
import Data.String.Utils
import Data.Char

main = do 
  args <- getArgs
  bibs  <- case args of
    [] -> (putStr $ usageInfo commands) >> return []
    _  -> liftM concat (mapM makeAbsParse args)
  initialize
  setAttemptedCompletionFunction (Just $ setupCompleter (makeOpenDescr bibs:commands))
  catchIO $ repl (Just []) bibs
  resetTerminal Nothing

commands :: [CommandDescr]
commands = [("help", "   -- show help", complete_none)
           ,("open", "   -- open document", complete_none)
           ,("load", "   -- load bibtex file",complete_file)
           ,("find", "   -- find documents",complete_none)
           ,("version", "-- show version", complete_none)
           ,("quit", "   -- quit",  complete_none)]

docOpen docs e = do
  mapM (\ x-> runCommand ("gv " ++ x)) 
       (catMaybes $ map (lookupKeyValue e "src") docs)
    
                 

search :: [String] -> [Entry] -> [[String]]
search s e = map displayEntry $ filter (blend s . (map toLower) . show) e
             where blend []   _ = False
                   blend (q:qx) e | isInfixOf q e = True
                                  | otherwise = blend qx e

-- helper functions to aid in opening
makeOpenDescr :: [Entry] -> CommandDescr
makeOpenDescr entries = ("open","   -- open document", completeIds entries)

completeIds :: [Entry] -> Completer
completeIds entries = do
  ids <- return $ map (\(Entry k _) -> k) entries
  return . complete_string ids

-- get the right dir
makeAbsParse :: FilePath -> IO [Entry]
makeAbsParse file = do
  entries <- parseIt file
  abDir <- (canonicalizePath . takeDirectory) file
  mapM (return . makeAbsField abDir) entries

makeAbsField :: FilePath -> Entry -> Entry
makeAbsField abDir e = 
  case (lookupKeyValue_ "src" e) of
    Just f -> if isRelative f 
              then updateField "src" (abDir</>f) e 
              else e
    Nothing -> e

-- getCurrentDirectory

-- Most of these functions cribbed from readline reference
catchIO :: IO () -> IO ()
catchIO = handle (hPrint stderr)

type Completer = String -> IO [String]
type CommandDescr = (String, String, String -> IO [String])

usageInfo :: [CommandDescr] -> String
usageInfo cmds = unlines $ [printf "%s %s" n d | (n, d, _) <- cmds]

completer comps defcomp w _ _ = do
    -- Get the entire input line so far
    line <- getLineBuffer
    
    -- Get the completer to use. First see whether the word is a command
    -- name, and if so use the completer associated with it. Otherwise, fall
    -- back to using the default command name completer
    let f | (c:_)   <- words line
          , Just f' <- lookup c comps
          = f'
          | otherwise = defcomp

    -- Run the completer
    completionMatches w f

complete_none :: Completer
complete_none = return . const []

complete_string :: [String] -> String -> [String]
complete_string cmp s = filter (isInfixOf s) cmp

complete_file :: Completer
complete_file = filenameCompletionFunction

setupCompleter :: [CommandDescr] -> String -> Int -> Int
               -> IO (Maybe (String, [String]))
setupCompleter cmds = completer comps (return . complete_string names)
    where
        comps = [(n, f) | (n, _, f) <- cmds]
        names = [n | (n, _, _) <- cmds]

getInput :: IO (Maybe String)
getInput = do
    line <- readline "# " `finally` setNonBlockingFD 0
    case line of
        Nothing -> putStr "\n" >> return Nothing
        Just xs -> addHistory xs >> return (Just xs)

handleInput :: String -> Maybe [String] -> [Entry] -> IO (Maybe [String],[Entry])
handleInput = execute . (map removeQuotes) . splitLine . strip  

repl :: Maybe [String] -> [Entry] -> IO ()
repl Nothing _ = return ()
repl prev db = do
  input <- getInput
  case input of
    Nothing -> return ()
    Just i  -> do
        (res,newdb) <- handleInput i prev db
        repl res newdb 

execute :: [String] -> Maybe [String] -> [Entry] -> IO (Maybe [String],[Entry])
execute ["help"] _ e = putStr (usageInfo commands) >> return (Just [],e)
execute ["open"] r e = docOpen (fromJust r) e >> return (Just [],e) -- should just pass all the last values found
execute ("open":xs) _ e = docOpen xs e >> return (Just [],e)
execute ("find":xs) _ e = mapM_ (putStrLn . printEntry) res >> return (Just (head $ transpose res),e) where res = search xs e
execute ("print":xs) _ e = putStrLn (concatMap (show . findEntry e) xs) >> return (Just [],e) -- to debug
execute ["quit"] _ e = return (Nothing,e)
execute ["version"] _ e = putStrLn "hsbib: 0.1" >> return (Just [],e)
execute ["list"] _ e = putStrLn (show e) >> return (Just [],e) -- to debug
execute debug _ e = putStr (concatMap id $ ":":debug++[":\n"]) >> return (Just [],e)
