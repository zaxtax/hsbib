{-# OPTIONS_GHC -XPatternGuards #-} 
module Main where

import Data.List (isInfixOf, isPrefixOf)
import Control.Monad
import System.Console.Readline hiding (Entry)
import System.Environment

import Control.Exception
import System.IO
import Text.Printf (printf)
import System.Posix.Internals (setNonBlockingFD)

import System.Process
-- import System.Cmd

import Text.ParserCombinators.Parsec (parseFromFile)
import BibParse
import Data.String.Utils

main = do 
  args <- getArgs
  foo  <- case args of
    [] -> putStrLn $ usageInfo commands
    _  -> mapM_ parse args
  initialize
  setAttemptedCompletionFunction (Just $ setupCompleter commands)
  catchIO repl
  resetTerminal Nothing

commands :: [CommandDescr]
commands = [("help", "   -- show help", complete_none)
           ,("open", "   -- open document",complete_file)
           ,("find", "   -- find documents",complete_none)
           ,("version", "-- show version", complete_none)
           ,("quit", "   -- quit",  complete_none)]

docOpen = id

search s = map (filter (/= '\"')) (splitLine s)

parse file = do
  parsed <-  parseFromFile bibfile file
  case parsed of
    Left err -> print err
    Right table -> print table

fromJust (Just a) = a

displayEntry :: Entry -> [String]
displayEntry entry = map (fromJust . flip lookup (snd entry)) ["title","author"]

splitLine [] = []
splitLine x  = y : splitLine ys 
    where [(y,ys)] = lex x

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

handleInput :: String -> IO Bool
handleInput = execute . splitLine . strip   

repl :: IO ()
repl = do
  input <- getInput
  case input of
    Nothing -> return ()
    Just i  -> handleInput i >>= flip unless repl

execute :: [String] -> IO Bool
execute ["help"] = putStr (usageInfo commands) >> return False
execute ["quit"] = return True
execute ["version"] = putStrLn "hsbib: 0.1" >> return False
execute debug = putStr (concatMap id $ ":":debug++[":\n"]) >> return False
