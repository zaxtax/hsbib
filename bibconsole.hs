{-# OPTIONS_GHC -XPatternGuards #-} 
module Main where

import Data.List (isInfixOf, isPrefixOf)
import Control.Monad
-- import Control.Monad.State -- we should use this at point
import System.Console.Readline hiding (Entry)
import System.Environment

import Control.Exception
import System.IO
import Text.Printf (printf)
import System.Posix.Internals (setNonBlockingFD)

import System.Process
-- import System.Cmd

import BibParse
import BibPrint
import BibUtils
import Data.String.Utils
import Data.Char

main = do 
  args <- getArgs
  bibs  <- case args of
    [] -> (putStr $ usageInfo commands) >> return []
    _  -> liftM concat (mapM parseIt args)
  initialize
  setAttemptedCompletionFunction (Just $ setupCompleter commands)
  catchIO $ repl (Just []) bibs
  resetTerminal Nothing

commands :: [CommandDescr]
commands = [("help", "   -- show help", complete_none)
           ,("open", "   -- open document",complete_file)
           ,("find", "   -- find documents",complete_none)
           ,("version", "-- show version", complete_none)
           ,("quit", "   -- quit",  complete_none)]

docOpen arg e = do
  runProcess "gv" ["RW1.pdf"] (Just "/home/cf/orig/") Nothing Nothing Nothing Nothing

search :: [String] -> [Entry] -> [String] 
search s e = map (printEntry . displayEntry) $ filter (blend s . (map toLower) . show) e
             where blend []   _ = False
                   blend (q:qx) e | isInfixOf q e = True
                                  | otherwise = blend qx e

-- get the right dir

-- canonicalizePath  . takeDirectory
-- (/) getCurrentDirectory isAbsolute

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

handleInput :: String -> [Entry] -> IO (Maybe [String],[Entry])
handleInput = execute . (map removeQuotes) . splitLine . strip  

repl :: Maybe [String] -> [Entry] -> IO ()
repl Nothing _ = return ()
repl prev db = do
  input <- getInput
  case input of
    Nothing -> return ()
    Just i  -> do
        (res,newdb) <- handleInput i db
        repl res newdb 

execute :: [String] -> [Entry] -> IO (Maybe [String],[Entry])
execute ["help"] e = putStr (usageInfo commands) >> return (Just [],e)
execute ("open":xs) e = docOpen xs e >> return (Just [],e)
execute ("find":xs) e = mapM_ putStrLn res >> return (Just res,e) where res = search xs e
execute ("dump":xs) e = putStrLn (concatMap (show . findEntry e) xs) >> return (Just [],e)
execute ["quit"] e = return (Nothing,e)
execute ["version"] e = putStrLn "hsbib: 0.1" >> return (Just [],e)
execute debug e = putStr (concatMap id $ ":":debug++[":\n"]) >> return (Just [],e)
