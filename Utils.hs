{-# OPTIONS_GHC -XPatternGuards #-} 
module Utils where

import Data.List
import Data.Char
import Control.Monad
import Data.String.Utils

import Parse

fromJust Nothing  = undefined
fromJust (Just a) = a

catMaybes ls = [x | Just x <- ls]

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

displayEntry :: Entry -> [String]
displayEntry (Entry k fields) = 
    k : (map (strip . fromJust . flip lookup fields) ["title","author"])

printEntry :: [String] -> String
printEntry (k:title:_) = concat ["(",k,")\n  Title: ",title]

removeQuotes :: String -> String
removeQuotes = filter (/= '\"')
--Given "a "b c" d" returns ["a","b c", d]
----Use that take thingy
splitLine [] = []
splitLine (c:cs) | isSpace c = splitLine cs
                 | c == '"' = (beforeq : splitLine afterq) 
                 | True     = (beforew: splitLine afterw)
   where (beforeq, _, afterq) = splitWhen (\c' -> c' == '"') cs 
         (beforew, _, afterw) = splitWhen isSpace (c:cs)
{-
splitLine x  = y : splitLine ys 
    where [(y,ys)] = lex x
-}

splitWhen :: (a->Bool)->[a] -> ([a],a,[a])
splitWhen f l   = (before, at , after)
   where before = takeWhile (not.f) l
         sortOfAfter = dropWhile (not.f) l
         after  = if null sortOfAfter then sortOfAfter else tail sortOfAfter
         at     = head $ dropWhile (not.f) l  --Is ghc smart enough?

findEntry :: [Entry] -> Key -> Maybe Entry
findEntry t k = find (\ (Entry key _) -> (map toLower key) == map toLower k) t

lookupKeyValue_ :: String -> Entry -> Maybe String
lookupKeyValue_ k (Entry _ fs)
    | Just kv <- find (\(k',_) -> k' == (map toLower k) ) fs
    = Just (snd kv)
    | otherwise = Nothing

lookupKeyValue :: [Entry] -> String-> Key -> Maybe String
lookupKeyValue t k ek 
    | Just e <- find ( \(Entry ek' fs) -> (map toLower ek') == (map toLower ek)) t 
    = lookupKeyValue_ k e
    | otherwise = Nothing

getWithTag :: [Entry]->String->[Entry]
getWithTag t s = filter (\e -> isTag e s) t 

isTag :: Entry->String->Bool
isTag e s | Just s' <- lookupKeyValue_ "tag" e
          = isInfixOf (map toLower s ) (map toLower s')
          | otherwise = False

-- |Takes a field and a new value and an entry and returns 
-- an entry where that field has the given value
updateField :: String -> String -> Entry -> Entry
updateField _ _ e@(Entry k []) = e
updateField old new e@(Entry key fs) 
    | length back >= 1 =Entry key (front++(old,new):tail back)
    | otherwise = e
    where (front,back) = break (\(k,v)->old==k) fs

mapFields :: (Field -> Field) -> Entry -> Entry
mapFields f (Entry key xs) = Entry key (map f xs) 

addField :: Field -> Entry -> Entry
addField n e@(Entry k fs) | not $ hasField e (fst n) = Entry k (n:fs)
                          | otherwise = e

dropField :: Field -> Entry -> Entry
dropField n (Entry key fs) = 
    Entry key (deleteBy (\(a,_) (b,_) -> a==b) n fs)

hasField :: Entry -> String -> Bool
hasField (Entry _ fs) key = any (\ x -> fst x == key) fs 

dumpToFile :: Entry -> IO ()
dumpToFile = undefined

dropDups :: Entry -> Entry
dropDups (Entry key fs) = Entry key (nubBy (\(a,_) (b,_) -> a==b) fs)

-- Help Functions

type Help =  (String,(String,String))

noMore = "No additional help provided."

helps :: [Help]
allShortHelp :: String

helps = [
	("bibconsolerc",("Startup configuration file for bibconsole",noMore)),
	("dump",("write \"pathToFile\" entries to the named file",noMore)),
	("find",("find documents",noMore)),
	("help",("help \"cmd\" for more information on a command.",noMore)),
	("load",("load \"pathToFile\" loads bibtex file entries from name file",noMore)),
	("open",	("open document","Opens document with associated viewer based on file extension. \n\tFile associations are specifiable in the bibconsolerc file.\n\tUse \"help bibconsolerc\" for more information.")),
	("version",("show version",noMore)),
	("quit",("Exits the program without checking to see if data is saved.",noMore))
	]

allShortHelp = concatMap formatShortHelp helps ++ "\n"

formatShortHelp :: Help->String
formatShortHelp (key, (short, long)) = concat [ key, "\t\t--", short, "\n" ]

formatLongHelp :: Help->String
formatLongHelp (key, (short, long)) = concat [ key, " - ", long, "\n" ]

getShortHelp :: String -> String
getShortHelp = (formatShortHelp . getHelp helps )
getLongHelp :: String -> String
getLongHelp  = (formatLongHelp  . getHelp helps )

getHelp ::[Help]->String->Help
getHelp ( h@(cmdName', _):hs ) cmdName = if cmdName == cmdName' then h else getHelp hs cmdName
getHelp _ _ = ("", ("help not found try just \"help\" instead","help not found try just \"help\" instead"))
