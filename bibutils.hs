{-# OPTIONS_GHC -XPatternGuards #-} 
module BibUtils where

import Data.List
import Data.Char
import Control.Monad
import Data.String.Utils

import BibParse

fromJust Nothing  = "none"
fromJust (Just a) = a

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

displayEntry :: Entry -> [String]
displayEntry (Entry k fields) = 
    k : (map (strip . fromJust . flip lookup fields) ["title","author"])

printEntry (k:title:_) = concat ["(",k,")\n  Title: ",title]

removeQuotes :: String -> String
removeQuotes = filter (/= '\"')

splitLine [] = []
splitLine x  = y : splitLine ys 
    where [(y,ys)] = lex x

findEntry :: [Entry] -> Key -> Maybe Entry
findEntry t k = find (\ (Entry key _) -> key == map toLower k) t

lookupKeyValue_ :: Entry->String->Maybe String
lookupKeyValue_ (Entry _ fs) k 
    | Just kv <- find (\(k',_) -> k' == (map toLower k) ) fs
    = Just (snd kv)
    | otherwise = Nothing

lookupKeyValue :: [Entry]->Key->String-> Maybe String
lookupKeyValue t ek k 
    | Just e <- find ( \(Entry ek' fs) -> ek' == (map toLower ek)) t 
    = lookupKeyValue_ e k
    | otherwise = Nothing

getWithTag :: [Entry]->String->[Entry]
getWithTag t s = filter (\e -> hasTag e s) t 

hasTag :: Entry->String->Bool
hasTag e s | Just s' <- lookupKeyValue_ e "tag"
           = isInfixOf (map toLower s ) (map toLower s')
           | otherwise = False

-- |Takes a field and a new value and a entry and returns 
-- an entry where that field has the given value
updateField :: String -> String -> Entry -> Entry
updateField _ _ e@(Entry k []) = e
updateField old new e@(Entry key fs) 
    | length back >= 1 =Entry key (front++(old,new):tail back)
    | otherwise = e
    where (front,back) = break (\(k,v)->old==k) fs

addField :: Field -> Entry -> Entry
addField n e@(Entry k fs) | not $ hasField e (fst n) = Entry k (n:fs)
                          | otherwise = e

hasField :: Entry -> String -> Bool
hasField (Entry _ fs) key = any (\ x -> fst x == key) fs 
