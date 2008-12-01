module BibUtils where

import Data.List
import Data.Maybe
import Data.Char
import BibParse

findEntry :: [Entry]->Key->Maybe Entry
findEntry t ek = find (\(ek',_) -> ek' == (map toLower ek) ) t 

lookupKeyValue_ :: Entry->String->Maybe String
lookupKeyValue_ (n,fs) k = case (find (\(k',_) -> k' == (map toLower k) ) fs) of
                             Just kv -> Just (snd kv)
                             Nothing -> Nothing

lookupKeyValue :: [Entry]->Key->String-> Maybe String
lookupKeyValue t ek k = case (find ( \(ek',fs) -> ek' == (map toLower ek) ) t ) of 
                          Just e -> lookupKeyValue_ e k
                          Nothing -> Nothing
getWithTag :: [Entry]->String->[Entry]
getWithTag t s = filter (\e -> hasTag e s) t 
hasTag :: Entry->String->Bool
hasTag e s = case lookupKeyValue_ e "tag" of
               Nothing -> False
               Just s' -> isInfixOf (map toLower s ) (map toLower s')
