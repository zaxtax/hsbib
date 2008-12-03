module BibPrint where

import BibParse
import Data.List
import Data.String.Utils

escapeField :: String->String
showField  :: Field->String
showEntry  :: Entry->String

escapeField v       = concat ["{",strip v,"}"]
showField  (k,v)    = concat ["\n  ",k," = ",escapeField v]
showEntry (Entry name xs)    
    = "@" ++ doctype ++ " {" ++ name ++ "," ++ fStr ++ "\n}"
      where fStr = intercalate "," (map showField fs)
            ([(_,doctype)],fs) = partition (\(k,_)->k=="doctype") xs

instance Show Entry where
    show = showEntry