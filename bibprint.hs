module BibPrint where

import BibParse
import Data.List
import Data.String.Utils

escapeField :: String->String
showField  :: Field->String
showEntry  :: Entry->String

escapeField v       = concat ["{",strip v,"}"]
showField  (k,v)    = concat ["  ",k," = ",escapeField v,"\n"]
showEntry (Entry name ((_,doctype):f))    
    = "@" ++ doctype ++ " { " ++ name ++ ",\n" ++ fStr ++ "}\n"
      where fStr = concatMap showField f

instance Show Entry where
    show = showEntry