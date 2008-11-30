module BibPrint where

import Data.List
import Data.String.Utils

keyToStr   :: String->String
valueToStr :: String->String
fieldToStr :: (String,String)->String
entryToStr :: (String,[(String,String)])->String

keyToStr = id
valueToStr v        = concat ["{",strip v,"}"]
fieldToStr (k,v)    = concat ["  ",k," = ",valueToStr v,"\n"]
entryToStr (name,(_,doctype):f)    
           = "@" ++ doctype ++ " { " ++ name ++ ",\n" ++ fStr ++ "}\n"
               where fStr = concatMap fieldToStr f

--Should have a more idealistic way of getting the doctype