module UserConfig where

f0 = "~/papers/papers.bib" 
-- f1 = ... 
-- f2 = ... 
-- ... 
-- fn = ...

files = [f0,"theory.bib"] 

webbrowser = "lynx" 
psview = "gv"

views = [
 (".html", webbrowser), 
 (".htm", webbrowser), 
 (".pdf" , psview),
 (".ps" , psview),
 (".txt" , "less")]