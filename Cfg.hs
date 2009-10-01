module Cfg where

import Data.Maybe
import Data.List
import System.Path
import System.Directory
import Language.Haskell.Interpreter

data Cfg = Cfg {files :: [String], views :: [(String,String)]} deriving (Show)

defaultCfg = Cfg {files = [],views=[(".pdf","gv"),(".ps","gv")]} -- (1)
parsedCfg  = defaultCfg {files = ["theory.bib"]}

mkVal f n setFields = interpret (fromMaybe (show $ f defaultCfg) $ lookup n setFields)
fromEither a v = 
    case v of
      Left _ -> a
      Right b -> b


--ToDo:
-- refactor so adding Cfg fields is easier
--   now must add

makeCfg :: FilePath -> Interpreter Cfg
makeCfg home = do
  loadModules [fromMaybe "" (absNormPath home ".hsbib/config.hs")]
  exports <- getModuleExports "UserConfig"
  setTopLevelModules ["UserConfig"]
  setImportsQ [("Prelude", Nothing)]
  let setKeys = map name exports
  setVals <- mapM eval setKeys
  let setFields = zip setKeys setVals
  
  fileVal <- mkVal files "files" setFields (as :: [String]) -- (2)
  viewVal <- mkVal views "views" setFields (as :: [(String,String)]) -- (3)
  return Cfg {files=fileVal,views=viewVal} -- (4)

getCfg :: IO Cfg
getCfg = do
  home <- getHomeDirectory
  r <- runInterpreter (makeCfg home)
  return $ fromEither defaultCfg r
