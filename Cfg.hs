module Cfg where

import System.Directory
import System.FilePath
import System.FilePath

import Data.Maybe
import Data.List
import Data.Either.Utils
import Language.Haskell.Interpreter

data Cfg = Cfg {files :: [String], views :: [(String,String)]} deriving (Show)

defaultCfg = Cfg {files = [],views=[(".pdf","gv"),(".ps","gv")]} -- (1)
parsedCfg  = defaultCfg {files = ["theory.bib"]}

mkVal f n setFields = interpret (fromMaybe (show $ f defaultCfg) $ lookup n setFields)

--ToDo:
-- fallback to home directory ".bibconsole"
-- refactor so adding Cfg fields is easier
--   now must add

getCfg :: Interpreter Cfg
getCfg = do
  loadModules ["/home/cf/.hsbib/config.hs"]
  exports <- getModuleExports "UserConfig"
  setTopLevelModules ["UserConfig"]
  setImportsQ [("Prelude", Nothing)]
  let setKeys = map name exports
  setVals <- mapM eval setKeys
  let setFields = zip setKeys setVals
  
  fileVal <- mkVal files "files" setFields (as :: [String]) -- (2)
  viewVal <- mkVal views "views" setFields (as :: [(String,String)]) -- (3)
  return Cfg {files=fileVal,views=viewVal} -- (4)

main :: IO ()
main = do r <- runInterpreter getCfg
          case r of
            Left err -> putStrLn $ "Ups... " ++ show err
            Right cfg -> print cfg

say :: String -> Interpreter ()
say = liftIO . putStrLn


