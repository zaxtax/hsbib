{-# OPTIONS_GHC -XPatternGuards -XDeriveDataTypeable #-} 
module Cfg where

import System.Directory
import System.FilePath
import System.FilePath

import Data.Generics

import Data.List
import Data.Either.Utils
import Language.Haskell.Interpreter

data Cfg = Cfg {files :: [String], views :: [(String,String)]} deriving (Typeable, Show)

defaultCfg = Cfg {files = [],views=[(".pdf","gv"),(".ps","gv")]}
parsedCfg  = defaultCfg {files = ["theory.bib"]}

unFun (Fun x) = x

--ToDo:
--fallback to home directory ".bibconsole"


{- 

loadCfg defaultCfg {files,defaultviewer,views}

-}

-- loadDefaultCFGFile = do
--    cdir <- getCurrentDirectory 
--    cfgFile  <- readfile cp (cdir </> defaultCFGFileName) 
--    return $ forceEither cfgFile

-- getBibFilePaths :: ConfigParser -> [String]
-- getBibFilePaths cp = map snd $ forceEither $ items cp "bibfiles"

-- getDocumentAssociations :: ConfigParser -> [(String,String)]
-- getDocumentAssociations cp = 
--    zip extensions viewers
--    where extensions = filter ((/= '\'').head) $ forceEither $ options cp "file_assoc" --exludes the substitute vars which start with '
--          viewers    =  map (forceEither . get cp "file_assoc")  extensions


lookupDocumentViewer :: [(String,String)]->FilePath->Maybe String
lookupDocumentViewer das fp = 
    case lookup fext das of
      Just a  -> Just a
      Nothing -> lookup "defaultviewer" das
    where fext = takeExtension fp


getCfg :: Interpreter Cfg
getCfg = do
  loadModules ["/home/cf/.hsbib/config.hs"]
  exports <- getModuleExports "UserConfig"
  setTopLevelModules ["UserConfig","Cfg"]
  setImportsQ [("Prelude", Nothing)]
  let setKeys = filter (flip elem ["files","views"]) (map unFun exports)
  setVals <- mapM eval setKeys
  let setFields = zipWith (\ k v -> k++"="++v) setKeys setVals
  let expr =  "defaultCfg {" ++ (intercalate "," setFields) ++ "}"
  interpret expr (as :: Cfg)

main :: IO ()
main = do r <- runInterpreter getCfg
          case r of
            Left err -> putStrLn $ "Ups... " ++ (show err)
            Right _ -> putStrLn "that's all folks"

say :: String -> Interpreter ()
say = liftIO . putStrLn


