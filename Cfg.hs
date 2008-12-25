module Cfg where

import Data.ConfigFile
import System.Directory
import System.FilePath
import Data.Either.Utils
import System.FilePath

defaultCFGFileName = "bibconsolerc"
maxInterpolationDepth = 10
cp = emptyCP { usedefault = False , accessfunc = interpolatingAccess maxInterpolationDepth }
--ToDo:
--fallback to home directory ".bibconsole"
--implement query storage NEED TO FINALIZE FORMAT for actual queries
--    Perhaps  find query:books?




--loadCFGFile :: fileName->ConfigFileData
--default behavior is try to load defaultCFGFileName from current directory then home directory
--if the above fails
--then load defaultCFGData
loadDefaultCFGFile = do
   cdir <- getCurrentDirectory 
   cfgFile  <- readfile cp (cdir </> defaultCFGFileName) 
   return $ forceEither cfgFile

getBibFilePaths :: ConfigParser -> [String]
getBibFilePaths cp = map snd $ forceEither $ items cp "bibfiles"

getDocumentAssociations :: ConfigParser -> [(String,String)]
getDocumentAssociations cp = 
   zip extensions viewers
   where extensions = filter ((/= '\'').head) $ forceEither $ options cp "file_assoc" --exludes the substitute vars which start with '
         viewers    =  map (forceEither . get cp "file_assoc")  extensions


lookupDocumentViewer :: [(String,String)]->FilePath->Maybe String
lookupDocumentViewer das fp = 
    case lookup fext das of
      Just a  -> Just a
      Nothing -> lookup "defaultviewer" das
    where fext = takeExtension fp

