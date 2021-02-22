-- | File handling

module File where

import Control.Exception (catch, IOException)
import Control.Monad (forM, filterM)
import System.IO (hGetContents, IOMode(ReadMode), openFile)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

{-|
     @data Exfile
     @brief Exfile holds an ordered array of lines from a file and
     the relative filepath.
     @var Exfile::filepath
     Member filepath String contains relative path.
     @var Exfile::contents
     Member contents [String] contains array of lines from the file
-}
data ExFile = ExFile {
    filepath :: String
  , contents :: [String]
  } deriving Show

{-|
    @brief Given a filepath it will extract the contents of the file and
    will return them as an array of lines.
    @param filepath String
    @returns array of lines from the file
-}
extractFile :: FilePath -> IO ExFile
extractFile filepath = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  let exfile = ExFile{
        filepath = filepath,
        contents = (lines contents)
                     }
  return exfile

{-|
   @brief Given a directory it will extract all the files in
   the directory.
   @param filepath
   @returns exfiles from the directory
-}
extractFilesInDir :: FilePath -> IO [ExFile]
extractFilesInDir path = do
  files <- getFiles path
  exfiles <- mapM extractFile files
  return exfiles

{-|
   @brief Given a directory it will extract all the files in
   the directory and its sub directories
   @param filepath
   @returns exfiles from the directory and subdirectories
-}
extractFilesRecursive :: FilePath -> IO [ExFile]
extractFilesRecursive path = do
  exfiles <- extractFilesInDir path
  -- putStrLn $ show exfiles
  dirs <- getDirectories path
  files <- forM dirs $ \d -> do
    subExFiles <- extractFilesInDir d
    s <- getFileStatus d
    if isDirectory s
      then extractFilesRecursive d
      else return subExFiles
  return (exfiles ++ (concat files))

{-|
   @brief Given a directory it will return a list of its
   subdirectories.
   @param filepath
   @returns list of directories
   TODO ERROR handling
-}
getDirectories :: FilePath -> IO [FilePath]
getDirectories filepath = do
  paths <- extractPaths filepath
  dirs <- filterM doesDirectoryExist paths
  return dirs

{-|
   @brief Given a directory it will returns a list of
   files.
   @param filepath
   @returns list of filepaths
   TODO ERROR handling
-}
getFiles :: FilePath -> IO [FilePath]
getFiles filepath = do
  paths <- extractPaths filepath
  files <- filterM doesFileExist paths
  return files

{-|
   @brief Given a directory it will returns a relative path
   list of its contents.
   @param filepath
   @returns list of relative filepaths
   TODO ERROR handling
-}
extractPaths :: FilePath -> IO [[Char]]
extractPaths filepath = do
  contents <- catch (listDirectory filepath) (\e -> do
        -- TODO better error handling
        putStrLn $ show (e :: IOException)
        return []
        )
  let fullpaths =
        if last filepath == '/' then map (filepath ++) contents
        else map ((filepath ++ "/") ++) contents
  return fullpaths
