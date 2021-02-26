-- | File handling

module File (
  ExFile(..),
  dispatchFileExtraction,
            )where

import Control.Exception (catch, IOException)
import Text.Regex.PCRE ((=~))
import Control.Monad (forM, filterM)
import System.IO (hGetContents, IOMode(ReadMode), openFile, latin1, hSetEncoding, hClose)
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

dotfileRegEx = "^\\/?(?:\\w+\\/)*(\\.\\w+)" :: String

{-|
    @brief Given a filepath and a recursive flag, dispatches
    the correct file finder.
    @throws IOException if file/dir does not exist
    @param filepath String
    @param recursive Bool
    @returns list of files and their contents
-}
dispatchFileExtraction :: FilePath -> Bool -> IO [ExFile]
dispatchFileExtraction filepath recursive
  | recursive = extractFilesRecursive filepath
  | otherwise = extractFile filepath

{-|
    @brief Given a filepath it will extract the contents of
    the file, Given a path to a direcotry it will return the
    contents of all the files in the directory.
    @throws IOException if file/dir does not exist
    @param filepath String
    @returns array of lines from the file
-}
extractFile :: FilePath -> IO [ExFile]
extractFile filepath = do
    res <- doesDirectoryExist filepath
    if res
      then  _extractFilesInDir filepath
      else  do
        ret <-_extractFile filepath
        return [ret]

{-|
    @brief Given a filepath it will extract the contents of the file and
    will return them as an array of lines.
    @throws IOException if file does not exist
    @param filepath String
    @returns array of lines from the file
-}
_extractFile :: FilePath -> IO ExFile
_extractFile filepath = do
  handler <- openFile filepath ReadMode
  hSetEncoding handler latin1
  contents <- hGetContents handler
  -- Forcing strict evaluation from 
  --   https://stackoverflow.com/questions/296792/haskell-io-and-closing-files
  reduceToNormal contents `seq` hClose handler
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
_extractFilesInDir :: FilePath -> IO [ExFile]
_extractFilesInDir path = do
  files <- _getFiles path
  exfiles <- mapM _extractFile files
  return exfiles

{-|
   @brief Given a directory it will extract all the files in
   the directory and its sub directories
   @param filepath
   @returns exfiles from the directory and subdirectories
-}
extractFilesRecursive :: FilePath -> IO [ExFile]
extractFilesRecursive path = do
  exfiles <- _extractFilesInDir path
  -- putStrLn $ show exfiles
  dirs <- _getDirectories path
  files <- forM dirs $ \d -> do
    subExFiles <- _extractFilesInDir d
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
-}
_getDirectories :: FilePath -> IO [FilePath]
_getDirectories filepath = do
  paths <- _extractPaths filepath
  dirs <- filterM doesDirectoryExist paths
  return dirs

{-|
   @brief Given a directory it will return a list of
   files.
   @param filepath
   @returns list of filepaths
-}
_getFiles :: FilePath -> IO [FilePath]
_getFiles filepath = do
  paths <- _extractPaths filepath
  files <- filterM doesFileExist paths
  return files

{-|
   @brief given a filepath it will return true if its a dotfile
   @param filepath
   @returns boolean
-}
_isDotFile :: FilePath -> Bool
_isDotFile f = f =~ dotfileRegEx

{-|
   @brief Given a directory it will return a relative path
   list of its contents.
   @param filepath
   @throws IOException
   @returns list of relative filepaths
-}
_extractPaths :: FilePath -> IO [[Char]]
_extractPaths filepath = do
  -- contents <- catch (listDirectory filepath) (\e -> do
  --       putStrLn $ show (e :: IOException)
  --       return []
  --       )
  files <- listDirectory filepath
  let contents = filter (\f -> not (_isDotFile f)) files
  let fullpaths =
        if last filepath == '/' then map (filepath ++) contents
        else map ((filepath ++ "/") ++) contents
  return fullpaths

{-| 
    @brief Traverses the given list. This forces reading on lazy IO.
    See here https://stackoverflow.com/questions/296792/haskell-io-and-closing-files for more information.
    @param list
    @returns empty tuple
-}
reduceToNormal :: [a] -> ()
reduceToNormal [] = ()
reduceToNormal (_:t) = reduceToNormal t
