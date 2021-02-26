module Lib
    ( someFunc
    ) where

import Options.Applicative
import File
import Matching
import System.Console.ANSI
import CmdParser(Command(..), Opts(..), Args(..), appInfo)

someFunc :: IO ()
someFunc = run =<< execParser CmdParser.appInfo

-- | Represents a file match with the path of the file and the matches
data FileMatch = FileMatch String [Match]

instance Show FileMatch where
  show (FileMatch name matches) = name ++ ":\n" ++ foldr (\x y -> ("\t" ++ show x ++ "\n") ++ y) "" matches

run :: Command -> IO ()
run (Command (Opts c fz) (Args p fs)) = do
  files <- sequence (map File.extractFile fs)
  let patt = p
      fileMatches = filter (\(FileMatch path matches) -> not (null matches))
        (map (\x -> exFileToMatch x (toFuzzy fz) patt) (concat files))
  mapM_ printFileMatch fileMatches

-- | Creates a FileMatch from the file, fuzziness of the query, and the string pattern.
exFileToMatch :: ExFile -> Fuzziness -> String -> FileMatch
exFileToMatch (ExFile filepath contents) fz pattern = do
  let matches = dispatchMatching contents 0 fz pattern
  FileMatch filepath matches

-- | properly formats the console output for a FileMatch.
printFileMatch :: FileMatch -> IO ()
printFileMatch (FileMatch filepath matches) = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr (filepath ++ ":\n")
  mapM_ printMatch matches
  putStr "\n"

-- | Porperly formats the console output for a Match.
printMatch :: Match -> IO ()
printMatch m = do
  let (f,s,t) = getLineParts m
  setSGR [SetColor Foreground Vivid White]
  putStr ("\t" ++ f)
  setSGR [SetColor Foreground Vivid Red]
  putStr s
  setSGR [SetColor Foreground Vivid White]
  putStrLn t


-- | Converts the given parameter to fuzziness level.
toFuzzy :: [Char] -> Fuzziness
toFuzzy "NONE" = NoFuzzy
toFuzzy "LOW" = LowFuzzy 
toFuzzy "MED" = MediumFuzzy 
toFuzzy "HIGH" = HighFuzzy 
toFuzzy err = error (err ++ " is not one of: \"NONE\", \"LOW\", \"MED\", \"HIGH\"")

