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

{-
  @brief Represents the matches for a given file,
    with the filepath, the list of matches for the file,
    as well as the contents of the file.
-}
data FileMatch = FileMatch String [Match] [String]

instance Show FileMatch where
  show (FileMatch name matches _) = name ++ ":\n" ++ foldr (\x y -> ("\t" ++ show x ++ "\n") ++ y) "" matches

run :: Command -> IO ()
run (Command (Opts c fz r lns) (Args p fs)) = do
  files <- sequence (map (`File.dispatchFileExtraction` r) fs)
  let patt = p
      fileMatches = filter (\(FileMatch _ matches _) -> not (null matches))
        (map (\x -> exFileToMatch x (toFuzzy fz) patt) (concat files))
  mapM_ (\x -> printFileMatch x lns c) fileMatches

-- | Creates a FileMatch from the file, fuzziness of the query, and the string pattern.
exFileToMatch :: ExFile -> Fuzziness -> String -> FileMatch
exFileToMatch (ExFile filepath contents) fz pattern = do
  let matches = dispatchMatching contents 0 fz pattern
  FileMatch filepath matches contents

-- | properly formats the console output for a FileMatch.
printFileMatch :: FileMatch -> Bool -> Int -> IO ()
printFileMatch (FileMatch filepath matches content) lns numContextLines = do
  let contextLines = fetchContext content numContextLines
      divider = getDivider content numContextLines
  setSGR [SetColor Foreground Vivid Blue]
  putStr (filepath ++ " (" ++ show (length matches) ++ " matches):\n")
  setSGR [SetColor Foreground Vivid White]
  putStrLn divider
  mapM_ (\x -> printMatch x lns contextLines divider) matches
  putStrLn ""

-- | Porperly formats the console output for a Match.
printMatch :: Match -> Bool -> (Int -> ([(String,Int)],[(String,Int)])) -> String -> IO ()
printMatch (Match line matchedPattern matchIdx lineNum) lns contextGetter divider = do
  let matchParts = getLineParts (Match line matchedPattern matchIdx lineNum)
      (trailing,leading) = contextGetter lineNum
  mapM_ (\(ln,idx) -> printLine (ln,"","") lns idx) trailing
  printLine matchParts lns lineNum
  mapM_ (\(ln,idx) -> printLine (ln,"","") lns idx) leading
  putStr divider 

printLine :: (String,String,String) -> Bool -> Int -> IO ()
printLine (f,s,t) lns lineNum = if lns 
  then printLineWithNum (f,s,t) lineNum
  else printLineNoNum (f,s,t)
  

printLineWithNum :: (String,String,String) -> Int -> IO ()
printLineWithNum (f,s,t) lineNum = do
  setSGR[SetColor Foreground Vivid Green]
  putStr (show (lineNum+1) ++ ":\t")
  setSGR [SetColor Foreground Vivid White]
  putStr f
  setSGR [SetColor Foreground Vivid Red]
  putStr s
  setSGR [SetColor Foreground Vivid White]
  putStrLn t

printLineNoNum :: (String,String,String) -> IO ()
printLineNoNum (f,s,t) = do
  setSGR [SetColor Foreground Vivid White]
  putStr ("\t" ++ f)
  setSGR [SetColor Foreground Vivid Red]
  putStr s
  setSGR [SetColor Foreground Vivid White]
  putStrLn t

getDivider :: [String] -> Int -> String
getDivider _ 0 = []
getDivider lines _ = do 
  let maxLineLength = foldr (max . length) 0 lines
  ([1..(min maxLineLength 75)] >> "=") ++ "\n"

-- | Converts the given parameter to fuzziness level.
toFuzzy :: [Char] -> Fuzziness
toFuzzy "NONE" = NoFuzzy
toFuzzy "LOW" = LowFuzzy 
toFuzzy "MED" = MediumFuzzy 
toFuzzy "HIGH" = HighFuzzy 
toFuzzy err = error (err ++ " is not one of: \"NONE\", \"LOW\", \"MED\", \"HIGH\"")

{-
  @brief Creates a tuple containing the trailing and leading context lines.
-}
fetchContext :: [String] -> Int -> Int -> ([(String,Int)], [(String,Int)])
fetchContext _ 0 _ = ([],[])
fetchContext content numLines matchIdx = (trailing content matchIdx, leading content matchIdx)
  where trailing content matchIdx = zip (drop trailingStartIdx (take trailingEndIdx content)) [trailingStartIdx .. trailingEndIdx]
        leading content matchIdx = zip (drop leadingStartIdx (take leadingEndIdx content)) [leadingStartIdx .. leadingEndIdx]
        trailingStartIdx = max 0 (matchIdx - numLines)
        trailingEndIdx = matchIdx
        leadingStartIdx = matchIdx + 1
        leadingEndIdx = min (matchIdx + numLines + 1) (length content)

