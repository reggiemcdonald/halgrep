module Lib
    ( someFunc
    ) where

import Options.Applicative
import File
import Matching
import System.Console.ANSI

someFunc :: IO ()
someFunc = run =<< execParser appInfo

-- | Represents a file match with the path of the file and the matches
data FileMatch = FileMatch String [Match]

instance Show FileMatch where
  show (FileMatch name matches) = name ++ ":\n" ++ foldr (\x y -> ("\t" ++ show x ++ "\n") ++ y) "" matches

run :: Command -> IO ()
run (Command (Opts c fz) (Args p fs)) = do
  putStrLn $ "Files to search: " ++ foldr (\ x y -> x ++ " " ++ y) "" fs
  putStrLn $ "Pattern to match on: " ++ p
  putStrLn $ "Options: Context Lines: " ++ show c ++ " fuzzy level: " ++ fz
  putStrLn "Results: "
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

data Opts = Opts
  { optContext :: Int
  , optFuzzy   :: String }

context :: Parser Int
context = option auto
  (  long "context-lines"
   <> short 'c'
   <> metavar "NUM"
   <> value 0
   <> help "Print NUM lines preceding and following each matched line" )

fuzzy :: Parser String
fuzzy = strOption
  ( long "fuzzy"
  <> short 'f'
  <> metavar "LEVEL"
  <> value "NONE"
  <> help "Desired LEVEL of fuzziness: NONE, LOW, MED, HIGH")


opts :: Parser Opts
opts = Opts <$> context <*> fuzzy

data Args = Args
  { argPattern :: String
  , argFiles :: [String] }

pattrn :: Parser String
pattrn = argument str
  ( metavar "PATTERN"
  <> help "Pattern to search for")

files :: Parser [String]
files = some (argument str
              ( metavar "FILES ..."
              <> help "Files to search"))

args :: Parser Args
args = Args <$> pattrn <*> files

data Command = Command Opts Args

cmd :: Parser Command
cmd = Command <$> opts <*> args

appInfo :: ParserInfo Command
appInfo = info (cmd <**> helper)
  ( fullDesc
  <> progDesc "Performs a search for the specified pattern in the given files."
  <> header "halgrep - a text search utility" )

-- | Converts the given parameter to fuzziness level.
toFuzzy :: [Char] -> Fuzziness
toFuzzy "NONE" = NoFuzzy
toFuzzy "LOW" = LowFuzzy 
toFuzzy "MED" = MediumFuzzy 
toFuzzy "HIGH" = HighFuzzy 
toFuzzy err = error (err ++ " is not one of: \"NONE\", \"LOW\", \"MED\", \"HIGH\"")

