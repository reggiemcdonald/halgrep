module Lib
    ( someFunc
    ) where

import Options.Applicative
import File
import Matching

someFunc :: IO ()
someFunc = run =<< execParser appInfo

run :: Command -> IO ()
run (Command (Opts c fz) (Args p fs)) = do
  files <- sequence (map File.extractFile fs)
  let patt = p
      fileContents = map (\ (ExFile filepath contents ) -> contents) (concat files)
      matches = map (\ x -> Matching.findMatches x 0 p) fileContents
      matchLines = map (\ (Matching.Match m st i) -> m) (concat matches)
  putStrLn $ "Files to search: " ++ foldr (\ x y -> x ++ " " ++ y) "" fs
  putStrLn $ "Pattern to match on: " ++ patt
  putStrLn $ "Options: Context Lines: " ++ show c ++ " fuzzy level: " ++ fz
  putStrLn $ "Results: "
  mapM_ print matchLines

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


--findMatches lines startingLineNum regex = []
--extractFile path = path
-- getFiles path = []
