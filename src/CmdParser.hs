module CmdParser (
  appInfo,
  Command(..),
  Opts(..),
  Args(..)

  ) where

import Options.Applicative

data Opts = Opts
  { optContext   :: Int
  , optFuzzy     :: String 
  , optRecursive :: Bool
  , optNumlines :: Bool }

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

recursive :: Parser Bool
recursive = switch ( long "recursive"
  <> short 'r'
  <> help "Recursively search subdirectories")

numlines :: Parser Bool
numlines = switch
  (  long "line-numbers"
   <> short 'n'
   <> help "Print the line numbers of each match: True, [False]" )

opts :: Parser Opts
opts = Opts <$> context <*> fuzzy <*> recursive <*> numlines

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
