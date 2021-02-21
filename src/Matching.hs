module Matching (
  Match(..),
  Fuzziness(..),
  findMatches,
  findMatchesFuzzy,

  -- Visible for testing
  newPatternMask,
  newR,
  bitap
  -- End of visible for testing
) where

import Text.Regex.TDFA ((=~))
import Data.Bits ( Bits(shiftL, complement, (.|.), (.&.)) )
import Data.Char (ord)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

{-
  Match contains the line that was matched, the matched text,
  and line number of a regex match.
-}
data Match = Match String String Int
  deriving (Show)

instance Eq Match where
  (Match line1 match1 idx1) == (Match line2 match2 idx2) = (line1 == line2) && (match1 == match2) && (idx1 == idx2)

{-
  Fuzziness is a enumeration for the edit distances of the fuzzy text search.
-}
data Fuzziness = LowFuzzy | MediumFuzzy | HighFuzzy

-- | Returns the edit distance corresponding to the amount fuzziness permitted in the fuzzy text search.
editDistance :: Fuzziness -> Int
editDistance LowFuzzy = 1
editDistance MediumFuzzy = 3
editDistance HighFuzzy = 5

instance Show Fuzziness where
  show LowFuzzy = "Fuzzy: Low(3)"
  show MediumFuzzy = "Fuzzy: Med(5)"
  show HighFuzzy = "Fuzzy: High(7)"

-- | Returns a list of matches found in the given list of lines from the file read.
findMatches :: [String] -> Int -> String -> [Match]
findMatches lines startingLineNum regex = foldr findMatches' [] (zip lines [startingLineNum .. (startingLineNum+length lines)])
  where findMatches' (str, line) y = if str =~ regex then Match str (str =~ regex) line:y else y

-- | Matches the given string to the lines read from the file according to the fuzziness level specified.
findMatchesFuzzy :: [String] -> Int -> Fuzziness -> String -> [Match]
findMatchesFuzzy lines startingLineNum fuzzyLevel fuzzyStr = foldr (\(text, idx) y -> 
  findMatchesFuzzy' text idx (bitap text k m (newPatternMask text fuzzyStr) (newR k)) y) 
  [] (zip lines [startingLineNum .. (startingLineNum + length lines)])
    where findMatchesFuzzy' _ _ Nothing y = y
          findMatchesFuzzy' text idx (Just str) y = Match text str idx:y
          k = editDistance fuzzyLevel
          m = length fuzzyStr

{-
  The code and all helpers for this algorithm were adapted from the C version at https://en.wikipedia.org/wiki/Bitap_algorithm

  The bitap algorithm is an approximate string matching algorithm that efficiently 
  identifies approximate substring matches in a given text.

  The algorithm is based upon the Levenshtein distance. As such, the algorithm consumes
  a k value, which defines the number of edits allowed to consider something to be a match. 

  For more information on approximate substring matching, refer to agrep documentation:
  https://github.com/Wikinaut/agrep/tree/master/docs

-}
bitap :: String -> Int -> Int -> IntMap Int64 -> [Int64] -> Maybe String
bitap text k m patternMask r = bitap' text k m patternMask r 0
  where bitap' [] _ _ _ _ _ = Nothing
        bitap' (h:t) k m patternMask r idx = do
          let oldR = head r
          let x = (oldR .|. IntMap.findWithDefault 0 (ord h) patternMask) `shiftL` 1
          let newR = x:substitute h oldR (tail r) patternMask
          if isMatch newR k m
            then Just (take m (drop (idx - m + 1) text))
            else bitap' t k m patternMask newR (idx+1)

{-
  Accessory functions for the bitap algorithm.
-}

newPatternMask :: String -> String -> IntMap Int64
newPatternMask abet pattern = newPatternMask' (zip pattern [0..(length pattern)]) initd
  --    initialize the IntMap with ~0 for each letter of the alphabet
  where initd = foldr (\x y -> IntMap.insert (ord x) (complement 0) y) IntMap.empty abet
  --    when all letters of the pattern have been visited, return the map      
        newPatternMask' [] mapping = mapping
  --    otherwise, compute the pattern mask
        newPatternMask' ((letter, idx):t) mapping = newPatternMask' t (IntMap.insert (ord letter) 
          (IntMap.findWithDefault 0 (ord letter) mapping .&. complement ((1 :: Int64) `shiftL` idx)) mapping)

-- | Returns a new IntMap initialized for R in the bitap algorithm.
newR :: Int -> [Int64]
newR k = [complement 1 | _ <- [0..k]]

isMatch :: [Int64] -> Int -> Int -> Bool
isMatch r k m = 0 == (last r .&. ((1 :: Int64) `shiftL` m))

substitute :: Char -> Int64 -> [Int64] -> IntMap Int64 -> [Int64]
substitute _ _ [] _ = []
substitute c oldR (h:t) patternMask = do
  let tmp = h
  let x = (oldR .&. (h .|. IntMap.findWithDefault 0 (ord c) patternMask)) `shiftL` 1
  x:substitute c tmp t patternMask


