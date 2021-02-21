module MatchingSpec (spec) where
import Matching
import Test.Hspec
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

spec = do
  describe "findMatches" $ do
    
    -- Initialize test data for findMatches
    let singleString = ["This is a matching string"]
    let multipleString = [
            "This is a matching string",
            "not a matching string",
            "some other non-matching string",
            "This is another great matching string!"
          ]
    let regexStart = "^This"
    let regexEnd = "string$"
    -- End of test data

    it "should find a single match when given a single matching line" $
      findMatches singleString 1 regexStart `shouldBe` [Match (head singleString) "This" 1]
    
    it "should find no match when given an empty list" $
      findMatches [] 1 "" `shouldBe` []
      

    it ("should find multiple matches with " ++ regexStart) $
      findMatches multipleString 10 regexStart `shouldBe` 
        [
          Match (head multipleString) "This" 10,
          Match (multipleString !! 3) "This" 13
        ]

    it ("should find multiple matches with " ++ regexEnd) $
      findMatches multipleString 10 regexEnd `shouldBe` 
        [
          Match (head multipleString) "string" 10,
          Match (multipleString !! 1) "string" 11, 
          Match (multipleString !! 2) "string" 12
        ]
  
  describe "makeAlphabet" $
    it "should remove duplicate characters" $
      makeAlphabet "mississippi" `shouldMatchList` "misp"

  describe "newR" $
    it "should create a new [Int] with k+1 entries" $
      newR 2 `shouldBe` [-2,-2,-2]

  describe "alphabetMask" $
    it "should generate a bit mask for each of the characters in the pattern" $
      IntMap.toList (newPatternMask (makeAlphabet "mississippi") "mis") `shouldMatchList` 
        [
          (105,-3),
          (109,-2),
          (112,-1),
          (115,-5)
        ]

  describe "findMatchesFuzzy" $ do
    -- Initialize test data
    let wildStrings = 
          [
            -- https://en.wikipedia.org/wiki/Alpaca
            "Alpacas are kept in herds that graze on the level heights of the Andes of Southern Peru, Western Bolivia, Ecuador, " 
            ++ "and Northern Chile at an altitude of 3,500 to 5,000 metres (11,000 to 16,000 feet) above sea level.",
            
            -- https://en.wikipedia.org/wiki/Pika
            "Pikas prefer rocky slopes and graze on a range of plants, mostly grasses, flowers, and young stems.",

            -- https://en.wikipedia.org/wiki/Anteater
            "Extant species are the giant anteater Myrmecophaga tridactyla, about 1.8 m (5 ft 11 in) long including the tail"
          ]
    -- End of test data

    it  "should find a single match when a match exists at LowFuzzy" $ do
      {-
        Start: tridpctyla
        Edit: repace 'p' with 'a' => tridactyla (1 edit)
        Match
      -}
      let fuzzyPattern = "tridpctyla"
      findMatchesFuzzy wildStrings 0 LowFuzzy fuzzyPattern `shouldBe` [Match (wildStrings !! 2) "tridactyla" 2]

    it "should find multiple matches when a match exists" $ do
      let fuzzyPattern = "if"
      findMatchesFuzzy wildStrings 0 LowFuzzy fuzzyPattern `shouldBe`
        {-
          Explanation:
            String 1: The first occurrence in the string that is approximately equal (~=~) to "if" is "in".
            String 2: The first occurrence is in "Pika", more specifically "ik".
            String 3: The first occurrence is in "species", more specifically the "ie".
        -}
        [
          Match (head wildStrings) "in" 0,
          Match (wildStrings !! 1) "ik" 1,
          Match (wildStrings !! 2) "ie" 2
        ]

    it "should not match with low fuzz" $ do
      {-
        Start: Myrmacephaga
        Edit: replace 'a' with 'e' => Myrmecephaga (1 edits)
        No match: Edit distance > threshold
      -}
      let fuzzyPattern = "Myrmacephaga"
      findMatchesFuzzy wildStrings 0 LowFuzzy fuzzyPattern `shouldBe` []

    it "should match with high fuzz" $ do
      let fuzzyPattern = "Myrmacephaga"
      {-
        Start: Myrmacephaga
        Edit: replace 'a' with 'e' => Myrmecephaga (1 edits)
        Edit: replace 'e' with 'o' => Myrmecophaga (2 edits)
        Match
      -}
      findMatchesFuzzy wildStrings 0 MediumFuzzy  fuzzyPattern `shouldBe` [Match (last wildStrings) "Myrmecophaga" 2]
    
    it "should handle deletion of characters" $ do
      let fuzzyPattern = "Pkas"
      {-
        Here we're testing the deletion of characters. "Pikas" ~=~ "Pkas"
        However, with a pattern of length m, then the returned match will also be of length m.
        In this case, we match "ikas", which has an edit distance of 1 from "Pkas":
          Start: "ikas"
          Edit: replace 'i' with 'P' => Pkas (1 edit)
          Match
      -}
      findMatchesFuzzy wildStrings 0 LowFuzzy fuzzyPattern `shouldContain` [Match (wildStrings !! 1) "ikas" 1]
