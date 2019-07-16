{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))
import Data.Version (makeVersion)
import System.Exit (exitFailure)
import Test.HUnit

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests =
        TestList
          [ TestLabel "Properties" $
            TestList
              [ generalCategory
              , canonicalCombiningClass
              , nameAliases
              , ages
              , script
              , scriptExts
              ]
          , TestLabel "Names" $
            TestList
              [ testFullNames @UCD.Block "Block"
              , testFullNames @UCD.Script "Script"
              ]
          ]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

generalCategory :: Test
generalCategory =
  TestLabel "General category" $
  TestCase $ do
    reference <- readFullTable enumP "generated/test_data/general_category.txt"
    assertEqual "Table size" (fromEnum maxCp - fromEnum minCp + 1) $
      length reference
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refGC) ->
      assertEqual (show cp) refGC $ UCD.generalCategory cp

canonicalCombiningClass :: Test
canonicalCombiningClass =
  TestLabel "Canonical combining class" $
  TestCase $ do
    reference <-
      readFullTable
        A.decimal
        "generated/test_data/canonical_combining_class.txt"
    assertEqual "Table size" (fromEnum maxCp - fromEnum minCp + 1) $
      length reference
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refCCC) ->
      assertEqual (show cp) refCCC $ UCD.canonicalCombiningClass cp

nameAliases :: Test
nameAliases =
  TestLabel "Name aliases" $
  TestCase $ do
    reference <- readFullTable parser "generated/test_data/name_aliases.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refNA) ->
      assertEqual (show cp) (sort refNA) $ UCD.nameAliases cp
  where
    parser :: A.Parser [(UCD.NameAliasType, B.ByteString)]
    parser =
      enclosedP "[" "]" $
      enclosedP
        "("
        ")"
        ((,) <$> enumP <* "," <*> enclosedP "\"" "\"" (A.takeWhile (/= '"'))) `A.sepBy`
      ","

ages :: Test
ages =
  TestLabel "Ages" $
  TestCase $ do
    referenceMinor <-
      readFullTable A.decimal "generated/test_data/age_minor.txt"
    referenceMajor <-
      readFullTable A.decimal "generated/test_data/age_major.txt"
    for_ (zip3 [minCp .. maxCp] referenceMajor referenceMinor) $ \(cp, refMaj, refMin) ->
      if refMaj == 0
        then assertEqual (show cp) Nothing $ UCD.age cp
        else assertEqual (show cp) (Just $ makeVersion [refMaj, refMin]) $
             UCD.age cp

script :: Test
script =
  TestLabel "Script" $
  TestCase $ do
    reference <- readFullTable enumP "generated/test_data/script.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refScr) ->
      assertEqual (show cp) refScr $ UCD.script cp

scriptExts :: Test
scriptExts =
  TestLabel "Script extensions (raw)" $
  TestCase $ do
    reference <- readFullTable parser "generated/test_data/script_exts.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refSE) ->
      assertEqual (show cp) (sort refSE) $ sort $ UCD.scriptExtensionsRaw cp
  where
    parser = enclosedP "[" "]" $ enumP `A.sepBy` ","

testFullNames ::
     forall p. (Show p, UCD.EnumeratedProperty p)
  => String
  -> Test
testFullNames name =
  TestLabel name $
  TestCase $
  for_ [minBound @p .. maxBound @p] $ \pval ->
    let pstr =
          map toLower . filter (\c -> c /= '_' && c /= '-') . B.unpack $
          UCD.fullPropertyValueName pval
        sstr = map toLower $ show pval
     in assertEqual (show pval) (take (length pstr) sstr) pstr

maxCp :: UCD.CodePoint
maxCp = maxBound

minCp :: UCD.CodePoint
minCp = minBound

readFullTable :: A.Parser a -> FilePath -> IO [a]
readFullTable p file = do
  txt <- B.readFile file
  let parsed = A.parseOnly (replicateM 0x110000 (p <* A.char '\n')) txt
  case parsed of
    Left err -> assertFailure err
    Right vals -> pure vals

enumP :: (Enum a, Show a, Bounded a) => A.Parser a
enumP =
  A.choice $
  map (\(e, str) -> e <$ A.string str) $
  sortOn (Down . B.length . snd) $
  map (\e -> (e, B.pack (show e))) [minBound .. maxBound]

enclosedP :: A.Parser a -> A.Parser b -> A.Parser c -> A.Parser c
enclosedP start end p = start *> p <* end
