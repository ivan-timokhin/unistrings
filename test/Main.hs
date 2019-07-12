{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))
import System.Exit (exitFailure)
import Test.HUnit

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests = TestList [generalCategory, canonicalCombiningClass, nameAliases]
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
