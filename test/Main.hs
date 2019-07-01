module Main where

import Control.Monad (when)
import Data.Foldable (for_)
import System.Exit (exitFailure)
import Test.HUnit

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests = TestList [generalCategory, canonicalCombiningClass]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

generalCategory :: Test
generalCategory =
  TestLabel "General category" $
  TestCase $ do
    reference <-
      map read . lines <$> readFile "generated/test_data/general_category.txt"
    assertEqual "Table size" (fromEnum maxCp - fromEnum minCp + 1) $
      length reference
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refGC) ->
      assertEqual (show cp) refGC $ UCD.generalCategory cp

canonicalCombiningClass :: Test
canonicalCombiningClass =
  TestLabel "Canonical combining class" $
  TestCase $ do
    reference <-
      map read . lines <$>
      readFile "generated/test_data/canonical_combining_class.txt"
    assertEqual "Table size" (fromEnum maxCp - fromEnum minCp + 1) $
      length reference
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refCCC) ->
      assertEqual (show cp) refCCC $ UCD.canonicalCombiningClass cp

maxCp :: UCD.CodePoint
maxCp = maxBound

minCp :: UCD.CodePoint
minCp = minBound
