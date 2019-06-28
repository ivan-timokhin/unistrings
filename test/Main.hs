module Main where

import Test.HUnit
import Data.Foldable (for_)
import System.Exit (exitFailure)
import Control.Monad (when)

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests = TestList [generalCategory]
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
      assertEqual ("#" ++ show (fromEnum cp)) refGC $ UCD.generalCategory cp

maxCp :: UCD.CodePoint
maxCp = maxBound

minCp :: UCD.CodePoint
minCp = minBound
