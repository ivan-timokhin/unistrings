{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Data.Char (GeneralCategory(NotAssigned))
import Data.Foldable (for_)
import System.Directory (createDirectoryIfMissing)

import Driver (generateASCIITableSources, processTable)
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "generated/test_data"
  mapConcurrently_
    id
    [ processTable "general_category" $
      UCD.UnicodeData.tableToVector NotAssigned $
      fmap UCD.UnicodeData.propCategory records
    , processTable "canonical_combining_class" $
      UCD.UnicodeData.tableToVector 0 $
      fmap UCD.UnicodeData.propCanonicalCombiningClass records
    , generateASCIITableSources "name" $ UCD.UnicodeData.tableToNames records
    ]

printLong :: Show a => [a] -> IO ()
printLong entries
  | entriesCount <= 2 * magic = print entries
  | otherwise = do
    putStrLn "["
    for_ (take magic entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "\t⋮"
    for_ (drop (entriesCount - magic) entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "]"
  where
    entriesCount = length entries
    magic = 10
