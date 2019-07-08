{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Data.Char (GeneralCategory(NotAssigned))
import Data.Foldable (for_)
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)

import Driver (generateASCIITableSources, processTable)
import ListM (ListM(Nil), generatePartitionings)
import qualified UCD.Jamo
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "generated/test_data"
  let fullPartitionings = generatePartitionings 4 0 16
  mapConcurrently_
    id
    [ processTable fullPartitionings "general_category" $
      UCD.UnicodeData.tableToVector NotAssigned $
      fmap UCD.UnicodeData.propCategory records
    , processTable fullPartitionings "canonical_combining_class" $
      UCD.UnicodeData.tableToVector 0 $
      fmap UCD.UnicodeData.propCanonicalCombiningClass records
    , generateASCIITableSources fullPartitionings "name" $
      UCD.UnicodeData.tableToNames records V.//
      map
        (, "")
        ([0xAC00 .. 0xD7A3] ++
         [0x3400 .. 0x4DB5] ++
         [0x4E00 .. 0x9FEF] ++
         [0x20000 .. 0x2A6D6] ++
         [0x2A700 .. 0x2B734] ++
         [0x2B740 .. 0x2B81D] ++
         [0x2B820 .. 0x2CEA1] ++
         [0x2CEB0 .. 0x2EBE0] ++
         [0x17000 .. 0x187F7] ++
         [0xF900 .. 0xFA6D] ++ [0xFA70 .. 0xFAD9] ++ [0x2F800 .. 0x2FA1D])
    , do shortNames <- UCD.Jamo.fetch
         generateASCIITableSources [Nil] "jamo_short_name" shortNames
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
