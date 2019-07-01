{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Gen
  ( EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , IntSpec(IntSpec, isCPrefix, isHsType)
  , Module(moduleC, moduleHs)
  , generateEnum
  , generateIntegral
  )
import Gen.Cost (pickBest, totalCost)
import Gen.Type (typeEnum, typeIntegral, word8)
import ListM (generatePartitionings)
import Trie (mkTrieM, partitioning)
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      partitionings = generatePartitionings 4 0 16
      tries = map typeEnum $ partitionings >>= mkTrieM gcs
      trie = fromMaybe (error "Can't pick best trie") $ pickBest tries
      md =
        generateEnum
          EnumSpec
            { esCPrefix = "general_category"
            , esHsType = "GeneralCategory"
            , esHsTypeModule = "Data.Char"
            }
          trie
  print $ partitioning trie
  print $ totalCost trie
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  B.writeFile "generated/cbits/general_category.c" (B.unlines $ moduleC md)
  B.writeFile
    "generated/hs/Data/UCD/Internal/GeneralCategory.hs"
    (B.unlines $
     "{-# OPTIONS_GHC -Wno-unused-imports #-}" :
     "module Data.UCD.Internal.GeneralCategory (retrieve) where\n" : moduleHs md)
  createDirectoryIfMissing True "generated/test_data"
  withFile "generated/test_data/general_category.txt" WriteMode $ \h ->
    for_ gcs $ hPrint h
  let cccs =
        UCD.UnicodeData.tableToVector 0 $
        fmap UCD.UnicodeData.propCanonicalCombiningClass records
      cctries = map (typeIntegral word8) $ partitionings >>= mkTrieM cccs
      cctrie = fromMaybe (error "Can't pick best trie") $ pickBest cctries
      ccmd =
        generateIntegral
          IntSpec {isCPrefix = "canonical_combining_class", isHsType = "Word8"}
          cctrie
  print $ partitioning cctrie
  print $ totalCost cctrie
  B.writeFile
    "generated/hs/Data/UCD/Internal/CanonicalCombiningClass.hs"
    (B.unlines $
     "{-# OPTIONS_GHC -Wno-unused-imports #-}" :
     "module Data.UCD.Internal.CanonicalCombiningClass (retrieve) where\n" :
     moduleHs ccmd)
  B.writeFile
    "generated/cbits/canonical_combining_class.c"
    (B.unlines $ moduleC ccmd)
  withFile "generated/test_data/canonical_combining_class.txt" WriteMode $ \h ->
    for_ cccs $ hPrint h

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
