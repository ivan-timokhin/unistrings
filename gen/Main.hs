{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode(WriteMode), hPrint, withFile)

import Gen
  ( EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , Module(moduleC, moduleHs)
  , generateEnum
  )
import Gen.Type (typeEnum)
import Trie (mkTrie)
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      trie = typeEnum $ mkTrie gcs [16, 8]
      md =
        generateEnum
          EnumSpec
            { esCPrefix = "general_category"
            , esHsType = "GeneralCategory"
            , esHsTypeModule = "Data.Char"
            }
          trie
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
