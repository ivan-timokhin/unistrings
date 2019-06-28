{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (for_)
import qualified Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)
import System.IO (withFile, IOMode(WriteMode), hPrint)

import qualified UCD.UnicodeData
import Trie (mkTrie)
import Gen
  ( EnumSpec(EnumSpec, esCPrefix, esHsType, esHsTypeModule)
  , Module(moduleC, moduleHs)
  , generateEnum
  )

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      trie = mkTrie gcs [16, 8]
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
