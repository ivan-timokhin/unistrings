{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (for_)
import qualified Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)

import qualified UCD.UnicodeData
import Trie (mkTrie)
import Gen (Module(moduleC), generateEnum)

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      trie = mkTrie gcs [16, 12, 8]
      md = generateEnum "general_category" trie
  createDirectoryIfMissing True "generated/cbits"
  B.writeFile "generated/cbits/general_category.c" (B.unlines $ moduleC md)

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
