module Main where

import Data.Foldable (for_)

import qualified UCD.UnicodeData
import Trie (mkTrie)

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      trie = mkTrie gcs [16, 12, 8]
  print trie

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
