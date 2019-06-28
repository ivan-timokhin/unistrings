{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State.Strict as S
import Data.Bits (shiftL)
import Data.Foldable (for_)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Tuple (swap)
import qualified Data.Vector as V

import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  let gcs = UCD.UnicodeData.generalCategoryVector records
      trie = mkTrie gcs [16, 12, 8]
  print trie

data TrieDesc t a
  = Bottom (t (V.Vector a))
  | Layer Int (t (V.Vector Int)) (TrieDesc V.Vector a)

deriving instance
         (Show (t (V.Vector Int)), Show (t (V.Vector a)), Show a) =>
         Show (TrieDesc t a)

mkTrie :: Ord a => V.Vector a -> [Int] -> TrieDesc Identity a
mkTrie xs [] = Bottom (Identity xs)
mkTrie xs (lowBits:rest) = split (go rest) lowBits (Identity xs)
  where
    go [] = Bottom
    go (lb:lbs) = split (go lbs) lb

split ::
     (Traversable t, Ord a)
  => (V.Vector (V.Vector a) -> TrieDesc V.Vector a)
  -> Int
  -> t (V.Vector a)
  -> TrieDesc t a
split recur lowBits xs = Layer lowBits indices $ recur compressed
  where
    (Compose indices, compressed) =
      deduplicate $ Compose $ fmap (matricise lowBits) xs

matricise :: Int -> V.Vector a -> V.Vector (V.Vector a)
matricise lowBits xs
  | remainder /= 0 =
    error "matricise: row size does not evenly divide full size"
  | otherwise =
    V.generate columnSize $ \rowIdx -> V.slice (rowIdx * rowSize) rowSize xs
  where
    fullSize = V.length xs
    rowSize = 1 `shiftL` lowBits
    (columnSize, remainder) = fullSize `divMod` rowSize

deduplicate :: (Traversable t, Ord a) => t a -> (t Int, V.Vector a)
deduplicate xs =
  second invertMap . flip S.runState M.empty . for xs $ \x -> do
    assocMap <- S.get
    let mapSize = M.size assocMap
        (index, newMap) =
          first (fromMaybe mapSize) $
          M.insertLookupWithKey (\_ _ old -> old) x mapSize assocMap
    S.put newMap
    pure index
  where
    invertMap :: M.Map a Int -> V.Vector a
    invertMap invMap =
      V.replicate n (error "deduplicate: element left unfilled") V.//
      map swap (M.assocs invMap)
      where
        n = M.size invMap

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
