{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module TrieOpt
  ( findOptimalPartitioning
  ) where

import Data.Bits (shiftL, shiftR)
import Data.Foldable (fold, foldl')
import Data.List (tails)
import Data.Maybe (fromJust)
import Data.Semigroup (Arg(Arg), Min(Min))
import qualified Data.Set as S
import Data.Traversable (mapAccumL)
import qualified Data.Vector as V

data Layer a =
  Layer
    { lowBits :: {-# UNPACK #-}!Int
    , classesCount :: {-# UNPACK #-}!Int
    , annotation :: a
    }

findOptimalPartitioning ::
     Ord a
  => ((Integer, Integer) -> Int)
  -> (Int -> Int)
  -> Int
  -> Int
  -> V.Vector a
  -> (Int, [Int])
findOptimalPartitioning rangeCost baseCost maxBits maxLayers xs =
  (\(Min (Arg cost layers)) -> (cost, reverse $ map lowBits layers)) $
  fromJust $
  foldMap
    (\layers ->
       Just $ Min $ Arg (totalCost baseCost (V.length xs) layers) layers) $
  subsequences maxLayers $ map (layerCost rangeCost) $ bitLayers maxBits xs

-- | Generate all subsequences with no more than the given number of
-- elements.
subsequences :: Int -> [a] -> [[a]]
subsequences 0 _ = [[]]
subsequences n xs =
  tails xs >>= \case
    [] -> [[]]
    (y:ys) -> map (y :) $ subsequences (n - 1) ys

totalCost :: (Int -> Int) -> Int -> [Layer Int] -> Int
totalCost = go 0 0
  where
    go :: Int -> Int -> (Int -> Int) -> Int -> [Layer Int] -> Int
    go !accum _ baseCost baseN [] = accum + baseCost baseN
    go accum baseBits baseCost baseN (l:ls) =
      go
        (accum + baseCost (classesCount l * (1 `shiftL` (lowBits l - baseBits))))
        (lowBits l)
        (* annotation l)
        (baseN `shiftR` (lowBits l - baseBits))
        ls

layerCost :: ((Integer, Integer) -> Int) -> Layer () -> Layer Int
layerCost rangeCost l =
  l {annotation = rangeCost (0, toInteger (classesCount l) - 1)}

bitLayers :: Ord a => Int -> V.Vector a -> [Layer ()]
bitLayers maxBits xs =
  reverse $ snd $ mapAccumL step xs [maxBits,maxBits - 1 .. 0]
  where
    step ys nbits =
      ( fold classes
      , Layer {lowBits = nbits, classesCount = S.size classes, annotation = ()})
      where
        classes = equivalenceClasses $ matricise (1 `shiftL` nbits) ys

matricise :: Int -> V.Vector a -> V.Vector (V.Vector a)
matricise rowSize xs
  | remainder /= 0 =
    error "matricise: row size does not evenly divide full size"
  | otherwise =
    V.generate columnSize $ \rowIdx -> V.slice (rowIdx * rowSize) rowSize xs
  where
    fullSize = V.length xs
    (columnSize, remainder) = fullSize `divMod` rowSize

equivalenceClasses :: (Foldable f, Ord a) => f a -> S.Set a
equivalenceClasses = foldl' (flip S.insert) S.empty
