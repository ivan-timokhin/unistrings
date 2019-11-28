{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module TrieOpt
  ( findOptimalPartitioning
  ) where

import Data.Bits (shiftL, shiftR)
import Data.Foldable (foldl')
import Data.List (tails)
import Data.Maybe (fromJust)
import Data.Semigroup (Arg(Arg), Min(Min))
import qualified Data.Set as S
import Data.Traversable (mapAccumL)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

data Layer a =
  Layer
    { lowBits :: {-# UNPACK #-}!Int
    , classesCount :: {-# UNPACK #-}!Int
    , annotation :: a
    }

findOptimalPartitioning ::
     (Ord (v a), VG.Vector v a)
  => ((Integer, Integer) -> Int)
  -> (Int -> Int)
  -> Int
  -> Int
  -> v a
  -> (Int, [Int])
findOptimalPartitioning rangeCost baseCost maxBits maxLayers xs =
  (\(Min (Arg cost layers)) -> (cost, reverse $ map lowBits layers)) $
  fromJust $
  foldMap
    (\layers ->
       Just $ Min $ Arg (totalCost baseCost (VG.length xs) layers) layers) $
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

bitLayers :: (Ord (v a), VG.Vector v a) => Int -> v a -> [Layer ()]
bitLayers maxBits xs =
  reverse $ snd $ mapAccumL step xs [maxBits,maxBits - 1 .. 0]
  where
    step ys nbits =
      ( VG.concat $ S.toList classes
      , Layer {lowBits = nbits, classesCount = S.size classes, annotation = ()})
      where
        classes = equivalenceClasses $ matricise (1 `shiftL` nbits) ys

matricise :: VG.Vector v a => Int -> v a -> V.Vector (v a)
matricise rowSize xs
  | remainder /= 0 =
    error "matricise: row size does not evenly divide full size"
  | otherwise =
    V.generate columnSize $ \rowIdx -> VG.slice (rowIdx * rowSize) rowSize xs
  where
    fullSize = VG.length xs
    (columnSize, remainder) = fullSize `divMod` rowSize

equivalenceClasses :: (Foldable f, Ord a) => f a -> S.Set a
equivalenceClasses = foldl' (flip S.insert) S.empty
