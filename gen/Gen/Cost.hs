{-# LANGUAGE FlexibleContexts #-}

module Gen.Cost
  ( SizedTy(sizeInBytes)
  , totalCost
  , pickBest
  ) where

import Data.Foldable (foldl')
import Data.Semigroup (Arg(Arg), Min(Min))
import qualified Data.Vector as V

import Trie (BottomAnnotation, LayerAnnotation, TrieDesc(Bottom, Layer))

class SizedTy ty where
  sizeInBytes :: ty -> Int

totalCost ::
     (SizedTy (BottomAnnotation ann), SizedTy (LayerAnnotation ann), Foldable t)
  => TrieDesc ann t a
  -> Int
totalCost (Bottom ty xs) = sizeInBytes ty * nestedLength xs
totalCost (Layer ty _ ixs rest) =
  sizeInBytes ty * nestedLength ixs + totalCost rest

nestedLength :: Foldable t => t (V.Vector a) -> Int
nestedLength = foldl' (\n v -> n + V.length v) 0

pickBest ::
     ( SizedTy (BottomAnnotation ann)
     , SizedTy (LayerAnnotation ann)
     , Foldable f
     , Foldable t
     )
  => f (TrieDesc ann t a)
  -> Maybe (TrieDesc ann t a)
pickBest =
  fmap unarg . foldMap (\trie -> Just $ Min $ Arg (totalCost trie) trie)

unarg :: Min (Arg a b) -> b
unarg (Min (Arg _ b)) = b