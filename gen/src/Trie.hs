{-
Copyright 2019 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Trie
  ( TrieDesc(Bottom, Layer)
  , mkTrie
  , deduplicate
  ) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State.Strict as S
import Data.Bifunctor (Bifunctor(bimap))
import Data.Bits (shiftL)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

data TrieDesc t layerAnnotation bottomAnnotation a
  = Bottom bottomAnnotation (t (V.Vector a))
  | Layer
      layerAnnotation
      Int
      (t (V.Vector Int))
      (TrieDesc V.Vector layerAnnotation bottomAnnotation a)
  deriving (Functor, Foldable, Traversable)

instance Functor t => Bifunctor (TrieDesc t layerAnnotation) where
  bimap f g (Bottom ann xs) = Bottom (f ann) (fmap (fmap g) xs)
  bimap f g (Layer ann nbits is rest) = Layer ann nbits is (bimap f g rest)

deriving instance
         (Show (t (V.Vector Int)), Show (t (V.Vector a)), Show a,
          Show layerAnnotation, Show bottomAnnotation) =>
         Show (TrieDesc t layerAnnotation bottomAnnotation a)

mkTrie ::
     forall v a. (VG.Vector v a, Ord (v a))
  => v a
  -> [Int]
  -> TrieDesc Identity () () a
mkTrie xs [] = Bottom () (Identity $ VG.convert xs)
mkTrie xs (lowBits:rest) = split (go rest) lowBits (Identity xs)
  where
    go :: [Int] -> V.Vector (v a) -> TrieDesc V.Vector () () a
    go [] = Bottom () . V.map VG.convert
    go (lb:lbs) = split (go lbs) lb

split ::
     (Traversable t, Ord (v a), VG.Vector v a)
  => (V.Vector (v a) -> TrieDesc V.Vector () () a)
  -> Int
  -> t (v a)
  -> TrieDesc t () () a
{-# INLINE split #-}
split recur lowBits xs = Layer () lowBits indices $ recur compressed
  where
    (Compose indices, compressed) =
      deduplicate $ Compose $ fmap (matricise lowBits) xs

matricise :: VG.Vector v a => Int -> v a -> V.Vector (v a)
matricise lowBits xs
  | remainder /= 0 =
    error "matricise: row size does not evenly divide full size"
  | otherwise =
    V.generate columnSize $ \rowIdx -> VG.slice (rowIdx * rowSize) rowSize xs
  where
    fullSize = VG.length xs
    rowSize = 1 `shiftL` lowBits
    (columnSize, remainder) = fullSize `divMod` rowSize

deduplicate :: (Traversable t, Ord a) => t a -> (t Int, V.Vector a)
{-# INLINE deduplicate #-}
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
