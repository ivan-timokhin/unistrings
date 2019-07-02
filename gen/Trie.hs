{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Trie
  ( TrieDesc(Bottom, Layer)
  , LayerAnnotation
  , BottomAnnotation
  , mkTrie
  , mkTrieM
  , partitioning
  ) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State.Strict as S
import Data.Bits (shiftL)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Tuple (swap)
import qualified Data.Vector as V

import ListM (ListM(Cons, Nil), fromList)

type family LayerAnnotation ann

type family BottomAnnotation ann

type instance LayerAnnotation () = ()

type instance BottomAnnotation () = ()

data TrieDesc ann t a
  = Bottom (BottomAnnotation ann) (t (V.Vector a))
  | Layer (LayerAnnotation ann) Int (t (V.Vector Int)) (TrieDesc ann V.Vector a)

deriving instance
         (Show (t (V.Vector Int)), Show (t (V.Vector a)), Show a,
          Show (BottomAnnotation ann), Show (LayerAnnotation ann)) =>
         Show (TrieDesc ann t a)

partitioning :: TrieDesc ann f a -> [Int]
partitioning (Bottom _ _) = []
partitioning (Layer _ b _ rest) = b : partitioning rest

mkTrie :: Ord a => V.Vector a -> [Int] -> TrieDesc () Identity a
mkTrie xs bits = runIdentity $ mkTrieM xs (fromList bits)

mkTrieM ::
     (Ord a, Monad m) => V.Vector a -> ListM m Int -> m (TrieDesc () Identity a)
{-# INLINE mkTrieM #-}
mkTrieM xs Nil = pure $ Bottom () (Identity xs)
mkTrieM xs (Cons lowBits rest) = split (go rest) lowBits (Identity xs)
  where
    go mbits ys =
      mbits >>= \case
        Nil -> pure $ Bottom () ys
        Cons lb lbs -> split (go lbs) lb ys

split ::
     (Traversable t, Ord a, Functor f)
  => (V.Vector (V.Vector a) -> f (TrieDesc () V.Vector a))
  -> Int
  -> t (V.Vector a)
  -> f (TrieDesc () t a)
{-# INLINE split #-}
split recur lowBits xs = Layer () lowBits indices <$> recur compressed
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
