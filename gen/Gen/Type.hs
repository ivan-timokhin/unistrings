{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Gen.Type
  ( EnumTy
  , IntegralType(..)
  , typeEnum
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Functor.Compose (Compose(Compose))
import Data.Int (Int16, Int32, Int8)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (Max(Max, getMax), Min(Min, getMin))
import qualified Data.Vector as V
import Data.Word (Word16, Word8)

import Gen.Cost (SizedTy(sizeInBytes))
import Trie (BottomAnnotation, LayerAnnotation, TrieDesc(Bottom, Layer))

data EnumTy

type instance BottomAnnotation EnumTy = IntegralType

type instance LayerAnnotation EnumTy = IntegralType

data IntegralType =
  IntegralType
    { itHaskell :: ByteString
    , itC :: ByteString
    , itMin :: Int
    , itMax :: Int
    , itSize :: Int
    }

instance SizedTy IntegralType where
  sizeInBytes = itSize

typeEnum :: (Foldable t, Enum a) => TrieDesc () t a -> TrieDesc EnumTy t a
typeEnum = typeG $ \xs -> findTypeForRange $ minMax 0 fromEnum (Compose xs)

typeG ::
     (Foldable t, LayerAnnotation annTy ~ IntegralType)
  => (forall f. Foldable f =>
                  f (V.Vector a) -> BottomAnnotation annTy)
  -> TrieDesc ann t a
  -> TrieDesc annTy t a
typeG f (Bottom _ xs) = Bottom (f xs) xs
typeG f (Layer _ nbits xs rest) =
  Layer (findTypeForRange $ minMax 0 id (Compose xs)) nbits xs (typeG f rest)

integralType ::
     Integral a => ByteString -> ByteString -> a -> a -> Int -> IntegralType
integralType h c mi ma = IntegralType h c (fromIntegral mi) (fromIntegral ma)

ffiIntegralTypes :: [IntegralType]
ffiIntegralTypes =
  [ integralType "Int8" "HsInt8" (minBound :: Int8) maxBound 1
  , integralType "Word8" "HsWord8" (minBound :: Word8) maxBound 1
  , integralType "Int16" "HsInt16" (minBound :: Int16) maxBound 2
  , integralType "Word16" "HsWord16" (minBound :: Word16) maxBound 2
  , integralType "Int32" "HsInt32" (minBound :: Int32) maxBound 4
  ]

findTypeForRange :: (Int, Int) -> IntegralType
findTypeForRange (lo, hi) =
  fromJust $ find (\ty -> itMin ty <= lo && itMax ty >= hi) ffiIntegralTypes

minMax :: (Foldable f, Ord b) => b -> (a -> b) -> f a -> (b, b)
minMax def f xs =
  (getMin $ fromMaybe (Min def) mMin, getMax $ fromMaybe (Max def) mMax)
  where
    (mMin, mMax) =
      foldMap
        (\x ->
           let y = f x
            in (Just (Min y), Just (Max y)))
        xs
