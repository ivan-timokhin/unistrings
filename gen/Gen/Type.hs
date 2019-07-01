{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Gen.Type
  ( EnumTy
  , IntegralType(..)
  , typeEnum
  , IntTy
  , typeIntegral
  , int8
  , word8
  , int16
  , word16
  , int32
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Foldable (foldl')
import Data.Functor.Compose (Compose(Compose))
import Data.Int (Int16, Int32, Int8)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)

import Gen.Cost (SizedTy(sizeInBytes))
import Trie (BottomAnnotation, LayerAnnotation, TrieDesc(Bottom, Layer))

data EnumTy

type instance BottomAnnotation EnumTy = IntegralType

type instance LayerAnnotation EnumTy = IntegralType

data IntTy

type instance BottomAnnotation IntTy = IntegralType

type instance LayerAnnotation IntTy = IntegralType

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

typeEnum :: (Foldable t, Enum a) => TrieDesc ann t a -> TrieDesc EnumTy t a
typeEnum = typeG (findTypeForTable fromEnum)

typeIntegral ::
     (Foldable t, Integral a)
  => IntegralType
  -> TrieDesc ann t a
  -> TrieDesc IntTy t a
-- TODO: check that the stated integral type admits all values.
-- This will be caught later in the test suite anyway, but it may be
-- worthwhile to check here.
typeIntegral intTy = typeG (const intTy)

typeG ::
     (Foldable t, LayerAnnotation annTy ~ IntegralType)
  => (forall f. Foldable f =>
                  f (V.Vector a) -> BottomAnnotation annTy)
  -> TrieDesc ann t a
  -> TrieDesc annTy t a
typeG f (Bottom _ xs) = Bottom (f xs) xs
typeG f (Layer _ nbits xs rest) =
  Layer (findTypeForTable id xs) nbits xs (typeG f rest)

findTypeForTable ::
     (Foldable f1, Foldable f2) => (a -> Int) -> f1 (f2 a) -> IntegralType
findTypeForTable f xs = findTypeForRange $ minMax 0 f (Compose xs)

integralType ::
     Integral a => ByteString -> ByteString -> a -> a -> Int -> IntegralType
integralType h c mi ma = IntegralType h c (fromIntegral mi) (fromIntegral ma)

int8, word8, int16, word16, int32 :: IntegralType
int8 = integralType "Int8" "HsInt8" (minBound :: Int8) maxBound 1

word8 = integralType "Word8" "HsWord8" (minBound :: Word8) maxBound 1

int16 = integralType "Int16" "HsInt16" (minBound :: Int16) maxBound 2

word16 = integralType "Word16" "HsWord16" (minBound :: Word16) maxBound 2

int32 = integralType "Int32" "HsInt32" (minBound :: Int32) maxBound 4

ffiIntegralTypes :: [IntegralType]
ffiIntegralTypes = [int8, word8, int16, word16, int32]

findTypeForRange :: (Int, Int) -> IntegralType
findTypeForRange (lo, hi) =
  fromJust $ find (\ty -> itMin ty <= lo && itMax ty >= hi) ffiIntegralTypes

data Pair a b =
  Pair !a !b

pair2tuple :: Pair a b -> (a, b)
pair2tuple (Pair x y) = (x, y)

minMax :: (Foldable f, Ord b) => b -> (a -> b) -> f a -> (b, b)
minMax def f =
  pair2tuple .
  foldl'
    (\(Pair curMin curMax) x ->
       let b = f x
        in Pair (min curMin b) (max curMax b))
    (Pair def def)
