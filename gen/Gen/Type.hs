{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Gen.Type
  ( IntegralType(..)
  , FFIIntegralType(itypeOf)
  , typeEnum
  , typeMEnum
  , typeASCII
  , typeContainer
  , typeContainerDedup
  , typeLayers
  , int8
  , word8
  , int16
  , word16
  , int32
  ) where

import Control.Monad.Trans.State.Strict (evalState, get, put)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (fold, foldl')
import Data.Functor.Compose (Compose(Compose))
import Data.Int (Int16, Int32, Int8)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)

import Gen.Cost (SizedTy(sizeInBytes))
import qualified Gen.Mono as Mono
import Trie (TrieDesc(Bottom, Layer), deduplicate)

data IntegralType =
  IntegralType
    { itHaskell :: ByteString
    , itC :: ByteString
    , itMin :: Int
    , itMax :: Int
    , itSize :: Int
    }

instance SizedTy IntegralType where
  sizeInBytes = (*) . itSize

instance SizedTy (IntegralType, ByteString) where
  sizeInBytes (it, str) n = itSize it * n + B.length str

instance SizedTy (IntegralType, V.Vector Word8) where
  sizeInBytes (it, v) n = itSize it * n + V.length v

class Show a =>
      FFIIntegralType a
  where
  itypeOf :: IntegralType

instance FFIIntegralType Word8 where
  itypeOf = word8

typeEnum :: Enum a => V.Vector a -> IntegralType
typeEnum = findTypeForTable fromEnum

typeMEnum :: Enum a => V.Vector (Maybe a) -> IntegralType
typeMEnum = findTypeForTable $ maybe 0 (succ . fromEnum)

typeASCII :: V.Vector ByteString -> ((IntegralType, ByteString), V.Vector Int)
typeASCII = typeContainer

typeContainer ::
     Mono.Container c => V.Vector c -> ((IntegralType, c), V.Vector Int)
typeContainer cs = ((findTypeForTable id indices, collapsed), indices)
  where
    collapsed = fold cs
    indices =
      flip evalState 0 $
      for cs $ \c ->
        if Mono.isNull c
          then pure 0
          else do
            currentLen <- get
            put $! currentLen + Mono.len c
            pure currentLen

typeContainerDedup ::
     (Ord (t a), Traversable t, Monoid (t a), Mono.Container (t a))
  => V.Vector (t a)
  -> ((IntegralType, t a), V.Vector Int)
typeContainerDedup cs = ((findTypeForTable id indices, collapsed), csIndices)
  where
    (csDedup, values) = deduplicate cs
    collapsed = fold values
    indices =
      flip evalState 0 $
      for values $ \v ->
        if Mono.isNull v
          then pure 0
          else do
            currentLen <- get
            put $! currentLen + Mono.len v
            pure currentLen
    csIndices = V.backpermute indices csDedup

typeLayers ::
     Foldable f
  => TrieDesc f layerAnnotation bottomAnnotation a
  -> TrieDesc f IntegralType bottomAnnotation a
typeLayers (Bottom ann xs) = Bottom ann xs
typeLayers (Layer _ nbits xs rest) =
  Layer (findTypeForTable id (Compose xs)) nbits xs (typeLayers rest)

findTypeForTable :: Foldable f => (a -> Int) -> f a -> IntegralType
findTypeForTable f xs = findTypeForRange $ minMax 0 f xs

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
