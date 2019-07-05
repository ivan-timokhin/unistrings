{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Gen.Type
  ( IntegralType(..)
  , typeEnum
  , typeIntegral
  , typeASCII
  , int8
  , word8
  , int16
  , word16
  , int32
  ) where

import Control.Monad.Trans.State.Strict (evalState, get, put)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl')
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Int (Int16, Int32, Int8)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)

import Gen.Cost (SizedTy(sizeInBytes))
import Trie (TrieDesc(Bottom, Layer))

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

typeEnum ::
     (Traversable t, Enum a)
  => TrieDesc la ba t a
  -> TrieDesc IntegralType IntegralType t a
typeEnum = typeG (findTypeForTable fromEnum)

typeIntegral ::
     (Traversable t, Integral a)
  => IntegralType
  -> TrieDesc la ba t a
  -> TrieDesc IntegralType IntegralType t a
-- TODO: check that the stated integral type admits all values.
-- This will be caught later in the test suite anyway, but it may be
-- worthwhile to check here.
typeIntegral intTy = typeG (const intTy)

typeASCII ::
     Traversable t
  => TrieDesc la ba t ByteString
  -> TrieDesc IntegralType (IntegralType, ByteString) t Int
typeASCII = typeGMod typeBottom
  where
    typeBottom ::
         Traversable f
      => f (V.Vector ByteString)
      -> ((IntegralType, ByteString), f (V.Vector Int))
    typeBottom strings = ((findTypeForTable id indices, collapsed), indices)
      where
        collapsed =
          toStrict $
          BB.toLazyByteString $ foldMap BB.byteString (Compose strings)
        indices =
          getCompose $
          flip evalState 0 $
          for (Compose strings) $ \string ->
            if B.null string
              then pure 0
              else do
                currentLen <- get
                put $! currentLen + B.length string
                pure currentLen

typeG ::
     (Traversable t)
  => (forall f. Traversable f =>
                  f (V.Vector a) -> bottomAnnotation)
  -> TrieDesc la ba t a
  -> TrieDesc IntegralType bottomAnnotation t a
typeG f = typeGMod (\xs -> (f xs, xs))

typeGMod ::
     (Traversable t)
  => (forall f. Traversable f =>
                  f (V.Vector a) -> (bottomAnnotation, f (V.Vector b)))
  -> TrieDesc la ba t a
  -> TrieDesc IntegralType bottomAnnotation t b
typeGMod f (Bottom _ xs) =
  let (ann, ys) = f xs
   in Bottom ann ys
typeGMod f (Layer _ nbits xs rest) =
  Layer (findTypeForTable id xs) nbits xs (typeGMod f rest)

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
