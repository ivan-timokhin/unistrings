{-# LANGUAGE TypeFamilies #-}

module Gen.Mono where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Word (Word8)

class Monoid c =>
      Container c
  where
  type Elem c
  toList :: c -> [Elem c]
  isNull :: c -> Bool
  len :: c -> Int

instance Container B.ByteString where
  type Elem B.ByteString = Word8
  toList = B.unpack
  isNull = B.null
  len = B.length

instance Container (V.Vector a) where
  type Elem (V.Vector a) = a
  toList = V.toList
  isNull = V.null
  len = V.length
