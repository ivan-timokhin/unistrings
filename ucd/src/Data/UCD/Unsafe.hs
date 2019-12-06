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
module Data.UCD.Unsafe
  ( CodePoint(CodePoint, getCodePoint)
  ) where

import Data.Coerce (coerce)
import Data.Ix (Ix)
import Data.Word (Word32)
import Numeric (showHex)

-- TODO: Read
newtype CodePoint =
  CodePoint
    { getCodePoint :: Word32
    }
  deriving (Eq, Ord, Ix)

instance Enum CodePoint where
  succ x
    | x == maxBound =
      errorWithoutStackTrace
        "Enum.succ{Data.UCD.CodePoint}: tried to take `succ' of maxBound"
    | otherwise = CodePoint (getCodePoint x + 1)
  pred x
    | x == minBound =
      errorWithoutStackTrace
        "Enum.pred{Data.UCD.CodePoint}: tried to take `pred' of minBound"
    | otherwise = CodePoint (getCodePoint x - 1)
  toEnum x
    | x < 0 || x > 0x10FFFF =
      errorWithoutStackTrace
        "Enum.toEnum{Data.UCD.CodePoint}: value out of bounds"
    | otherwise = CodePoint $ toEnum x
  fromEnum = fromEnum . getCodePoint
  enumFrom n = enumFromTo n maxBound
  enumFromThen n1 n2
    | n1 > n2 = enumFromThenTo n1 n2 minBound
    | otherwise = enumFromThenTo n1 n2 maxBound
  enumFromTo lo hi = coerce [getCodePoint lo .. getCodePoint hi]
  enumFromThenTo x1 x2 fin =
    coerce [getCodePoint x1,getCodePoint x2 .. getCodePoint fin]

instance Bounded CodePoint where
  minBound = CodePoint 0
  maxBound = CodePoint 0x10FFFF

instance Show CodePoint where
  showsPrec d (CodePoint c) =
    showParen (d > 10) $ showString "toEnum 0x" . showHex c
