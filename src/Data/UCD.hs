module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  ) where

import Data.Char (GeneralCategory, ord)
import Data.Coerce (coerce)
import Data.Word (Word32)

import qualified Data.UCD.Internal.GeneralCategory as GC

-- TODO: Move the declaration into internal module, and export
-- constructor from there.
-- TODO: Read, Show, Ix
newtype CodePoint =
  CodePoint
    { getCodePoint :: Word32
    }
  deriving (Eq, Ord)

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
    | otherwise = CodePoint $ fromIntegral x
  fromEnum = fromIntegral . getCodePoint
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

class IsCodePoint c where
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . fromIntegral . ord

generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = GC.retrieve . fromIntegral . getCodePoint . toCodePoint
