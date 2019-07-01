module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  , canonicalCombiningClass
  ) where

import Data.Char (GeneralCategory, ord)
import Data.Word (Word8)

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.GeneralCategory as GC

class IsCodePoint c where
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . toEnum . ord

generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = GC.retrieve . fromEnum . toCodePoint

canonicalCombiningClass :: IsCodePoint cp => cp -> Word8
canonicalCombiningClass = CCC.retrieve . fromEnum . toCodePoint
