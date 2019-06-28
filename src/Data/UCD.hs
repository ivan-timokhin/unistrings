module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  ) where

import Data.Char (GeneralCategory, ord)

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.GeneralCategory as GC

class IsCodePoint c where
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . toEnum . ord

generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = GC.retrieve . fromEnum . toCodePoint
