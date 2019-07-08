{-# LANGUAGE OverloadedStrings #-}

module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  , GeneralCategory(..)
  , canonicalCombiningClass
  , name
  ) where

import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Word (Word8)

import Data.UCD.Internal (CodePoint(CodePoint))
import Data.UCD.Internal.ByteString (mkByteString, renderUnicodeInt)
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.GeneralCategory as GC
import qualified Data.UCD.Internal.NameLen as NameLen
import qualified Data.UCD.Internal.NamePtr as NamePtr

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

-- NB: This function is incorrect!  It does not currently handle
-- hangul syllables
name :: IsCodePoint cp => cp -> ByteString
name cp
  | 0xAC00 <= icp && icp <= 0xD7A3 = "Hangul syllable NYI"
  | otherwise =
    case prefix of
      Just nameP -> nameP <> renderUnicodeInt icp
      Nothing -> mkByteString (NameLen.retrieve icp) (NamePtr.retrieve icp)
  where
    icp = fromEnum $ toCodePoint cp
    prefix
      | (0x3400 <= icp && icp <= 0x4DB5) ||
          (0x4E00 <= icp && icp <= 0x9FEF) ||
          (0x20000 <= icp && icp <= 0x2A6D6) ||
          (0x2A700 <= icp && icp <= 0x2B734) ||
          (0x2B740 <= icp && icp <= 0x2B81D) ||
          (0x2B820 <= icp && icp <= 0x2CEA1) ||
          (0x2CEB0 <= icp && icp <= 0x2EBE0) = Just "CJK UNIFIED IDEOGRAPH-"
      | 0x17000 <= icp && icp <= 0x187F7 = Just "TANGUT IDEOGRAPH-"
      | (0xF900 <= icp && icp <= 0xFA6D) ||
          (0xFA70 <= icp && icp <= 0xFAD9) || (0x2F800 <= icp && icp <= 0x2FA1D) =
        Just "CJK COMPATIBILITY IDEOGRAPH-"
      | otherwise = Nothing
