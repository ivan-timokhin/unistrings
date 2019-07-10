{-# LANGUAGE OverloadedStrings #-}

module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  , GeneralCategory(..)
  , canonicalCombiningClass
  , name
  , nameAliases
  ) where

import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)

import Data.UCD.Internal (CodePoint(CodePoint))
import Data.UCD.Internal.ByteString (mkByteString, renderUnicodeInt)
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.GeneralCategory as GC
import qualified Data.UCD.Internal.JamoShortNameLen as JSNLen
import qualified Data.UCD.Internal.JamoShortNamePtr as JSNPtr
import qualified Data.UCD.Internal.NameAliasesAliasesLen as NAALen
import qualified Data.UCD.Internal.NameAliasesAliasesPtr as NAAPtr
import qualified Data.UCD.Internal.NameAliasesAliasesSublens as NAASublens
import qualified Data.UCD.Internal.NameAliasesTypes as NAT
import qualified Data.UCD.Internal.NameLen as NameLen
import qualified Data.UCD.Internal.NamePtr as NamePtr
import Data.UCD.Internal.Ptr (unsafeReadPtr)
import Data.UCD.Internal.Types (NameAliasType)

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

name :: IsCodePoint cp => cp -> ByteString
name cp
  | 0xAC00 <= icp && icp <= 0xD7A3 = "HANGUL SYLLABLE " <> hangulSyllableSuffix
  | otherwise =
    case prefix of
      Just nameP -> nameP <> renderUnicodeInt icp
      Nothing -> mkByteString (NameLen.retrieve icp) (NamePtr.retrieve icp)
  where
    icp = fromEnum $ toCodePoint cp
    hangulSyllableSuffix
      | tindex > 0 = ljsn <> vjsn <> tjsn
      | otherwise = ljsn <> vjsn
      where
        ljsn = mkByteString (JSNLen.retrieve lindex) (JSNPtr.retrieve lindex)
        vjsn = mkByteString (JSNLen.retrieve vpart) (JSNPtr.retrieve vpart)
        tjsn = mkByteString (JSNLen.retrieve tpart) (JSNPtr.retrieve tpart)
        vpart = vbase + vindex
        tpart = tbase + tindex
        (lindex, vtindex) = sindex `divMod` ncount
        (vindex, tindex) = vtindex `divMod` tcount
        sindex = icp - sbase
        sbase = 0xAC00
        vbase = 0x61
        tbase = 0xA7
        vcount = 21
        tcount = 28
        ncount = vcount * tcount
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

nameAliases :: IsCodePoint cp => cp -> [(NameAliasType, ByteString)]
{-# INLINE nameAliases #-} -- List fusion
nameAliases cp =
  map
    (\i ->
       let offset = fromEnum $ unsafeReadPtr sublBase i
        in ( toEnum . fromEnum $ unsafeReadPtr tyBase i
           , mkByteString
               (fromEnum (unsafeReadPtr sublBase (i + 1)) - offset)
               (valBase `plusPtr` offset)))
    [0 .. (count - 1)]
  where
    valBase = NAAPtr.retrieve icp
    sublBase = NAASublens.retrieve icp
    tyBase = NAT.retrieve icp
    count = NAALen.retrieve icp
    icp = fromEnum $ toCodePoint cp
