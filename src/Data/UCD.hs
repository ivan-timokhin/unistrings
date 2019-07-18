{-# LANGUAGE OverloadedStrings #-}

module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  , GeneralCategory(..)
  , canonicalCombiningClass
  , name
  , nameAliases
  , NameAliasType(..)
  , block
  , Block(..)
  , age
  , Age(..)
  , script
  , Script(..)
  , scriptExtensions
  , scriptExtensionsRaw
  , EnumeratedProperty(..)
  ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.Age as Age
import qualified Data.UCD.Internal.Blocks as Blocks
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
import qualified Data.UCD.Internal.Script as Script
import qualified Data.UCD.Internal.ScriptExtsLen as SELen
import qualified Data.UCD.Internal.ScriptExtsPtr as SEPtr
import Data.UCD.Internal.Types
  ( Age(..)
  , Block(..)
  , EnumeratedProperty(..)
  , NameAliasType(..)
  , Script(..)
  )

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

block :: IsCodePoint cp => cp -> Block
block = Blocks.retrieve . (`shiftR` 4) . fromEnum . toCodePoint

age :: IsCodePoint cp => cp -> Maybe Age
age cp
  | iage == 0 = Nothing
  | otherwise = Just $ toEnum (iage - 1)
  where
    icp = fromEnum $ toCodePoint cp
    iage = Age.retrieve icp

script :: IsCodePoint cp => cp -> Script
script = Script.retrieve . fromEnum . toCodePoint

scriptExtensions :: IsCodePoint cp => cp -> [Script]
scriptExtensions cp =
  if count == 0
    then [script cp]
    else scriptExtensionsRaw cp
  where
    count = SELen.retrieve icp
    icp = fromEnum $ toCodePoint cp

scriptExtensionsRaw :: IsCodePoint cp => cp -> [Script]
{-# INLINE scriptExtensionsRaw #-} -- List fusion
scriptExtensionsRaw cp =
  map (toEnum . fromEnum . unsafeReadPtr ptr) [0 .. count - 1]
  where
    ptr = SEPtr.retrieve icp
    count = SELen.retrieve icp
    icp = fromEnum $ toCodePoint cp
