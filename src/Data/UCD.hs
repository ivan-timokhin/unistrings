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
  , whiteSpace
  , bidiControl
  , joinControl
  , dash
  , quotationMark
  , terminalPunctuation
  , hexDigit
  , asciiHexDigit
  , ideographic
  , diacritic
  , extender
  , noncharacterCodePoint
  , idsBinaryOperator
  , idsTrinaryOperator
  , radical
  , unifiedIdeograph
  , deprecated
  , softDotted
  , logicalOrderException
  , sentenceTerminal
  , variationSelector
  , patternWhiteSpace
  , patternSyntax
  , prependedConcatenationMark
  , regionalIndicator
  , math
  , alphabetic
  , uppercase
  , lowercase
  , cased
  , caseIgnorable
  , changesWhenLowercased
  , changesWhenUppercased
  , changesWhenTitlecased
  , changesWhenCasefolded
  , changesWhenCasemapped
  , idStart
  , idContinue
  , xidStart
  , xidContinue
  , defaultIgnorableCodePoint
  , graphemeExtend
  , graphemeBase
  , EnumeratedProperty(..)
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.Age as Age
import qualified Data.UCD.Internal.Alphabetic as A
import qualified Data.UCD.Internal.BidiControl as BC
import qualified Data.UCD.Internal.Blocks as Blocks
import Data.UCD.Internal.ByteString (mkByteString, renderUnicodeInt)
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.CaseIgnorable as CI
import qualified Data.UCD.Internal.Cased as Cs
import qualified Data.UCD.Internal.ChangesWhenCasefolded as CWCF
import qualified Data.UCD.Internal.ChangesWhenCasemapped as CWCM
import qualified Data.UCD.Internal.ChangesWhenLowercased as CWL
import qualified Data.UCD.Internal.ChangesWhenTitlecased as CWT
import qualified Data.UCD.Internal.ChangesWhenUppercased as CWU
import qualified Data.UCD.Internal.Dash as Da
import qualified Data.UCD.Internal.DefaultIgnorableCodePoint as DICP
import qualified Data.UCD.Internal.Deprecated as De
import qualified Data.UCD.Internal.Diacritic as Di
import qualified Data.UCD.Internal.Extender as Ext
import qualified Data.UCD.Internal.GeneralCategory as GC
import qualified Data.UCD.Internal.GraphemeBase as GB
import qualified Data.UCD.Internal.GraphemeExtend as GE
import qualified Data.UCD.Internal.IdContinue as IC
import qualified Data.UCD.Internal.IdStart as IS
import qualified Data.UCD.Internal.Ideographic as Ide
import qualified Data.UCD.Internal.JamoShortNameLen as JSNLen
import qualified Data.UCD.Internal.JamoShortNamePtr as JSNPtr
import qualified Data.UCD.Internal.LogicalOrderException as LOE
import qualified Data.UCD.Internal.Lowercase as LC
import qualified Data.UCD.Internal.Math as M
import qualified Data.UCD.Internal.NameAliasesAliasesLen as NAALen
import qualified Data.UCD.Internal.NameAliasesAliasesPtr as NAAPtr
import qualified Data.UCD.Internal.NameAliasesAliasesSublens as NAASublens
import qualified Data.UCD.Internal.NameAliasesTypes as NAT
import qualified Data.UCD.Internal.NameLen as NameLen
import qualified Data.UCD.Internal.NamePtr as NamePtr
import qualified Data.UCD.Internal.PatternSyntax as PS
import qualified Data.UCD.Internal.PrependedConcatenationMark as PCM
import Data.UCD.Internal.Ptr (unsafeReadPtr)
import qualified Data.UCD.Internal.QuotationMark as QM
import qualified Data.UCD.Internal.Script as Script
import qualified Data.UCD.Internal.ScriptExtsLen as SELen
import qualified Data.UCD.Internal.ScriptExtsPtr as SEPtr
import qualified Data.UCD.Internal.SentenceTerminal as ST
import qualified Data.UCD.Internal.SoftDotted as SD
import qualified Data.UCD.Internal.TerminalPunctuation as TP
import Data.UCD.Internal.Types
  ( Age(..)
  , Block(..)
  , EnumeratedProperty(..)
  , NameAliasType(..)
  , Script(..)
  )
import qualified Data.UCD.Internal.UnifiedIdeograph as UI
import qualified Data.UCD.Internal.Uppercase as UC
import qualified Data.UCD.Internal.XidContinue as XIC
import qualified Data.UCD.Internal.XidStart as XIS

class IsCodePoint c where
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . toEnum . ord

generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = withCP GC.retrieve

canonicalCombiningClass :: IsCodePoint cp => cp -> Word8
canonicalCombiningClass = withCP CCC.retrieve

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
block = withCP $ Blocks.retrieve . (`shiftR` 4)

age :: IsCodePoint cp => cp -> Maybe Age
age cp
  | iage == 0 = Nothing
  | otherwise = Just $ toEnum (iage - 1)
  where
    icp = fromEnum $ toCodePoint cp
    iage = Age.retrieve icp

script :: IsCodePoint cp => cp -> Script
script = withCP Script.retrieve

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

whiteSpace :: IsCodePoint cp => cp -> Bool
whiteSpace c
  | cp <= 0x00A0 =
    cp == 0x20 || (0x9 <= cp && cp <= 0xd) || cp == 0x85 || cp == 0xa0
  | cp >= 0x1680 && cp <= 0x3000 =
    (0x2000 <= cp && cp <= 0x200a) ||
    cp == 0x2028 ||
    cp == 0x2029 || cp == 0x202F || cp == 0x205F || cp == 0x1680 || cp == 0x3000
  | otherwise = False
  where
    CodePoint cp = toCodePoint c

bidiControl :: IsCodePoint cp => cp -> Bool
bidiControl = withCP BC.retrieve

joinControl :: IsCodePoint cp => cp -> Bool
joinControl c = cp >= CodePoint 0x200C && cp <= CodePoint 0x200D
  where
    cp = toCodePoint c

dash :: IsCodePoint cp => cp -> Bool
dash = withCP Da.retrieve

quotationMark :: IsCodePoint cp => cp -> Bool
quotationMark = withCP QM.retrieve

terminalPunctuation :: IsCodePoint cp => cp -> Bool
terminalPunctuation = withCP TP.retrieve

hexDigit :: IsCodePoint cp => cp -> Bool
hexDigit c
  | cp <= 0x0066 =
    cp >= 0x0061 || (0x30 <= cp && cp <= 0x39) || (0x41 <= cp && cp <= 0x46)
  | cp >= 0xff10 =
    cp <= 0xff19 ||
    (0xff21 <= cp && cp <= 0xff26) || (0xff41 <= cp && cp <= 0xff46)
  | otherwise = False
  where
    CodePoint cp = toCodePoint c

asciiHexDigit :: IsCodePoint cp => cp -> Bool
asciiHexDigit c =
  cp <= 0x66 &&
  cp >= 0x30 && (cp <= 0x39 || (0x41 <= cp && cp <= 0x46) || cp >= 0x61)
  where
    CodePoint cp = toCodePoint c

ideographic :: IsCodePoint cp => cp -> Bool
ideographic = withCP Ide.retrieve

diacritic :: IsCodePoint cp => cp -> Bool
diacritic = withCP Di.retrieve

extender :: IsCodePoint cp => cp -> Bool
extender = withCP Ext.retrieve

noncharacterCodePoint :: IsCodePoint cp => cp -> Bool
noncharacterCodePoint c =
  (0xfdd0 <= cp && cp <= 0xfdef) || (cp .&. 0xfffe == 0xfffe)
  where
    CodePoint cp = toCodePoint c

idsBinaryOperator :: IsCodePoint cp => cp -> Bool
idsBinaryOperator c =
  cp .&. 0xfffff0 == 0x2ff0 && (cp <= 0x2ff1 || (0x2ff4 <= cp && cp <= 0x2ffb))
  where
    CodePoint cp = toCodePoint c

idsTrinaryOperator :: IsCodePoint cp => cp -> Bool
idsTrinaryOperator c = 0x2ff2 <= cp && cp <= 0x2ff3
  where
    CodePoint cp = toCodePoint c

radical :: IsCodePoint cp => cp -> Bool
radical c =
  0x2e80 <= cp &&
  cp <= 0x2fd5 &&
  (cp <= 0x2e99 || (0x2e9b <= cp && cp <= 0x2ef3) || 0x2f00 <= cp)
  where
    CodePoint cp = toCodePoint c

unifiedIdeograph :: IsCodePoint cp => cp -> Bool
unifiedIdeograph = withCP UI.retrieve

deprecated :: IsCodePoint cp => cp -> Bool
deprecated = withCP De.retrieve

softDotted :: IsCodePoint cp => cp -> Bool
softDotted = withCP SD.retrieve

logicalOrderException :: IsCodePoint cp => cp -> Bool
logicalOrderException = withCP LOE.retrieve

sentenceTerminal :: IsCodePoint cp => cp -> Bool
sentenceTerminal = withCP ST.retrieve

variationSelector :: IsCodePoint cp => cp -> Bool
variationSelector c =
  cp >= 0x180b &&
  (cp <= 0x180d ||
   (cp >= 0xfe00 && (cp <= 0xfe0f || (cp >= 0xe0100 && cp <= 0xe01ef))))
  where
    CodePoint cp = toCodePoint c

patternWhiteSpace :: IsCodePoint cp => cp -> Bool
patternWhiteSpace c
  | cp <= 0x85 = (0x9 <= cp && cp <= 0xd) || cp == 0x20 || cp == 0x85
  | cp >= 0x200e = cp <= 0x200f || cp == 0x2028 || cp == 0x2029
  | otherwise = False
  where
    CodePoint cp = toCodePoint c

patternSyntax :: IsCodePoint cp => cp -> Bool
patternSyntax = withCP PS.retrieve

prependedConcatenationMark :: IsCodePoint cp => cp -> Bool
prependedConcatenationMark = withCP PCM.retrieve

regionalIndicator :: IsCodePoint cp => cp -> Bool
regionalIndicator c = 0x1f1e6 <= cp && cp <= 0x1f1ff
  where
    CodePoint cp = toCodePoint c

math :: IsCodePoint cp => cp -> Bool
math = withCP M.retrieve

alphabetic :: IsCodePoint cp => cp -> Bool
alphabetic c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = A.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

uppercase :: IsCodePoint cp => cp -> Bool
uppercase = withCP UC.retrieve

lowercase :: IsCodePoint cp => cp -> Bool
lowercase = withCP LC.retrieve

cased :: IsCodePoint cp => cp -> Bool
cased = withCP Cs.retrieve

caseIgnorable :: IsCodePoint cp => cp -> Bool
caseIgnorable = withCP CI.retrieve

changesWhenLowercased :: IsCodePoint cp => cp -> Bool
changesWhenLowercased = withCP CWL.retrieve

changesWhenUppercased :: IsCodePoint cp => cp -> Bool
changesWhenUppercased = withCP CWU.retrieve

changesWhenTitlecased :: IsCodePoint cp => cp -> Bool
changesWhenTitlecased = withCP CWT.retrieve

changesWhenCasefolded :: IsCodePoint cp => cp -> Bool
changesWhenCasefolded = withCP CWCF.retrieve

changesWhenCasemapped :: IsCodePoint cp => cp -> Bool
changesWhenCasemapped = withCP CWCM.retrieve

idStart :: IsCodePoint cp => cp -> Bool
idStart c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = IS.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

idContinue :: IsCodePoint cp => cp -> Bool
idContinue c
  | cp < 0xaa =
    cp >= 0x30 &&
    cp <= 0x7a &&
    (cp <= 0x39 || (cp >= 0x41 && (cp <= 0x5a || cp == 0x5f || cp >= 0x61)))
  | otherwise = IC.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

xidStart :: IsCodePoint cp => cp -> Bool
xidStart c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = XIS.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

xidContinue :: IsCodePoint cp => cp -> Bool
xidContinue c
  | cp < 0xaa =
    cp >= 0x30 &&
    cp <= 0x7a &&
    (cp <= 0x39 || (cp >= 0x41 && (cp <= 0x5a || cp == 0x5f || cp >= 0x61)))
  | otherwise = XIC.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

defaultIgnorableCodePoint :: IsCodePoint cp => cp -> Bool
defaultIgnorableCodePoint = withCP DICP.retrieve

graphemeExtend :: IsCodePoint cp => cp -> Bool
graphemeExtend = withCP GE.retrieve

graphemeBase :: IsCodePoint cp => cp -> Bool
graphemeBase = withCP GB.retrieve

withCP :: IsCodePoint cp => (Int -> a) -> cp -> a
withCP f = f . fromEnum . toCodePoint
