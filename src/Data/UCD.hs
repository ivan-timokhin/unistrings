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
  , hangulSyllableType
  , HangulSyllableType(..)
  , simpleLowercaseMapping
  , simpleUppercaseMapping
  , simpleTitlecaseMapping
  , simpleCaseFolding
  , lowercaseMapping
  , uppercaseMapping
  , titlecaseMapping
  , caseFolding
  , CaseMapping(..)
  , numeric
  , Numeric(..)
  , decompositionType
  , DecompositionType(..)
  , canonicalDecomposition
  , compatibilityDecomposition
  , nontrivialCanonicalDecomposition
  , nontrivialCompatibilityDecomposition
  , canonicalComposition
  , canonicalCompositionStart
  , canonicalCompositionFinish
  , CompositionToken
  , nfdQuickCheck
  , nfcQuickCheck
  , nfkdQuickCheck
  , nfkcQuickCheck
  , nfkcCaseFold
  , NFKCCaseFold(ShortCF, LongCF)
  , changesWhenNFKCCasefolded
  , joiningType
  , JoiningType(..)
  , joiningGroup
  , JoiningGroup(..)
  , verticalOrientation
  , VerticalOrientation(..)
  , lineBreak
  , LineBreak(..)
  , graphemeClusterBreak
  , GraphemeClusterBreak(..)
  , sentenceBreak
  , SentenceBreak(..)
  , wordBreak
  , WordBreak(..)
  , eastAsianWidth
  , EastAsianWidth(..)
  , bidiClass
  , BidiClass(..)
  , bidiMirrored
  , bidiMirroringGlyph
  , bidiPairedBracket
  , bidiPairedBracketType
  , BidiPairedBracketType(..)
  , equivalentUnifiedIdeograph
  , unicode1Name
  , indicPositionalCategory
  , IndicPositionalCategory(..)
  , indicSyllabicCategory
  , IndicSyllabicCategory(..)
  , EnumeratedProperty(..)
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import GHC.Real (Ratio((:%)))

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.Age as Age
import qualified Data.UCD.Internal.Alphabetic as A
import qualified Data.UCD.Internal.BidiClass as BCl
import qualified Data.UCD.Internal.BidiControl as BC
import qualified Data.UCD.Internal.BidiMirrored as BM
import qualified Data.UCD.Internal.BidiMirroringGlyph as BMG
import qualified Data.UCD.Internal.BidiPairedBracket as BPB
import qualified Data.UCD.Internal.BidiPairedBracketType as BPBT
import qualified Data.UCD.Internal.Blocks as Blocks
import Data.UCD.Internal.ByteString (mkByteString, renderUnicodeInt)
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.CanonicalCompositionBottom as CCB
import qualified Data.UCD.Internal.CanonicalCompositionTop as CCT
import qualified Data.UCD.Internal.CanonicalDecompositionLen as CDLen
import qualified Data.UCD.Internal.CanonicalDecompositionPtr as CDPtr
import qualified Data.UCD.Internal.CaseIgnorable as CI
import qualified Data.UCD.Internal.Cased as Cs
import qualified Data.UCD.Internal.ChangesWhenCasefolded as CWCF
import qualified Data.UCD.Internal.ChangesWhenCasemapped as CWCM
import qualified Data.UCD.Internal.ChangesWhenLowercased as CWL
import qualified Data.UCD.Internal.ChangesWhenNfkcCasefolded as CWNC
import qualified Data.UCD.Internal.ChangesWhenTitlecased as CWT
import qualified Data.UCD.Internal.ChangesWhenUppercased as CWU
import qualified Data.UCD.Internal.CompatibilityDecompositionLen as KDLen
import qualified Data.UCD.Internal.CompatibilityDecompositionPtr as KDPtr
import qualified Data.UCD.Internal.ComplexNfkcCasefoldLen as CNFKCCFLen
import qualified Data.UCD.Internal.ComplexNfkcCasefoldPtr as CNFKCCFPtr
import qualified Data.UCD.Internal.Dash as Da
import qualified Data.UCD.Internal.DecompositionType as DT
import qualified Data.UCD.Internal.DefaultIgnorableCodePoint as DICP
import qualified Data.UCD.Internal.Deprecated as De
import qualified Data.UCD.Internal.Diacritic as Di
import qualified Data.UCD.Internal.EastAsianWidth as EAW
import qualified Data.UCD.Internal.EquivalentUnifiedIdeograph as EUI
import qualified Data.UCD.Internal.Extender as Ext
import qualified Data.UCD.Internal.FullCaseFolding0 as FCF0
import qualified Data.UCD.Internal.FullCaseFolding1 as FCF1
import qualified Data.UCD.Internal.FullCaseFolding2 as FCF2
import qualified Data.UCD.Internal.GeneralCategory as GC
import qualified Data.UCD.Internal.GraphemeBase as GB
import qualified Data.UCD.Internal.GraphemeClusterBreak as GCB
import qualified Data.UCD.Internal.GraphemeExtend as GE
import qualified Data.UCD.Internal.HangulSyllableType as HST
import qualified Data.UCD.Internal.IdContinue as IC
import qualified Data.UCD.Internal.IdStart as IS
import qualified Data.UCD.Internal.Ideographic as Ide
import qualified Data.UCD.Internal.IndicPositionalCategory as IPC
import qualified Data.UCD.Internal.IndicSyllabicCategory as ISC
import qualified Data.UCD.Internal.JamoShortNameLen as JSNLen
import qualified Data.UCD.Internal.JamoShortNamePtr as JSNPtr
import qualified Data.UCD.Internal.JoiningGroup as JG
import qualified Data.UCD.Internal.JoiningType as JT
import qualified Data.UCD.Internal.LineBreak as LB
import qualified Data.UCD.Internal.LogicalOrderException as LOE
import qualified Data.UCD.Internal.Lowercase as LC
import qualified Data.UCD.Internal.Math as M
import qualified Data.UCD.Internal.NameAliasesAliasesLen as NAALen
import qualified Data.UCD.Internal.NameAliasesAliasesPtr as NAAPtr
import qualified Data.UCD.Internal.NameAliasesAliasesSublens as NAASublens
import qualified Data.UCD.Internal.NameAliasesTypes as NAT
import qualified Data.UCD.Internal.NameLen as NameLen
import qualified Data.UCD.Internal.NamePtr as NamePtr
import qualified Data.UCD.Internal.NfcQuickCheck as NFCQC
import qualified Data.UCD.Internal.NfdQuickCheck as NFDQC
import qualified Data.UCD.Internal.NfkcQuickCheck as NFKCQC
import qualified Data.UCD.Internal.NfkdQuickCheck as NFKDQC
import qualified Data.UCD.Internal.NumericDenominator as ND
import qualified Data.UCD.Internal.NumericNumerator as NN
import qualified Data.UCD.Internal.NumericType as NT
import qualified Data.UCD.Internal.PatternSyntax as PS
import qualified Data.UCD.Internal.PrependedConcatenationMark as PCM
import Data.UCD.Internal.Ptr (unsafeReadPtr)
import qualified Data.UCD.Internal.QuotationMark as QM
import qualified Data.UCD.Internal.Script as Script
import qualified Data.UCD.Internal.ScriptExtsLen as SELen
import qualified Data.UCD.Internal.ScriptExtsPtr as SEPtr
import qualified Data.UCD.Internal.SentenceBreak as SB
import qualified Data.UCD.Internal.SentenceTerminal as ST
import qualified Data.UCD.Internal.SimpleCaseFolding as SCF
import qualified Data.UCD.Internal.SimpleLowercaseMapping as SLM
import qualified Data.UCD.Internal.SimpleNfkcCasefold as SNFKCCF
import qualified Data.UCD.Internal.SimpleTitlecaseMapping as STM
import qualified Data.UCD.Internal.SimpleUppercaseMapping as SUM
import qualified Data.UCD.Internal.SoftDotted as SD
import qualified Data.UCD.Internal.SpecialLowercaseMapping0 as SpLM0
import qualified Data.UCD.Internal.SpecialLowercaseMapping1 as SpLM1
import qualified Data.UCD.Internal.SpecialTitlecaseMapping0 as SpTM0
import qualified Data.UCD.Internal.SpecialTitlecaseMapping1 as SpTM1
import qualified Data.UCD.Internal.SpecialTitlecaseMapping2 as SpTM2
import qualified Data.UCD.Internal.SpecialUppercaseMapping0 as SpUM0
import qualified Data.UCD.Internal.SpecialUppercaseMapping1 as SpUM1
import qualified Data.UCD.Internal.SpecialUppercaseMapping2 as SpUM2
import qualified Data.UCD.Internal.TerminalPunctuation as TP
import Data.UCD.Internal.Types
  ( Age(..)
  , BidiClass(..)
  , BidiPairedBracketType(..)
  , Block(..)
  , DecompositionType(..)
  , EastAsianWidth(..)
  , EnumeratedProperty(..)
  , GraphemeClusterBreak(..)
  , HangulSyllableType(..)
  , IndicPositionalCategory(..)
  , IndicSyllabicCategory(..)
  , JoiningGroup(..)
  , JoiningType(..)
  , LineBreak(..)
  , NameAliasType(..)
  , Script(..)
  , SentenceBreak(..)
  , VerticalOrientation(..)
  , WordBreak(..)
  )
import qualified Data.UCD.Internal.Unicode1NameLen as U1NL
import qualified Data.UCD.Internal.Unicode1NamePtr as U1NP
import qualified Data.UCD.Internal.UnifiedIdeograph as UI
import qualified Data.UCD.Internal.Uppercase as UC
import qualified Data.UCD.Internal.VerticalOrientation as VO
import qualified Data.UCD.Internal.WordBreak as WB
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
        vbase = 0x61
        tbase = 0xA7
        (lindex, vindex, tindex) = splitHangulSyllable icp
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

block :: IsCodePoint cp => cp -> Maybe Block
block = withCP $ Blocks.retrieve . (`shiftR` 4)

age :: IsCodePoint cp => cp -> Maybe Age
age = withCP Age.retrieve

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

hangulSyllableType :: IsCodePoint cp => cp -> Maybe HangulSyllableType
hangulSyllableType = withCP HST.retrieve

simpleLowercaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleLowercaseMapping = simpleCaseMapping SLM.retrieve

simpleUppercaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleUppercaseMapping = simpleCaseMapping SUM.retrieve

simpleTitlecaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleTitlecaseMapping = simpleCaseMapping STM.retrieve

simpleCaseFolding :: IsCodePoint cp => cp -> CodePoint
simpleCaseFolding = simpleCaseMapping SCF.retrieve

simpleCaseMapping :: IsCodePoint cp => (Int -> Int) -> cp -> CodePoint
simpleCaseMapping f = withCP $ \cp -> CodePoint $ fromIntegral $ cp + f cp

lowercaseMapping :: IsCodePoint cp => cp -> CaseMapping
lowercaseMapping cp
  | first == 0 = SingleCM (simpleLowercaseMapping cp)
  | otherwise =
    DoubleCM (CodePoint first) $ CodePoint $ fromIntegral $ SpLM1.retrieve icp
  where
    icp = fromEnum $ toCodePoint cp
    first = fromIntegral $ SpLM0.retrieve icp

uppercaseMapping :: IsCodePoint cp => cp -> CaseMapping
uppercaseMapping cp
  | first == 0 = SingleCM (simpleUppercaseMapping cp)
  | third == 0 = DoubleCM (CodePoint first) (CodePoint second)
  | otherwise = TripleCM (CodePoint first) (CodePoint second) (CodePoint third)
  where
    icp = fromEnum $ toCodePoint cp
    first = fromIntegral $ SpUM0.retrieve icp
    second = fromIntegral $ SpUM1.retrieve icp
    third = fromIntegral $ SpUM2.retrieve icp

titlecaseMapping :: IsCodePoint cp => cp -> CaseMapping
titlecaseMapping cp
  | first == 0 = SingleCM (simpleTitlecaseMapping cp)
  | third == 0 = DoubleCM (CodePoint first) (CodePoint second)
  | otherwise = TripleCM (CodePoint first) (CodePoint second) (CodePoint third)
  where
    icp = fromEnum $ toCodePoint cp
    first = fromIntegral $ SpTM0.retrieve icp
    second = fromIntegral $ SpTM1.retrieve icp
    third = fromIntegral $ SpTM2.retrieve icp

caseFolding :: IsCodePoint cp => cp -> CaseMapping
caseFolding cp
  | first == 0 = SingleCM (simpleCaseFolding cp)
  | third == 0 = DoubleCM (CodePoint first) (CodePoint second)
  | otherwise = TripleCM (CodePoint first) (CodePoint second) (CodePoint third)
  where
    icp = fromEnum $ toCodePoint cp
    first = fromIntegral $ FCF0.retrieve icp
    second = fromIntegral $ FCF1.retrieve icp
    third = fromIntegral $ FCF2.retrieve icp

data CaseMapping
  = SingleCM {-# UNPACK #-}!CodePoint
  | DoubleCM {-# UNPACK #-}!CodePoint {-# UNPACK #-}!CodePoint
  | TripleCM
      {-# UNPACK #-}!CodePoint
      {-# UNPACK #-}!CodePoint
      {-# UNPACK #-}!CodePoint
  deriving (Show, Eq)

numeric :: IsCodePoint cp => cp -> Maybe Numeric
numeric =
  withCP $ \icp ->
    let ty = NT.retrieve icp
        numerator = NN.retrieve icp
        denominator = ND.retrieve icp
     in case ty of
          0 -> Nothing
          1 -> Just $ Decimal $ fromIntegral numerator
          2 -> Just $ Digit $ fromIntegral numerator
          _ -> Just $ Numeric $ numerator :% denominator

data Numeric
  = Decimal Word8
  | Digit Word8
  | Numeric (Ratio Int64)
  deriving (Show, Eq)

decompositionType :: IsCodePoint cp => cp -> Maybe DecompositionType
{-# INLINE decompositionType #-}
decompositionType =
  withCP $ \icp ->
    if 0xac00 <= icp && icp <= 0xd7a3
      then Just Canonical
      else DT.retrieve icp

nontrivialCanonicalDecomposition :: IsCodePoint cp => cp -> [CodePoint]
{-# INLINE nontrivialCanonicalDecomposition #-}
nontrivialCanonicalDecomposition =
  withCP $ \cp ->
    if 0xAC00 <= cp && cp <= 0xD7A3
      then hangulSyllableDecomposition cp
      else let ptr = CDPtr.retrieve cp
               len = CDLen.retrieve cp
            in map (CodePoint . fromIntegral . unsafeReadPtr ptr) [0 .. len - 1]

canonicalDecomposition :: IsCodePoint cp => cp -> [CodePoint]
canonicalDecomposition cp =
  case nontrivialCanonicalDecomposition cp of
    [] -> [toCodePoint cp]
    d -> d

nontrivialCompatibilityDecomposition :: IsCodePoint cp => cp -> [CodePoint]
{-# INLINE nontrivialCompatibilityDecomposition #-}
nontrivialCompatibilityDecomposition =
  withCP $ \cp ->
    if 0xAC00 <= cp && cp <= 0xD7A3
      then hangulSyllableDecomposition cp
      else let ptr = KDPtr.retrieve cp
               len = KDLen.retrieve cp
            in map (CodePoint . fromIntegral . unsafeReadPtr ptr) [0 .. len - 1]

compatibilityDecomposition :: IsCodePoint cp => cp -> [CodePoint]
compatibilityDecomposition cp =
  case nontrivialCompatibilityDecomposition cp of
    [] -> [toCodePoint cp]
    d -> d

hangulSyllableDecomposition :: Int -> [CodePoint]
{-# INLINE hangulSyllableDecomposition #-}
hangulSyllableDecomposition cp =
  let (lindex, vindex, tindex) = splitHangulSyllable cp
      lbase = 0x1100
      vbase = 0x1161
      tbase = 0x11a7
      lpart = lbase + lindex
      vpart = vbase + vindex
      tpart = tbase + tindex
   in if tindex > 0
        then [ CodePoint (fromIntegral lpart)
             , CodePoint (fromIntegral vpart)
             , CodePoint (fromIntegral tpart)
             ]
        else [CodePoint (fromIntegral lpart), CodePoint (fromIntegral vpart)]

splitHangulSyllable :: Int -> (Int, Int, Int)
{-# INLINE splitHangulSyllable #-}
splitHangulSyllable icp = (lindex, vindex, tindex)
  where
    (lindex, vtindex) = sindex `divMod` ncount
    (vindex, tindex) = vtindex `divMod` tcount
    sindex = icp - sbase
    sbase = 0xAC00
    vcount = 21
    tcount = 28
    ncount = vcount * tcount

canonicalComposition ::
     (IsCodePoint cp1, IsCodePoint cp2) => cp1 -> cp2 -> Maybe CodePoint
{-# INLINE canonicalComposition #-}
canonicalComposition cp1 cp2 =
  canonicalCompositionStart cp1 >>= flip canonicalCompositionFinish cp2

canonicalCompositionStart :: IsCodePoint cp => cp -> Maybe CompositionToken
{-# INLINE canonicalCompositionStart #-}
canonicalCompositionStart =
  withCP $ \icp ->
    if 0x1100 <= icp && icp <= 0x1112
      then Just $ HangulL (icp - 0x1100)
      else let offset = icp - 0xAC00
            in if 0 <= offset && offset <= 0x2B88 && offset `mod` 28 == 00
                 then Just $ HangulLV icp
                 else Generic . (* 0x110000) <$> CCT.retrieve icp

canonicalCompositionFinish ::
     IsCodePoint cp => CompositionToken -> cp -> Maybe CodePoint
{-# INLINE canonicalCompositionFinish #-}
canonicalCompositionFinish (Generic offset) =
  withCP $ \cp ->
    let result = CCB.retrieve (offset + cp)
     in if result == 0
          then Nothing
          else Just $ CodePoint result
canonicalCompositionFinish (HangulL lindex) =
  withCP $ \cp ->
    if 0x1161 <= cp && cp <= 0x1175
      then Just $
           CodePoint $ fromIntegral $ 0xAC00 + lindex * 588 + (cp - 0x1161) * 28
      else Nothing
canonicalCompositionFinish (HangulLV lvpart) =
  withCP $ \cp ->
    if 0x11A8 <= cp && cp <= 0x11C2
      then Just $ CodePoint $ fromIntegral $ lvpart + (cp - 0x11A7)
      else Nothing

data CompositionToken
  = Generic {-# UNPACK #-}!Int
  | HangulL {-# UNPACK #-}!Int
  | HangulLV {-# UNPACK #-}!Int

nfdQuickCheck :: IsCodePoint cp => cp -> Bool
nfdQuickCheck = withCP NFDQC.retrieve

nfcQuickCheck :: IsCodePoint cp => cp -> Maybe Bool
nfcQuickCheck = withCP NFCQC.retrieve

nfkdQuickCheck :: IsCodePoint cp => cp -> Bool
nfkdQuickCheck = withCP NFKDQC.retrieve

nfkcQuickCheck :: IsCodePoint cp => cp -> Maybe Bool
nfkcQuickCheck = withCP NFKCQC.retrieve

nfkcCaseFold :: IsCodePoint cp => cp -> NFKCCaseFold
{-# INLINE nfkcCaseFold #-}
nfkcCaseFold =
  withCP $ \cp ->
    case CNFKCCFLen.retrieve cp of
      1 -> ShortCF $ CodePoint $ fromIntegral $ cp + SNFKCCF.retrieve cp
      l ->
        LongCF $
        flip map [0 .. (l - 1)] $
        CodePoint . fromIntegral . unsafeReadPtr (CNFKCCFPtr.retrieve cp)

data NFKCCaseFold
  = ShortCF {-# UNPACK #-}!CodePoint
  | LongCF [CodePoint]
  deriving (Show, Eq)

changesWhenNFKCCasefolded :: IsCodePoint cp => cp -> Bool
changesWhenNFKCCasefolded = withCP CWNC.retrieve

joiningType :: IsCodePoint cp => cp -> JoiningType
joiningType = withCP JT.retrieve

joiningGroup :: IsCodePoint cp => cp -> Maybe JoiningGroup
joiningGroup = withCP JG.retrieve

verticalOrientation :: IsCodePoint cp => cp -> VerticalOrientation
verticalOrientation = withCP VO.retrieve

lineBreak :: IsCodePoint cp => cp -> LineBreak
lineBreak = withCP LB.retrieve

graphemeClusterBreak :: IsCodePoint cp => cp -> GraphemeClusterBreak
graphemeClusterBreak = withCP GCB.retrieve

sentenceBreak :: IsCodePoint cp => cp -> SentenceBreak
sentenceBreak = withCP SB.retrieve

wordBreak :: IsCodePoint cp => cp -> WordBreak
wordBreak = withCP WB.retrieve

eastAsianWidth :: IsCodePoint cp => cp -> EastAsianWidth
eastAsianWidth = withCP EAW.retrieve

bidiClass :: IsCodePoint cp => cp -> BidiClass
bidiClass = withCP BCl.retrieve

bidiMirrored :: IsCodePoint cp => cp -> Bool
bidiMirrored = withCP BM.retrieve

bidiMirroringGlyph :: IsCodePoint cp => cp -> Maybe CodePoint
bidiMirroringGlyph =
  withCP $ \cp ->
    let diff = BMG.retrieve cp
     in if diff == 0
          then Nothing
          else Just $ CodePoint $ fromIntegral $ cp + diff

bidiPairedBracket :: IsCodePoint cp => cp -> CodePoint
bidiPairedBracket =
  withCP $ \cp ->
    let diff = BPB.retrieve cp
     in CodePoint $ fromIntegral $ cp + diff

bidiPairedBracketType :: IsCodePoint cp => cp -> Maybe BidiPairedBracketType
bidiPairedBracketType = withCP $ \cp -> BPBT.retrieve cp

equivalentUnifiedIdeograph :: IsCodePoint cp => cp -> Maybe CodePoint
equivalentUnifiedIdeograph =
  withCP $ fmap (CodePoint . fromIntegral) . EUI.retrieve

unicode1Name :: IsCodePoint cp => cp -> ByteString
unicode1Name =
  withCP $ \cp -> mkByteString (U1NL.retrieve cp) (U1NP.retrieve cp)

indicPositionalCategory :: IsCodePoint cp => cp -> Maybe IndicPositionalCategory
indicPositionalCategory = withCP IPC.retrieve

indicSyllabicCategory :: IsCodePoint cp => cp -> IndicSyllabicCategory
indicSyllabicCategory = withCP ISC.retrieve

withCP :: IsCodePoint cp => (Int -> a) -> cp -> a
withCP f = f . fromEnum . toCodePoint
