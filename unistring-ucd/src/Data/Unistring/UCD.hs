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
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Unistring.UCD
Description : Main properties defined in the Unicode Character Database
Copyright   : (c) Ivan Timokhin 2019
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental

Functions in this module provide access to the main properties defined
in the Unicode Character Database.  Specifically, they cover
properties listed the property table
in [UAX #44 ‘Unicode Character Database’](https://www.unicode.org/reports/tr44/),
except for

* obsolete and deprecated properties,
* contributory properties that are taken into account by higher-lever
  functions,
* Unicode_Radical_Stroke property, for reasons explained below.

The following, while formally part of the Unicode Character Database,
are nevertheless /not/ covered by this library:

 * Unihan database, defined in [UAX #38 ‘Unicode Han Database’](https://www.unicode.org/reports/tr38/)
   as ‘the repository of Unicode Consortium's collective knowledge
   regarding the CJK Unified Ideographs contained in the Unicode
   Standard.’  Importantly, this includes the kRSUnicode property, for
   which the aforementioned Unicode_Radical_Stroke is an alias.
 * Provenance data: this includes files such as ‘NushuSources,’
   ‘TangutSources,’ and similar.
 * Named sequences
 * Standardised variants: variants of existing code points' rendering
   that can be selected using variation selectors.

= Property types

All of the properties exposed here belong to one of two types:

  [Normative properties]: used in the Unicode Standard itself, and
  [Informative properties]: provided for information only.

= The use of 'Maybe' in property values

Most of the UCD properties have to include some catch-all value to be
used for code points to which the property doesn't really apply; if
for no other reason, then because most of the Unicode codespace is
still unassigned.  For non-enumerated properties, such value usually
arises naturally; e.g. boolean properties evaluate to 'False' for
unassigned code points.  For enumerated properties, however, it is
usually listed in the UCD explicitly along all others.

The decision of how to represent that value is generally made on a
case-by-case basis. As a general guideline, if that value is named
some variant of ‘None,’ we wrap the property value in 'Maybe' and
represent the catch-all value as 'Nothing'; otherwise, the value is
added to the Haskell enumeration type as well.

= Database version

This module provides property values according to __Unicode 12.1__

@since 0.1.0.0
-}
module Data.Unistring.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  -- * General
  , name
  , nameAliases
  , NameAliasType(..)
  , block
  , Block(..)
  , age
  , Age(..)
  , generalCategory
  , GeneralCategory(..)
  , script
  , Script(..)
  , scriptExtensions
  , scriptExtensionsRaw
  , whiteSpace
  , alphabetic
  , hangulSyllableType
  , HangulSyllableType(..)
  , noncharacterCodePoint
  , defaultIgnorableCodePoint
  , deprecated
  , logicalOrderException
  , variationSelector
  -- * Case
  , uppercase
  , lowercase
  , lowercaseMapping
  , uppercaseMapping
  , titlecaseMapping
  , caseFolding
  , CaseMapping(..)
  , simpleLowercaseMapping
  , simpleUppercaseMapping
  , simpleTitlecaseMapping
  , simpleCaseFolding
  , softDotted
  , cased
  , caseIgnorable
  , changesWhenLowercased
  , changesWhenUppercased
  , changesWhenTitlecased
  , changesWhenCasefolded
  , changesWhenCasemapped
  -- * Numeric
  , numeric
  , Numeric(..)
  , hexDigit
  , asciiHexDigit
  -- * Normalisation
  , canonicalCombiningClass
  , canonicalDecomposition
  , compatibilityDecomposition
  , nontrivialCanonicalDecomposition
  , nontrivialCompatibilityDecomposition
  , canonicalComposition
  , canonicalCompositionStart
  , canonicalCompositionFinish
  , CompositionToken
  , decompositionType
  , DecompositionType(..)
  , nfdQuickCheck
  , nfcQuickCheck
  , nfkdQuickCheck
  , nfkcQuickCheck
  , nfkcCaseFold
  , NFKCCaseFold(ShortCF, LongCF)
  , changesWhenNFKCCasefolded
  -- * Shaping and Rendering
  , joinControl
  , joiningGroup
  , JoiningGroup(..)
  , joiningType
  , JoiningType(..)
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
  , prependedConcatenationMark
  -- * Bidirectional
  , bidiClass
  , BidiClass(..)
  , bidiControl
  , bidiMirrored
  , bidiMirroringGlyph
  , bidiPairedBracket
  , bidiPairedBracketType
  , BidiPairedBracketType(..)
  -- * Identifiers
  , idContinue
  , idStart
  , xidContinue
  , xidStart
  , patternSyntax
  , patternWhiteSpace
  -- * CJK
  , ideographic
  , unifiedIdeograph
  , radical
  , idsBinaryOperator
  , idsTrinaryOperator
  , equivalentUnifiedIdeograph
  -- * Miscellaneous
  , math
  , quotationMark
  , dash
  , sentenceTerminal
  , terminalPunctuation
  , diacritic
  , extender
  , graphemeBase
  , graphemeExtend
  , unicode1Name
  , regionalIndicator
  , indicPositionalCategory
  , IndicPositionalCategory(..)
  , indicSyllabicCategory
  , IndicSyllabicCategory(..)
  -- * Enumerated properties support
  , EnumeratedProperty(..)
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), ord)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import GHC.Real (Ratio((:%)))

import qualified Data.Unistring.UCD.Internal.Age as Age
import qualified Data.Unistring.UCD.Internal.Alphabetic as A
import qualified Data.Unistring.UCD.Internal.BidiClass as BCl
import qualified Data.Unistring.UCD.Internal.BidiControl as BC
import qualified Data.Unistring.UCD.Internal.BidiMirrored as BM
import qualified Data.Unistring.UCD.Internal.BidiMirroringGlyph as BMG
import qualified Data.Unistring.UCD.Internal.BidiPairedBracket as BPB
import qualified Data.Unistring.UCD.Internal.BidiPairedBracketType as BPBT
import qualified Data.Unistring.UCD.Internal.Blocks as Blocks
import Data.Unistring.UCD.Internal.ByteString (mkByteString, renderUnicodeInt)
import qualified Data.Unistring.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.Unistring.UCD.Internal.CanonicalCompositionBottom as CCB
import qualified Data.Unistring.UCD.Internal.CanonicalCompositionTop as CCT
import qualified Data.Unistring.UCD.Internal.CanonicalDecompositionLen as CDLen
import qualified Data.Unistring.UCD.Internal.CanonicalDecompositionPtr as CDPtr
import qualified Data.Unistring.UCD.Internal.CaseIgnorable as CI
import qualified Data.Unistring.UCD.Internal.Cased as Cs
import qualified Data.Unistring.UCD.Internal.ChangesWhenCasefolded as CWCF
import qualified Data.Unistring.UCD.Internal.ChangesWhenCasemapped as CWCM
import qualified Data.Unistring.UCD.Internal.ChangesWhenLowercased as CWL
import qualified Data.Unistring.UCD.Internal.ChangesWhenNfkcCasefolded as CWNC
import qualified Data.Unistring.UCD.Internal.ChangesWhenTitlecased as CWT
import qualified Data.Unistring.UCD.Internal.ChangesWhenUppercased as CWU
import qualified Data.Unistring.UCD.Internal.CompatibilityDecompositionLen as KDLen
import qualified Data.Unistring.UCD.Internal.CompatibilityDecompositionPtr as KDPtr
import qualified Data.Unistring.UCD.Internal.ComplexNfkcCasefoldLen as CNFKCCFLen
import qualified Data.Unistring.UCD.Internal.ComplexNfkcCasefoldPtr as CNFKCCFPtr
import qualified Data.Unistring.UCD.Internal.Dash as Da
import qualified Data.Unistring.UCD.Internal.DecompositionType as DT
import qualified Data.Unistring.UCD.Internal.DefaultIgnorableCodePoint as DICP
import qualified Data.Unistring.UCD.Internal.Deprecated as De
import qualified Data.Unistring.UCD.Internal.Diacritic as Di
import qualified Data.Unistring.UCD.Internal.EastAsianWidth as EAW
import qualified Data.Unistring.UCD.Internal.EquivalentUnifiedIdeograph as EUI
import qualified Data.Unistring.UCD.Internal.Extender as Ext
import qualified Data.Unistring.UCD.Internal.FullCaseFolding0 as FCF0
import qualified Data.Unistring.UCD.Internal.FullCaseFolding1 as FCF1
import qualified Data.Unistring.UCD.Internal.FullCaseFolding2 as FCF2
import qualified Data.Unistring.UCD.Internal.GeneralCategory as GC
import qualified Data.Unistring.UCD.Internal.GraphemeBase as GB
import qualified Data.Unistring.UCD.Internal.GraphemeClusterBreak as GCB
import qualified Data.Unistring.UCD.Internal.GraphemeExtend as GE
import qualified Data.Unistring.UCD.Internal.HangulSyllableType as HST
import qualified Data.Unistring.UCD.Internal.IdContinue as IC
import qualified Data.Unistring.UCD.Internal.IdStart as IS
import qualified Data.Unistring.UCD.Internal.Ideographic as Ide
import qualified Data.Unistring.UCD.Internal.IndicPositionalCategory as IPC
import qualified Data.Unistring.UCD.Internal.IndicSyllabicCategory as ISC
import qualified Data.Unistring.UCD.Internal.JamoShortNameLen as JSNLen
import qualified Data.Unistring.UCD.Internal.JamoShortNamePtr as JSNPtr
import qualified Data.Unistring.UCD.Internal.JoiningGroup as JG
import qualified Data.Unistring.UCD.Internal.JoiningType as JT
import qualified Data.Unistring.UCD.Internal.LineBreak as LB
import qualified Data.Unistring.UCD.Internal.LogicalOrderException as LOE
import qualified Data.Unistring.UCD.Internal.Lowercase as LC
import qualified Data.Unistring.UCD.Internal.Math as M
import qualified Data.Unistring.UCD.Internal.NameAliasesAliasesLen as NAALen
import qualified Data.Unistring.UCD.Internal.NameAliasesAliasesPtr as NAAPtr
import qualified Data.Unistring.UCD.Internal.NameAliasesAliasesSublens as NAASublens
import qualified Data.Unistring.UCD.Internal.NameAliasesTypes as NAT
import qualified Data.Unistring.UCD.Internal.NameLen as NameLen
import qualified Data.Unistring.UCD.Internal.NamePtr as NamePtr
import qualified Data.Unistring.UCD.Internal.NfcQuickCheck as NFCQC
import qualified Data.Unistring.UCD.Internal.NfdQuickCheck as NFDQC
import qualified Data.Unistring.UCD.Internal.NfkcQuickCheck as NFKCQC
import qualified Data.Unistring.UCD.Internal.NfkdQuickCheck as NFKDQC
import qualified Data.Unistring.UCD.Internal.NumericDenominator as ND
import qualified Data.Unistring.UCD.Internal.NumericNumerator as NN
import qualified Data.Unistring.UCD.Internal.NumericType as NT
import qualified Data.Unistring.UCD.Internal.PatternSyntax as PS
import qualified Data.Unistring.UCD.Internal.PrependedConcatenationMark as PCM
import Data.Unistring.UCD.Internal.Ptr (unsafeReadPtr)
import qualified Data.Unistring.UCD.Internal.QuotationMark as QM
import qualified Data.Unistring.UCD.Internal.Script as Script
import qualified Data.Unistring.UCD.Internal.ScriptExtsLen as SELen
import qualified Data.Unistring.UCD.Internal.ScriptExtsPtr as SEPtr
import qualified Data.Unistring.UCD.Internal.SentenceBreak as SB
import qualified Data.Unistring.UCD.Internal.SentenceTerminal as ST
import qualified Data.Unistring.UCD.Internal.SimpleCaseFolding as SCF
import qualified Data.Unistring.UCD.Internal.SimpleLowercaseMapping as SLM
import qualified Data.Unistring.UCD.Internal.SimpleNfkcCasefold as SNFKCCF
import qualified Data.Unistring.UCD.Internal.SimpleTitlecaseMapping as STM
import qualified Data.Unistring.UCD.Internal.SimpleUppercaseMapping as SUM
import qualified Data.Unistring.UCD.Internal.SoftDotted as SD
import qualified Data.Unistring.UCD.Internal.SpecialLowercaseMapping0 as SpLM0
import qualified Data.Unistring.UCD.Internal.SpecialLowercaseMapping1 as SpLM1
import qualified Data.Unistring.UCD.Internal.SpecialTitlecaseMapping0 as SpTM0
import qualified Data.Unistring.UCD.Internal.SpecialTitlecaseMapping1 as SpTM1
import qualified Data.Unistring.UCD.Internal.SpecialTitlecaseMapping2 as SpTM2
import qualified Data.Unistring.UCD.Internal.SpecialUppercaseMapping0 as SpUM0
import qualified Data.Unistring.UCD.Internal.SpecialUppercaseMapping1 as SpUM1
import qualified Data.Unistring.UCD.Internal.SpecialUppercaseMapping2 as SpUM2
import qualified Data.Unistring.UCD.Internal.TerminalPunctuation as TP
import Data.Unistring.UCD.Internal.Types
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
import qualified Data.Unistring.UCD.Internal.Unicode1NameLen as U1NL
import qualified Data.Unistring.UCD.Internal.Unicode1NamePtr as U1NP
import qualified Data.Unistring.UCD.Internal.UnifiedIdeograph as UI
import qualified Data.Unistring.UCD.Internal.Uppercase as UC
import qualified Data.Unistring.UCD.Internal.VerticalOrientation as VO
import qualified Data.Unistring.UCD.Internal.WordBreak as WB
import qualified Data.Unistring.UCD.Internal.XidContinue as XIC
import qualified Data.Unistring.UCD.Internal.XidStart as XIS
import Data.Unistring.UCD.Unsafe (CodePoint(CodePoint))

-- | Class for types that can be converted into a 'CodePoint'.
--
-- @since 0.1.0.0
class IsCodePoint c where
  -- | Convert the value to 'CodePoint'
  --
  -- @since 0.1.0.0
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . toEnum . ord

-- | Basic classification for code points, based on their primary
-- usage.
--
-- Keep in mind that the classification is based only on most typical
-- usage, and is not intended to be comprehensive.  For a simple
-- example, ‘\<’ and ‘\>’ are classified as 'MathSymbol', despite their
-- occasional use as bracket punctuation.
--
-- === __Examples__
--
-- Code point counts per general category:
--
-- >>> import Data.List (sort)
-- >>> import qualified Data.List.NonEmpty as NE
-- >>> mapM_ (\cs -> putStrLn $ show (NE.head cs) ++ ": " ++ show (NE.length cs)) $ NE.group $ sort $ map generalCategory ['\x0'..]
-- UppercaseLetter: 1788
-- LowercaseLetter: 2151
-- TitlecaseLetter: 31
-- ModifierLetter: 259
-- OtherLetter: 121414
-- NonSpacingMark: 1826
-- SpacingCombiningMark: 429
-- EnclosingMark: 13
-- DecimalNumber: 630
-- LetterNumber: 236
-- OtherNumber: 888
-- ConnectorPunctuation: 10
-- DashPunctuation: 24
-- OpenPunctuation: 75
-- ClosePunctuation: 73
-- InitialQuote: 12
-- FinalQuote: 10
-- OtherPunctuation: 588
-- MathSymbol: 948
-- CurrencySymbol: 62
-- ModifierSymbol: 121
-- OtherSymbol: 6161
-- Space: 17
-- LineSeparator: 1
-- ParagraphSeparator: 1
-- Control: 65
-- Format: 161
-- Surrogate: 2048
-- PrivateUse: 137468
-- NotAssigned: 836602
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = withCP GC.retrieve

-- | The classes used for Unicode Canonical Ordering Algorithm.
--
-- If you're /not/ implementing canonical ordering, these are
-- generally of limited interest.
--
-- === __Examples__
--
-- Most code points have canonical combining class @0@
--
-- >>> canonicalCombiningClass 'a'
-- 0
-- >>> canonicalCombiningClass '0'
-- 0
--
-- Largest value is currently 240
--
-- >>> maximum $ map canonicalCombiningClass ['\x0'..]
-- 240
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
canonicalCombiningClass :: IsCodePoint cp => cp -> Word8
canonicalCombiningClass = withCP CCC.retrieve

-- | Unique identifier for a code point.
--
-- Names are typically derived from existing English names of a
-- character or symbol, but not always.
--
-- Names are limited to
--
-- * uppercase Latin letters
-- * space (U+0020)
-- * and hypthen (U+002D)
--
-- In particular, they are always in ASCII.
--
-- Names are /stable/ — that is, once the code point is assigned, the
-- name will not change in future Unicode versions.  A downside of
-- that policy is that names sometimes end up being incorrect, either
-- grammatically or semantically.  For more details, see
-- 'nameAliases'.
--
-- === __Examples__
--
-- Names are not defined for control code points
--
-- >>> name '\n'
-- ""
--
-- Most code points have predictable names
--
-- >>> name 'a'
-- "LATIN SMALL LETTER A"
--
-- Names for some code points are algorithmically derived, and thus
-- uninformative
--
-- >>> name '\x34ab'
-- "CJK UNIFIED IDEOGRAPH-34AB"
--
-- And sometimes names have typos that can't be fixed because of
-- stability guarantees
--
-- >>> name '\xfe18'
-- "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET"
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
name :: IsCodePoint cp => cp -> ByteString
name cp
  | 0xAC00 <= icp && icp <= 0xD7A3 =
    "HANGUL SYLLABLE " `mappend` hangulSyllableSuffix
  | otherwise =
    case prefix of
      Just nameP -> nameP `mappend` renderUnicodeInt icp
      Nothing -> mkByteString (NameLen.retrieve icp) (NamePtr.retrieve icp)
  where
    icp = fromEnum $ toCodePoint cp
    hangulSyllableSuffix
      | tindex > 0 = ljsn `mappend` vjsn `mappend` tjsn
      | otherwise = ljsn `mappend` vjsn
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

-- | Additional formal aliases for code points.
--
-- Name aliases occupy the same name space as names (and named
-- character sequences); that is, each name alias (and name) is unique
-- across the full range of all names and aliases.
--
-- === __Examples__
--
-- Aliases of type 'CorrectionAlias' are given to code points for
-- which the name is incorrect, either because of a typo…
--
-- >>> name '\xfe18'
-- "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET"
-- >>> nameAliases '\xfe18'
-- [(CorrectionAlias,"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET")]
--
-- … or because of a historical accident
--
-- >>> name '\x01a2'
-- "LATIN CAPITAL LETTER OI"
-- >>> nameAliases '\x01a2'
-- [(CorrectionAlias,"LATIN CAPITAL LETTER GHA")]
--
-- Do note that ‘name is incorrect’ refers to semantic correctness;
-- the normative value of the @Name@ property is still given by
-- 'name', and it is what it is.
--
-- Control codes have aliases of type 'ControlAlias', giving their ISO
-- 6429 names, as well as 'AbbreviationAlias' with a corresponding
-- abbreviation.
--
-- >>> nameAliases '\b'
-- [(ControlAlias,"BACKSPACE"),(AbbreviationAlias,"BS")]
--
-- Except for @'\\a'@, because the corresponding ISO 6429 name (@\"BELL\"@)
-- is taken by @'\\x1f514'@:
--
-- >>> nameAliases '\a'
-- [(ControlAlias,"ALERT"),(AbbreviationAlias,"BEL")]
-- >>> name '\x1f514'
-- "BELL"
--
-- There is, as of the moment of this writing, /exactly one/
-- occurrence of 'AlternateAlias' type
--
-- >>> name '\xfeff'
-- "ZERO WIDTH NO-BREAK SPACE"
-- >>> nameAliases '\xfeff'
-- [(AlternateAlias,"BYTE ORDER MARK"),(AbbreviationAlias,"BOM"),(AbbreviationAlias,"ZWNBSP")]
-- >>> mapM_ (print . toCodePoint) $ filter ((AlternateAlias `elem`) . map fst . nameAliases) ['\x0'..]
-- toEnum 0xfeff
--
-- 'FigmentAlias' aliases are names that leaked into the general use
-- from early standard drafts, but were never actually standardised
--
-- >>> nameAliases '\x80'
-- [(FigmentAlias,"PADDING CHARACTER"),(AbbreviationAlias,"PAD")]
-- >>> nameAliases '\x81'
-- [(FigmentAlias,"HIGH OCTET PRESET"),(AbbreviationAlias,"HOP")]
-- >>> nameAliases '\x99'
-- [(FigmentAlias,"SINGLE GRAPHIC CHARACTER INTRODUCER"),(AbbreviationAlias,"SGC")]
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
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

-- | Unicode allocation block to which the code point belongs
--
-- Despite their normative status, blocks do not convey any semantic
-- meaning and are used primarily to organise code charts.
--
-- To avoid breaking up code charts columns (which consist of 16 code
-- points), all blocks begin at code point divisible by 16, and end at
-- code point that is one less than divisible by 16 (i.e. the last
-- hexadecimal digit is ‘F’).
--
-- === __Examples__
--
-- The 'BasicLatinBlock' is just ASCII:
--
-- >>> filter ((== Just BasicLatinBlock) . block) ['\x0'..] == ['\x0'..'\x7f']
-- True
--
-- Since blocks are simply a crude partitioning of the code space,
-- there are many unassigned code points in assigned blocks:
--
-- >>> import Data.Maybe (isJust)
-- >>> length $ filter (\c -> isJust (block c) && generalCategory c == NotAssigned) ['\x0'..]
-- 3882
--
-- Blocks in the enumeration are given in the order they are located
-- in the codespace:
--
-- >>> import Data.Maybe (mapMaybe)
-- >>> import Data.List (group)
-- >>> map head (group $ mapMaybe block ['\x0'..]) == [minBound..]
-- True
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
block :: IsCodePoint cp => cp -> Maybe Block
block = withCP $ Blocks.retrieve . (`shiftR` 4)

-- | The first version of the Unicode standard in which the code point
-- was assigned /or/ reserved, starting with Unicode 1.1.
--
-- While the age values are represented here by the 'Age' enumeration,
-- the Unicode Standard provides the following restrictions on values
-- of this property:
--
-- * Age consists of a major and minor version numbers (code points
--   are never assigned in update versions with non-null third
--   number);
-- * Major version number is constrained to the range @1..255@;
-- * Minor version number is constrained to the range @0..255@.
--
-- See [the corresponding section of UAX #44](https://www.unicode.org/reports/tr44/#Character_Age)
-- for more details.
--
-- === __Examples__
--
-- All assigned code points have an 'age' value
--
-- >>> all (\c -> age c /= Nothing) $ filter (\c -> generalCategory c /= NotAssigned) ['\x0'..]
-- True
--
-- In addition, a number of noncharacter code points have a definite
-- value of 'age', corresponding to the Unicode Standard version in
-- which they were first reserved.
--
-- >>> length $ filter (\c -> (generalCategory c == NotAssigned) && (age c /= Nothing)) ['\x0'..]
-- 66
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
age :: IsCodePoint cp => cp -> Maybe Age
age = withCP Age.retrieve

-- | A script to which the code point belongs.
--
-- For the detailed discussion, consult [UAX #24 ‘Unicode Script Property’](https://unicode.org/reports/tr24/).
--
-- === __Examples__
--
-- Many code points have intuitively predictable Script values
--
-- >>> script 'a'
-- LatinScript
-- >>> script 'я'
-- CyrillicScript
-- >>> script '日'
-- HanScript
--
-- Some code points are used with multiple scripts; they are given a
-- special value 'CommonScript'
--
-- >>> script '.'
-- CommonScript
-- >>> script '0'
-- CommonScript
--
-- Some code points inherit the script of the previous code point
--
-- >>> name '\x200c'
-- "ZERO WIDTH NON-JOINER"
-- >>> script '\x200c'
-- InheritedScript
--
-- Unassigned, private use, noncharacter and surrogate code points
-- have an 'UnknownScript'
--
-- >>> generalCategory '\x10fff0'
-- PrivateUse
-- >>> script '\x10fff0'
-- UnknownScript
--
-- === Property type
--
-- This is an /informative/ property
--
-- @since 0.1.0.0
script :: IsCodePoint cp => cp -> Script
script = withCP Script.retrieve

-- | The scripts that the code point is used with.
--
-- For most code points @c@ this is just @['script' c]@, but if the
-- code point can be used with multiple scripts, /and/ their number is
-- sufficiently small, this is instead a enumeration of all the
-- scripts it's actually used with.
--
-- === __Examples__
--
-- For most code points, this function coincides with 'script':
--
-- >>> script 'a'
-- LatinScript
-- >>> scriptExtensions 'a'
-- [LatinScript]
--
-- For some, however, it is an explicit enumeration:
--
-- >>> name '\x3099'
-- "COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK"
-- >>> script '\x3099'
-- InheritedScript
-- >>> scriptExtensions '\x3099'
-- [HiraganaScript,KatakanaScript]
--
-- Sometimes there is just one script in enumeration
--
-- >>> name '\x1cf7'
-- "VEDIC SIGN ATIKRAMA"
-- >>> script '\x1cf7'
-- CommonScript
-- >>> scriptExtensions '\x1cf7'
-- [BengaliScript]
--
-- And sometimes the enumeration can be rather long
--
-- >>> name '\x0965'
-- "DEVANAGARI DOUBLE DANDA"
-- >>> mapM_ print $ zip [(0::Int)..] $ scriptExtensions '\x0965'
-- (0,BengaliScript)
-- (1,DevanagariScript)
-- (2,DograScript)
-- (3,GunjalaGondiScript)
-- (4,MasaramGondiScript)
-- (5,GranthaScript)
-- (6,GujaratiScript)
-- (7,GurmukhiScript)
-- (8,KannadaScript)
-- (9,LimbuScript)
-- (10,MahajaniScript)
-- (11,MalayalamScript)
-- (12,NandinagariScript)
-- (13,OriyaScript)
-- (14,KhudawadiScript)
-- (15,SinhalaScript)
-- (16,SylotiNagriScript)
-- (17,TakriScript)
-- (18,TamilScript)
-- (19,TeluguScript)
-- (20,TirhutaScript)
--
-- But if the enumeration would be /too/ long, it is not attempted
--
-- >>> scriptExtensions '.'
-- [CommonScript]
--
-- Sometimes the enumeration is non-trivial even if the code point
-- already has a reasonable 'script' value
--
-- >>> name '\x0483'
-- "COMBINING CYRILLIC TITLO"
-- >>> script '\x0483'
-- CyrillicScript
-- >>> scriptExtensions '\x0483'
-- [CyrillicScript,OldPermicScript]
--
-- === Property type
--
-- This is an /informative/ property
--
-- @since 0.1.0.0
scriptExtensions :: IsCodePoint cp => cp -> [Script]
scriptExtensions cp =
  if count == 0
    then [script cp]
    else scriptExtensionsRaw cp
  where
    count = SELen.retrieve icp
    icp = fromEnum $ toCodePoint cp

-- | Same as 'scriptExtensions', but the common case of
-- @'scriptExtensions' c == ['script' c]@ is represented by an empty
-- list instead.
--
-- @since 0.1.0.0
--
-- === __Examples__
--
-- Most code points produce an empty list
--
-- >>> script 'a'
-- LatinScript
-- >>> scriptExtensions 'a'
-- [LatinScript]
-- >>> scriptExtensionsRaw 'a'
-- []
--
-- Only non-trivial lists are produced
--
-- >>> script '\x3099'
-- InheritedScript
-- >>> scriptExtensions '\x3099'
-- [HiraganaScript,KatakanaScript]
-- >>> scriptExtensionsRaw '\x3099'
-- [HiraganaScript,KatakanaScript]
--
-- Even if they consist of only one value
--
-- >>> script '\x1cf7'
-- CommonScript
-- >>> scriptExtensions '\x1cf7'
-- [BengaliScript]
-- >>> scriptExtensionsRaw '\x1cf7'
-- [BengaliScript]
scriptExtensionsRaw :: IsCodePoint cp => cp -> [Script]
{-# INLINE scriptExtensionsRaw #-} -- List fusion
scriptExtensionsRaw cp =
  map (toEnum . fromEnum . unsafeReadPtr ptr) [0 .. count - 1]
  where
    ptr = SEPtr.retrieve icp
    count = SELen.retrieve icp
    icp = fromEnum $ toCodePoint cp

-- | Spaces and other code points that should be treated as ‘white
--  space’ for parsing purposes.
--
-- === __Examples__
--
-- All code points commonly treated as white space are included in this
-- category
--
-- >>> whiteSpace ' '
-- True
-- >>> whiteSpace '\n'
-- True
-- >>> whiteSpace '\r'
-- True
--
-- And it obviously doesn't include non-whitespace characters
--
-- >>> whiteSpace 'a'
-- False
-- >>> whiteSpace 'я'
-- False
--
-- But it has some unusual code points
--
-- >>> whiteSpace ' '
-- True
-- >>> name ' '
-- "OGHAM SPACE MARK"
--
-- See section 8.12 of the Unicode Standard for explanation.
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
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

-- | Special format control characters used in
-- [UAX #9 ‘Unicode Bidirectional Algorithm’](https://www.unicode.org/reports/tr9/)
--
-- === __Examples__
--
-- There aren't many of these
--
-- >>> mapM_ (print . name) $ filter bidiControl ['\0'..]
-- "ARABIC LETTER MARK"
-- "LEFT-TO-RIGHT MARK"
-- "RIGHT-TO-LEFT MARK"
-- "LEFT-TO-RIGHT EMBEDDING"
-- "RIGHT-TO-LEFT EMBEDDING"
-- "POP DIRECTIONAL FORMATTING"
-- "LEFT-TO-RIGHT OVERRIDE"
-- "RIGHT-TO-LEFT OVERRIDE"
-- "LEFT-TO-RIGHT ISOLATE"
-- "RIGHT-TO-LEFT ISOLATE"
-- "FIRST STRONG ISOLATE"
-- "POP DIRECTIONAL ISOLATE"
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
bidiControl :: IsCodePoint cp => cp -> Bool
bidiControl = withCP BC.retrieve

-- | Format control code points which have specific functions for
-- control of cursive joining and ligature formation.
--
-- === __Examples__
--
-- There are only two such code points
--
-- >>> map name $ filter joinControl ['\0'..]
-- ["ZERO WIDTH NON-JOINER","ZERO WIDTH JOINER"]
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
joinControl :: IsCodePoint cp => cp -> Bool
joinControl c = cp >= CodePoint 0x200C && cp <= CodePoint 0x200D
  where
    cp = toCodePoint c

-- | Dashes and their compatibility equivalents.
--
-- === __Examples__
--
-- The full list
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter dash ['\0'..]
-- (toEnum 0x2d,"HYPHEN-MINUS")
-- (toEnum 0x58a,"ARMENIAN HYPHEN")
-- (toEnum 0x5be,"HEBREW PUNCTUATION MAQAF")
-- (toEnum 0x1400,"CANADIAN SYLLABICS HYPHEN")
-- (toEnum 0x1806,"MONGOLIAN TODO SOFT HYPHEN")
-- (toEnum 0x2010,"HYPHEN")
-- (toEnum 0x2011,"NON-BREAKING HYPHEN")
-- (toEnum 0x2012,"FIGURE DASH")
-- (toEnum 0x2013,"EN DASH")
-- (toEnum 0x2014,"EM DASH")
-- (toEnum 0x2015,"HORIZONTAL BAR")
-- (toEnum 0x2053,"SWUNG DASH")
-- (toEnum 0x207b,"SUPERSCRIPT MINUS")
-- (toEnum 0x208b,"SUBSCRIPT MINUS")
-- (toEnum 0x2212,"MINUS SIGN")
-- (toEnum 0x2e17,"DOUBLE OBLIQUE HYPHEN")
-- (toEnum 0x2e1a,"HYPHEN WITH DIAERESIS")
-- (toEnum 0x2e3a,"TWO-EM DASH")
-- (toEnum 0x2e3b,"THREE-EM DASH")
-- (toEnum 0x2e40,"DOUBLE HYPHEN")
-- (toEnum 0x301c,"WAVE DASH")
-- (toEnum 0x3030,"WAVY DASH")
-- (toEnum 0x30a0,"KATAKANA-HIRAGANA DOUBLE HYPHEN")
-- (toEnum 0xfe31,"PRESENTATION FORM FOR VERTICAL EM DASH")
-- (toEnum 0xfe32,"PRESENTATION FORM FOR VERTICAL EN DASH")
-- (toEnum 0xfe58,"SMALL EM DASH")
-- (toEnum 0xfe63,"SMALL HYPHEN-MINUS")
-- (toEnum 0xff0d,"FULLWIDTH HYPHEN-MINUS")
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
dash :: IsCodePoint cp => cp -> Bool
dash = withCP Da.retrieve

-- | Various code points used as quotation marks in different languages.
--
-- === __Examples__
--
-- The full list
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter quotationMark ['\0'..]
-- (toEnum 0x22,"QUOTATION MARK")
-- (toEnum 0x27,"APOSTROPHE")
-- (toEnum 0xab,"LEFT-POINTING DOUBLE ANGLE QUOTATION MARK")
-- (toEnum 0xbb,"RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK")
-- (toEnum 0x2018,"LEFT SINGLE QUOTATION MARK")
-- (toEnum 0x2019,"RIGHT SINGLE QUOTATION MARK")
-- (toEnum 0x201a,"SINGLE LOW-9 QUOTATION MARK")
-- (toEnum 0x201b,"SINGLE HIGH-REVERSED-9 QUOTATION MARK")
-- (toEnum 0x201c,"LEFT DOUBLE QUOTATION MARK")
-- (toEnum 0x201d,"RIGHT DOUBLE QUOTATION MARK")
-- (toEnum 0x201e,"DOUBLE LOW-9 QUOTATION MARK")
-- (toEnum 0x201f,"DOUBLE HIGH-REVERSED-9 QUOTATION MARK")
-- (toEnum 0x2039,"SINGLE LEFT-POINTING ANGLE QUOTATION MARK")
-- (toEnum 0x203a,"SINGLE RIGHT-POINTING ANGLE QUOTATION MARK")
-- (toEnum 0x2e42,"DOUBLE LOW-REVERSED-9 QUOTATION MARK")
-- (toEnum 0x300c,"LEFT CORNER BRACKET")
-- (toEnum 0x300d,"RIGHT CORNER BRACKET")
-- (toEnum 0x300e,"LEFT WHITE CORNER BRACKET")
-- (toEnum 0x300f,"RIGHT WHITE CORNER BRACKET")
-- (toEnum 0x301d,"REVERSED DOUBLE PRIME QUOTATION MARK")
-- (toEnum 0x301e,"DOUBLE PRIME QUOTATION MARK")
-- (toEnum 0x301f,"LOW DOUBLE PRIME QUOTATION MARK")
-- (toEnum 0xfe41,"PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET")
-- (toEnum 0xfe42,"PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET")
-- (toEnum 0xfe43,"PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET")
-- (toEnum 0xfe44,"PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET")
-- (toEnum 0xff02,"FULLWIDTH QUOTATION MARK")
-- (toEnum 0xff07,"FULLWIDTH APOSTROPHE")
-- (toEnum 0xff62,"HALFWIDTH LEFT CORNER BRACKET")
-- (toEnum 0xff63,"HALFWIDTH RIGHT CORNER BRACKET")
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
quotationMark :: IsCodePoint cp => cp -> Bool
quotationMark = withCP QM.retrieve

-- | Punctuation that generally marks the end of textual units.
--
-- === __Examples__
--
-- This category includes all of 'sentenceTerminal'
--
-- >>> all terminalPunctuation $ filter sentenceTerminal ['\0'..]
-- True
--
-- plus a number of other code points that generally don't terminate
-- /sentences/
--
-- >>> sentenceTerminal ':'
-- False
-- >>> terminalPunctuation ':'
-- True
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
terminalPunctuation :: IsCodePoint cp => cp -> Bool
terminalPunctuation = withCP TP.retrieve

-- | Hexadecimal digits
--
-- === __Examples__
--
-- This category includes all the usual hexadecimal digits, as well as
-- their fullwidth variants.
--
-- >>> putStrLn $ filter hexDigit ['\x0'..]
-- 0123456789ABCDEFabcdef０１２３４５６７８９ＡＢＣＤＥＦａｂｃｄｅｆ
--
-- Note that, since their primary use is as letters, not digits,
-- ‘digits’ a–f do not have 'numeric' properties.
--
-- >>> numeric 'a'
-- Nothing
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
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

-- | ASCII hexadecimal digits
--
-- === __Examples__
--
-- This category includes just the usual hexadecimal digits, and
-- differs from 'hexDigit' in that it doesn't include fullwidth
-- variants.
--
-- >>> putStrLn $ filter asciiHexDigit ['\x0'..]
-- 0123456789ABCDEFabcdef
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
asciiHexDigit :: IsCodePoint cp => cp -> Bool
asciiHexDigit c =
  cp <= 0x66 &&
  cp >= 0x30 && (cp <= 0x39 || (0x41 <= cp && cp <= 0x46) || cp >= 0x61)
  where
    CodePoint cp = toCodePoint c

-- | Chinese-related ideographs.
--
-- === __Examples__
--
-- >>> name '\x34AB'
-- "CJK UNIFIED IDEOGRAPH-34AB"
-- >>> ideographic '\x34AB'
-- True
--
-- Note that this is a property specifically for /chinese/ and other
-- siniform ideographs.
--
-- >>> name '\x13000'
-- "EGYPTIAN HIEROGLYPH A001"
-- >>> ideographic '\x13000'
-- False
--
-- >>> name '\x12000'
-- "CUNEIFORM SIGN A"
-- >>> ideographic '\x12000'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
ideographic :: IsCodePoint cp => cp -> Bool
ideographic = withCP Ide.retrieve

-- | Code points corresponding to characters that linguistically
-- modify the meaning of another character to which they apply.
--
-- === __Examples__
--
-- Not all diacritics are combining characters
--
-- >>> name '\xb7'
-- "MIDDLE DOT"
-- >>> diacritic '\xb7'
-- True
-- >>> generalCategory '\xb7'
-- OtherPunctuation
--
-- and not all combining characters are diacritics
--
-- >>> name '\x20dd'
-- "COMBINING ENCLOSING CIRCLE"
-- >>> diacritic '\x20dd'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
diacritic :: IsCodePoint cp => cp -> Bool
diacritic = withCP Di.retrieve

-- | Code points the principal function of which is to ‘extend’ the
-- adjacent characters, either logically or graphically.
--
-- === __Examples__
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ take 10 $ filter extender ['\0'..]
-- (toEnum 0xb7,"MIDDLE DOT")
-- (toEnum 0x2d0,"MODIFIER LETTER TRIANGULAR COLON")
-- (toEnum 0x2d1,"MODIFIER LETTER HALF TRIANGULAR COLON")
-- (toEnum 0x640,"ARABIC TATWEEL")
-- (toEnum 0x7fa,"NKO LAJANYALAN")
-- (toEnum 0xe46,"THAI CHARACTER MAIYAMOK")
-- (toEnum 0xec6,"LAO KO LA")
-- (toEnum 0x180a,"MONGOLIAN NIRUGU")
-- (toEnum 0x1843,"MONGOLIAN LETTER TODO LONG VOWEL SIGN")
-- (toEnum 0x1aa7,"TAI THAM SIGN MAI YAMOK")
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
extender :: IsCodePoint cp => cp -> Bool
extender = withCP Ext.retrieve

-- | Code points that are guaranteed never to be assigned, and are
-- intended for applications' internal use.
--
-- === __Examples__
--
-- On each plane, code point ending in @ffff@ is reserved so that it
-- can be used as a delimiter…
--
-- >>> import Data.Char (chr)
-- >>> all noncharacterCodePoint [ chr $ plane * 0x10000 + 0xffff | plane <- [0..16]]
-- True
--
-- …and code point ending in @fffe@ is reserved so that the byte-order
-- mark is unambiguous
--
-- >>> nameAliases '\xfeff'
-- [(AlternateAlias,"BYTE ORDER MARK"),(AbbreviationAlias,"BOM"),(AbbreviationAlias,"ZWNBSP")]
-- >>> import Data.Char (chr)
-- >>> all noncharacterCodePoint [ chr $ plane * 0x10000 + 0xfffe | plane <- [0..16]]
-- True
--
-- In addition, there are 32 reserved code points on a Basic
-- Multilingual Plane for internal use
--
-- >>> all noncharacterCodePoint ['\xfdd0'..'\xfdef']
-- True
--
-- And these @2 * 17 + 32 = 66@ are the only ones
--
-- >>> length $ filter noncharacterCodePoint ['\x0'..]
-- 66
--
-- === Property status
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
noncharacterCodePoint :: IsCodePoint cp => cp -> Bool
noncharacterCodePoint c =
  (0xfdd0 <= cp && cp <= 0xfdef) || (cp .&. 0xfffe == 0xfffe)
  where
    CodePoint cp = toCodePoint c

-- | A property used in the definition of ideographic description
-- sequences.
--
-- See section 18.2 of the Unicode Standard for details.
--
-- === __Examples__
--
-- The full list:
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter idsBinaryOperator ['\0'..]
-- (toEnum 0x2ff0,"IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT")
-- (toEnum 0x2ff1,"IDEOGRAPHIC DESCRIPTION CHARACTER ABOVE TO BELOW")
-- (toEnum 0x2ff4,"IDEOGRAPHIC DESCRIPTION CHARACTER FULL SURROUND")
-- (toEnum 0x2ff5,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM ABOVE")
-- (toEnum 0x2ff6,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM BELOW")
-- (toEnum 0x2ff7,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM LEFT")
-- (toEnum 0x2ff8,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM UPPER LEFT")
-- (toEnum 0x2ff9,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM UPPER RIGHT")
-- (toEnum 0x2ffa,"IDEOGRAPHIC DESCRIPTION CHARACTER SURROUND FROM LOWER LEFT")
-- (toEnum 0x2ffb,"IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID")
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
idsBinaryOperator :: IsCodePoint cp => cp -> Bool
idsBinaryOperator c =
  cp .&. 0xfffff0 == 0x2ff0 && (cp <= 0x2ff1 || (0x2ff4 <= cp && cp <= 0x2ffb))
  where
    CodePoint cp = toCodePoint c

-- | A property used in the definition of ideographic description
-- sequences.
--
-- See section 18.2 of the Unicode Standard for details.
--
-- === __Examples__
--
-- The full list:
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter idsTrinaryOperator ['\0'..]
-- (toEnum 0x2ff2,"IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT")
-- (toEnum 0x2ff3,"IDEOGRAPHIC DESCRIPTION CHARACTER ABOVE TO MIDDLE AND BELOW")
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
idsTrinaryOperator :: IsCodePoint cp => cp -> Bool
idsTrinaryOperator c = 0x2ff2 <= cp && cp <= 0x2ff3
  where
    CodePoint cp = toCodePoint c

-- | A property used in the definition of ideographic description
-- sequences.
--
-- See section 18.2 of the Unicode Standard for details.
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
radical :: IsCodePoint cp => cp -> Bool
radical c =
  0x2e80 <= cp &&
  cp <= 0x2fd5 &&
  (cp <= 0x2e99 || (0x2e9b <= cp && cp <= 0x2ef3) || 0x2f00 <= cp)
  where
    CodePoint cp = toCodePoint c

-- | The set of unified CJK ideographs.
--
-- This is a subset of 'ideographic'.
--
-- === __Examples__
--
-- >>> name '\x34ab'
-- "CJK UNIFIED IDEOGRAPH-34AB"
-- >>> unifiedIdeograph '\x34ab'
-- True
--
-- Examples of CJK ideographs that do not count as unified are
-- compatibility ideographs
--
-- >>> name '\xf900'
-- "CJK COMPATIBILITY IDEOGRAPH-F900"
-- >>> ideographic '\xf900'
-- True
-- >>> unifiedIdeograph '\xf900'
-- False
-- >>> canonicalDecomposition '\xf900'
-- [toEnum 0x8c48]
--
-- and 'CJKSymbolsAndPunctuationBlock'
--
-- >>> name '\x3024'
-- "HANGZHOU NUMERAL FOUR"
-- >>> ideographic '\x3024'
-- True
-- >>> unifiedIdeograph '\x3024'
-- False
-- >>> block '\x3024'
-- Just CJKSymbolsAndPunctuationBlock
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
unifiedIdeograph :: IsCodePoint cp => cp -> Bool
unifiedIdeograph = withCP UI.retrieve

-- | Deprecated code points.
--
-- The assignments in Unicode code space are /never/ undone, but the
-- use of these code points is strongly discouraged, typically either
-- due to architectural problems with the code points themselves or
-- implementation problems caused by their support.
--
-- === __Examples__
--
-- Unicode used to (technically still does) have a mechanism for
-- replacing default digit shapes with locale-appropriate ones; modern
-- recommendation is to just use appropriate code points
--
-- >>> name '\x206e'
-- "NATIONAL DIGIT SHAPES"
-- >>> deprecated '\x206e'
-- True
-- >>> name '\x206f'
-- "NOMINAL DIGIT SHAPES"
-- >>> deprecated '\x206f'
-- True
--
-- Unicode used to (again, technically still does) have own mechanism
-- for embedding language tags; modern recommendation is to rely on a
-- higher-level protocol instead
--
-- >>> name '\xe0001'
-- "LANGUAGE TAG"
-- >>> deprecated '\xe0001'
-- True
--
-- In total, there aren't that many deprecated code points
--
-- >>> length $ filter deprecated ['\x0'..]
-- 15
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
deprecated :: IsCodePoint cp => cp -> Bool
deprecated = withCP De.retrieve

-- | Code points that
--
-- * render with a dot on top, and
-- * should have that dot removed if another accent is applied.
--
-- === __Examples__
--
-- Most of these are some variations of @i@ and @j@
--
-- >>> softDotted 'i'
-- True
--
-- Here's this property in action:
--
-- >>> putStrLn "i"
-- i
-- >>> putStrLn "i\x0300"
-- ì
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
softDotted :: IsCodePoint cp => cp -> Bool
softDotted = withCP SD.retrieve

-- | Code points that are stored in strings in visual order, rather
-- than logical one.
--
-- As a general principle, code points in Unicode strings are stored
-- in a /logical/ order, which usually corresponds to the phonetic and
-- typing order.  This is true regardless of the relative positioning
-- of the resulting glyphs.
--
-- Except, that is, for these code points, which are stored in a
-- visual order instead.  This makes for a simpler rendering, but more
-- complicated sorting and search.
--
-- === __Examples__
--
-- This category is the same as 'VisualOrderLeftIPC':
--
-- >>> all (\c -> logicalOrderException c == (indicPositionalCategory c == Just VisualOrderLeftIPC)) ['\x0'..]
-- True
--
-- There are only a few such code points
--
-- >>> length $ filter logicalOrderException ['\x0'..]
-- 19
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
logicalOrderException :: IsCodePoint cp => cp -> Bool
logicalOrderException = withCP LOE.retrieve

-- | Punctuation that generally marks the end of sentences.
--
-- === __Examples__
--
-- >>> sentenceTerminal '.'
-- True
-- >>> sentenceTerminal ','
-- False
--
-- >>> name '\x11047'
-- "BRAHMI DANDA"
-- >>> sentenceTerminal '\x11047'
-- True
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
sentenceTerminal :: IsCodePoint cp => cp -> Bool
sentenceTerminal = withCP ST.retrieve

-- | Dedicated code points used to choose a particular rendering of
-- the previous code point.
--
-- Specifically, Unicode Standard specifies a number of alternative
-- renderings for some code points that can be selected via variation
-- selectors; see @StandardizedVariants.txt@ in the UCD and
-- @emoji-variation-sequences.txt@ in UTS #51 data files for full list.
--
-- === __Examples__
--
-- There are three variation selectors for use specifically with
-- Mongolian script…
--
-- >>> map toCodePoint $ filter variationSelector ['\x0'..'\xfdff']
-- [toEnum 0x180b,toEnum 0x180c,toEnum 0x180d]
-- >>> name '\x180b'
-- "MONGOLIAN FREE VARIATION SELECTOR ONE"
-- >>> name '\x180d'
-- "MONGOLIAN FREE VARIATION SELECTOR THREE"
--
-- …16 general-purpose variation selectors in the BMP…
--
-- >>> filter variationSelector ['\xfdff'..'\xffff'] == ['\xfe00'..'\xfe0f']
-- True
-- >>> name '\xfe00'
-- "VARIATION SELECTOR-1"
-- >>> name '\xfe0f'
-- "VARIATION SELECTOR-16"
--
-- …and 240 more in the 14th plane
--
-- >>> filter variationSelector ['\x10000'..] == ['\xe0100'..'\xe01ef']
-- True
-- >>> name '\xe0100'
-- "VARIATION SELECTOR-17"
-- >>> name '\xe01ef'
-- "VARIATION SELECTOR-256"
--
-- And here's a small example of variation selectors in action (second
-- variant should have a dot at the lower end of a vertical stroke—and
-- the first shouldn't):
--
-- >>> putStrLn "\xaa6f"
-- ꩯ
-- >>> putStrLn "\xaa6f\xfe00"
-- ꩯ︀
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
variationSelector :: IsCodePoint cp => cp -> Bool
variationSelector c =
  cp >= 0x180b &&
  (cp <= 0x180d ||
   (cp >= 0xfe00 && (cp <= 0xfe0f || (cp >= 0xe0100 && cp <= 0xe01ef))))
  where
    CodePoint cp = toCodePoint c

-- | A set of code points recommended to be recognised as whitespace
-- in patterns (such as regular expressions).
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- This property is /stable/: its values will never change.
--
-- === __Examples__
--
-- There are only a few of those:
--
-- >>> length $ filter patternWhiteSpace ['\0'..]
-- 11
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
patternWhiteSpace :: IsCodePoint cp => cp -> Bool
patternWhiteSpace c
  | cp <= 0x85 = (0x9 <= cp && cp <= 0xd) || cp == 0x20 || cp == 0x85
  | cp >= 0x200e = cp <= 0x200f || cp == 0x2028 || cp == 0x2029
  | otherwise = False
  where
    CodePoint cp = toCodePoint c

-- | A set of code points recommended to be recognised as a part of a
-- pattern syntax in patterns (such as regular expressions).
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- This property is /stable/: its values will never change.
--
-- === __Examples__
--
-- To provide stability while allowing for future expansions, a number
-- of unassigned code points are given this property.
--
-- >>> age '\x2427'
-- Nothing
-- >>> patternSyntax '\x2427'
-- True
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
patternSyntax :: IsCodePoint cp => cp -> Bool
patternSyntax = withCP PS.retrieve

-- | Visible format controls that precede and span a sequence of other
-- characters.
--
-- Here's an example with U+0601 ‘ARABIC SIGN SANAH’ from the Unicode
-- Standard, chapter 9.
--
-- <<images/arabic_year.png>>
--
-- === __Examples__
--
-- Here's the full list:
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter prependedConcatenationMark ['\0'..]
-- (toEnum 0x600,"ARABIC NUMBER SIGN")
-- (toEnum 0x601,"ARABIC SIGN SANAH")
-- (toEnum 0x602,"ARABIC FOOTNOTE MARKER")
-- (toEnum 0x603,"ARABIC SIGN SAFHA")
-- (toEnum 0x604,"ARABIC SIGN SAMVAT")
-- (toEnum 0x605,"ARABIC NUMBER MARK ABOVE")
-- (toEnum 0x6dd,"ARABIC END OF AYAH")
-- (toEnum 0x70f,"SYRIAC ABBREVIATION MARK")
-- (toEnum 0x8e2,"ARABIC DISPUTED END OF AYAH")
-- (toEnum 0x110bd,"KAITHI NUMBER SIGN")
-- (toEnum 0x110cd,"KAITHI NUMBER SIGN ABOVE")
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
prependedConcatenationMark :: IsCodePoint cp => cp -> Bool
prependedConcatenationMark = withCP PCM.retrieve

-- | 26 code points that correspond to letters of the Latin alphabet
-- and, when used in pairs, represent ISO 3166 region codes.
--
-- The Unicode Standard (section 22.10) technically permits any of the
-- 26×26 pairs to represent any region code, but in practice the
-- correspondence is straightforwardly given by the corresponding
-- letters.
--
-- Similarly, the Unicode Standard does not mandate any particular
-- rendering for resultant region codes, but in practice they are
-- rendered as a flag of the corresponding region.
--
-- === __Examples__
--
-- The full list
--
-- >>> mapM_ (\c -> print (toCodePoint c, name c)) $ filter regionalIndicator ['\0'..]
-- (toEnum 0x1f1e6,"REGIONAL INDICATOR SYMBOL LETTER A")
-- (toEnum 0x1f1e7,"REGIONAL INDICATOR SYMBOL LETTER B")
-- (toEnum 0x1f1e8,"REGIONAL INDICATOR SYMBOL LETTER C")
-- (toEnum 0x1f1e9,"REGIONAL INDICATOR SYMBOL LETTER D")
-- (toEnum 0x1f1ea,"REGIONAL INDICATOR SYMBOL LETTER E")
-- (toEnum 0x1f1eb,"REGIONAL INDICATOR SYMBOL LETTER F")
-- (toEnum 0x1f1ec,"REGIONAL INDICATOR SYMBOL LETTER G")
-- (toEnum 0x1f1ed,"REGIONAL INDICATOR SYMBOL LETTER H")
-- (toEnum 0x1f1ee,"REGIONAL INDICATOR SYMBOL LETTER I")
-- (toEnum 0x1f1ef,"REGIONAL INDICATOR SYMBOL LETTER J")
-- (toEnum 0x1f1f0,"REGIONAL INDICATOR SYMBOL LETTER K")
-- (toEnum 0x1f1f1,"REGIONAL INDICATOR SYMBOL LETTER L")
-- (toEnum 0x1f1f2,"REGIONAL INDICATOR SYMBOL LETTER M")
-- (toEnum 0x1f1f3,"REGIONAL INDICATOR SYMBOL LETTER N")
-- (toEnum 0x1f1f4,"REGIONAL INDICATOR SYMBOL LETTER O")
-- (toEnum 0x1f1f5,"REGIONAL INDICATOR SYMBOL LETTER P")
-- (toEnum 0x1f1f6,"REGIONAL INDICATOR SYMBOL LETTER Q")
-- (toEnum 0x1f1f7,"REGIONAL INDICATOR SYMBOL LETTER R")
-- (toEnum 0x1f1f8,"REGIONAL INDICATOR SYMBOL LETTER S")
-- (toEnum 0x1f1f9,"REGIONAL INDICATOR SYMBOL LETTER T")
-- (toEnum 0x1f1fa,"REGIONAL INDICATOR SYMBOL LETTER U")
-- (toEnum 0x1f1fb,"REGIONAL INDICATOR SYMBOL LETTER V")
-- (toEnum 0x1f1fc,"REGIONAL INDICATOR SYMBOL LETTER W")
-- (toEnum 0x1f1fd,"REGIONAL INDICATOR SYMBOL LETTER X")
-- (toEnum 0x1f1fe,"REGIONAL INDICATOR SYMBOL LETTER Y")
-- (toEnum 0x1f1ff,"REGIONAL INDICATOR SYMBOL LETTER Z")
--
-- Example usage
--
-- >>> name '\x1f1fa'
-- "REGIONAL INDICATOR SYMBOL LETTER U"
-- >>> name '\x1f1f3'
-- "REGIONAL INDICATOR SYMBOL LETTER N"
-- >>> putStrLn "\x1f1fa\x1f1f3"
-- 🇺🇳
--
-- (the last line should be a flag of UN).
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
regionalIndicator :: IsCodePoint cp => cp -> Bool
regionalIndicator c = 0x1f1e6 <= cp && cp <= 0x1f1ff
  where
    CodePoint cp = toCodePoint c

-- | Symbols commonly used in mathematical text and formulas.
--
-- See section 22.5 of the Unicode Standard for more details.
--
-- === __Examples__
--
-- This category contains the entirety of 'MathSymbol'
--
-- >>> generalCategory '+'
-- MathSymbol
-- >>> math '+'
-- True
--
-- But also some letters
--
-- >>> name '\x3d0'
-- "GREEK BETA SYMBOL"
-- >>> generalCategory '\x3d0'
-- LowercaseLetter
-- >>> math '\x3d0'
-- True
--
-- accents
--
-- >>> math '^'
-- True
--
-- and other notation
--
-- >>> name '\x2032'
-- "PRIME"
-- >>> generalCategory '\x2032'
-- OtherPunctuation
-- >>> math '\x2032'
-- True
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
math :: IsCodePoint cp => cp -> Bool
math = withCP M.retrieve

-- | Primary units of alphabets and/or syllabaries
--
-- === __Examples__
--
-- This category includes alphabets of various kinds…
--
-- >>> name 'ᠦ'
-- "MONGOLIAN LETTER UE"
-- >>> alphabetic 'ᠦ'
-- True
-- >>> name 'ג'
-- "HEBREW LETTER GIMEL"
-- >>> alphabetic 'ג'
-- True
-- >>> name 'ऐ'
-- "DEVANAGARI LETTER AI"
-- >>> alphabetic 'ऐ'
-- True
--
-- …and syllabaries of various kinds…
--
-- >>> name '無'
-- "CJK UNIFIED IDEOGRAPH-7121"
-- >>> alphabetic '無'
-- True
-- >>> name 'オ'
-- "KATAKANA LETTER O"
-- >>> alphabetic 'オ'
-- True
-- >>> name '갅'
-- "HANGUL SYLLABLE GANJ"
-- >>> alphabetic '갅'
-- True
--
-- …including composite code points,…
--
-- >>> name 'Ǟ'
-- "LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON"
-- >>> alphabetic 'Ǟ'
-- True
--
-- …digraphs and ligatures,…
--
-- >>> name 'ʣ'
-- "LATIN SMALL LETTER DZ DIGRAPH"
-- >>> alphabetic 'ʣ'
-- True
-- >>> name 'œ'
-- "LATIN SMALL LIGATURE OE"
-- >>> alphabetic 'œ'
-- True
--
-- …modifier letters,…
--
-- >>> name '\x02b0'
-- "MODIFIER LETTER SMALL H"
-- >>> alphabetic '\x02b0'
-- True
--
-- …and /some/ other forms
--
-- >>> name '🄰'
-- "SQUARED LATIN CAPITAL LETTER A"
-- >>> alphabetic '🄰'
-- True
-- >>> name '🈒'
-- "SQUARED CJK UNIFIED IDEOGRAPH-53CC"
-- >>> alphabetic '🈒'
-- False
--
-- But not, for example, digits…
--
-- >>> alphabetic '0'
-- False
--
-- …or punctuation
--
-- >>> name '«'
-- "LEFT-POINTING DOUBLE ANGLE QUOTATION MARK"
-- >>> alphabetic '«'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
alphabetic :: IsCodePoint cp => cp -> Bool
alphabetic c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = A.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

-- | Code points representing uppercase symbols.
--
-- === __Examples__
--
-- This category includes uppercase letters (with 'UppercaseLetter'
-- 'generalCategory')…
--
-- >>> uppercase 'A'
-- True
--
-- …as well as some symbols that, while not technically letters, are
-- nevertheless definitely uppercase,…
--
-- >>> uppercase 'Ⓐ'
-- True
--
-- …but not lowercase letters or symbols with no notion of case
--
-- >>> uppercase 'a'
-- False
-- >>> uppercase '0'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
uppercase :: IsCodePoint cp => cp -> Bool
uppercase = withCP UC.retrieve

-- | Code points representing lowercase symbols.
--
-- === __Examples__
--
-- This category includes lowercase letters (with 'LowercaseLetter'
-- 'generalCategory')…
--
-- >>> lowercase 'a'
-- True
--
-- …as well as some symbols that, while not technically letters, are
-- nevertheless definitely lowercase,…
--
-- >>> lowercase 'ⓐ'
-- True
--
-- …but not uppercase letters or symbols with no notion of case
--
-- >>> lowercase 'A'
-- False
-- >>> lowercase '0'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
lowercase :: IsCodePoint cp => cp -> Bool
lowercase = withCP LC.retrieve

-- | Code points that are considered to be either uppercase,
-- lowercase, or titlecase.
--
-- === __Examples__
--
-- This is equivalent to a combination of 'lowercase' and 'uppercase'
-- properties and the general category 'TitlecaseLetter'
--
-- >>> flip all ['\x0'..] $ \c -> cased c == (uppercase c || lowercase c || (generalCategory c == TitlecaseLetter))
-- True
--
-- But it is /not/ equivalent to 'changesWhenCasemapped'
--
-- >>> name '\x138'
-- "LATIN SMALL LETTER KRA"
-- >>> cased '\x138'
-- True
-- >>> changesWhenCasemapped '\x138'
-- False
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
cased :: IsCodePoint cp => cp -> Bool
cased = withCP Cs.retrieve

-- | Code points which are ignored for certain case-related purposes.
--
-- For more details, see definition /D136/ in chapter 3 of the Unicode
-- Standard.
--
-- === __Examples__
--
-- Some punctuation is case ignorable:
--
-- >>> caseIgnorable ':'
-- True
--
-- Note that 'cased' and 'caseIgnorable' are not mutually exclusive
--
-- >>> name '\x37a'
-- "GREEK YPOGEGRAMMENI"
-- >>> cased '\x37a'
-- True
-- >>> caseIgnorable '\x37a'
-- True
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
caseIgnorable :: IsCodePoint cp => cp -> Bool
caseIgnorable = withCP CI.retrieve

-- | Code points that are not stable under 'lowercaseMapping'.
--
-- That is, code points for which @'lowercaseMapping' c /= 'SingleCM' c@.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenLowercased :: IsCodePoint cp => cp -> Bool
changesWhenLowercased = withCP CWL.retrieve

-- | Code points that are not stable under 'uppercaseMapping'.
--
-- That is, code points for which @'uppercaseMapping' c /= 'SingleCM' c@.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenUppercased :: IsCodePoint cp => cp -> Bool
changesWhenUppercased = withCP CWU.retrieve

-- | Code points that are not stable under 'titlecaseMapping'.
--
-- That is, code points for which @'titlecaseMapping' c /= 'SingleCM' c@.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenTitlecased :: IsCodePoint cp => cp -> Bool
changesWhenTitlecased = withCP CWT.retrieve

-- | Code points, /the normalised forms of which/ are not stable under
-- 'caseFolding'.
--
-- === __Examples__
--
-- >>> changesWhenCasefolded '\x01f0'
-- False
-- >>> caseFolding '\x01f0'
-- DoubleCM (toEnum 0x6a) (toEnum 0x30c)
-- >>> canonicalDecomposition '\x01f0'
-- [toEnum 0x6a,toEnum 0x30c]
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenCasefolded :: IsCodePoint cp => cp -> Bool
changesWhenCasefolded = withCP CWCF.retrieve

-- | Code points that are not stable under one of case mappings.
--
-- Specifically, this is a union of 'changesWhenLowercased',
-- 'changesWhenUppercased' and 'changesWhenTitlecased'.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenCasemapped :: IsCodePoint cp => cp -> Bool
changesWhenCasemapped = withCP CWCM.retrieve

-- | Original set of code points recommended to be allowed at the
-- start of identifiers.
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- Keep in mind that, absent special requirements, the UAX recommends
-- to use 'xidStart' instead.
--
-- This is a subset of 'idContinue'.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
idStart :: IsCodePoint cp => cp -> Bool
idStart c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = IS.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

-- | Original set of code points recommended to be allowed in
-- identifiers.
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- Keep in mind that, absent special requirements, the UAX recommends
-- to use 'xidContinue' instead.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
idContinue :: IsCodePoint cp => cp -> Bool
idContinue c
  | cp < 0xaa =
    cp >= 0x30 &&
    cp <= 0x7a &&
    (cp <= 0x39 || (cp >= 0x41 && (cp <= 0x5a || cp == 0x5f || cp >= 0x61)))
  | otherwise = IC.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

-- | Set of code points recommended to be allowed at the start of
-- identifiers.
--
-- Compared to 'idStart', it is modified so that the set of
-- identifiers is closed under all four normal forms and case
-- transformations.
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- This is a subset of 'xidContinue'.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
xidStart :: IsCodePoint cp => cp -> Bool
xidStart c
  | cp < 0xaa = cp >= 0x41 && cp <= 0x7a && (cp <= 0x5a || cp >= 0x61)
  | otherwise = XIS.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

-- | Set of code points recommended to be allowed in identifiers.
--
-- Compared to 'idContinue', it is modified so that the set of
-- identifiers is closed under all four normal forms and case
-- transformations.
--
-- See
-- [UAX #31 ‘Unicode Identifier and Pattern Syntax’](https://www.unicode.org/reports/tr31/)
-- for details.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
xidContinue :: IsCodePoint cp => cp -> Bool
xidContinue c
  | cp < 0xaa =
    cp >= 0x30 &&
    cp <= 0x7a &&
    (cp <= 0x39 || (cp >= 0x41 && (cp <= 0x5a || cp == 0x5f || cp >= 0x61)))
  | otherwise = XIC.retrieve (fromEnum cp)
  where
    CodePoint cp = toCodePoint c

-- | Code points that, if not supported, should be ignored for display
-- altogether instead of using a fallback glyph.
--
-- === __Examples__
--
-- This property is generally used for code points that have no
-- visible glyphs of their own even when supported, and instead affect
-- presentation of other code points
--
-- >>> name '\x034f'
-- "COMBINING GRAPHEME JOINER"
-- >>> defaultIgnorableCodePoint '\x034f'
-- True
-- >>> name '\x2060'
-- "WORD JOINER"
-- >>> defaultIgnorableCodePoint '\x2060'
-- True
-- >>> name '\xffa0'
-- "HALFWIDTH HANGUL FILLER"
-- >>> defaultIgnorableCodePoint '\xffa0'
-- True
--
-- Code points with own glyphs, even if whitespace, are not ignorable.
--
-- >>> defaultIgnorableCodePoint ' '
-- False
-- >>> defaultIgnorableCodePoint '㈰'
-- False
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
defaultIgnorableCodePoint :: IsCodePoint cp => cp -> Bool
defaultIgnorableCodePoint = withCP DICP.retrieve

-- | Grapheme extenders.
--
-- See D59 ‘Grapheme extender’ in chapter 3 of the Unicode Standard
-- for details.
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
graphemeExtend :: IsCodePoint cp => cp -> Bool
graphemeExtend = withCP GE.retrieve

-- | Single code point grapheme bases.
--
-- See D58 ‘Grapheme base’ in chapter 3 of the Unicode Standard for
-- details.
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
graphemeBase :: IsCodePoint cp => cp -> Bool
graphemeBase = withCP GB.retrieve

-- | Classification of Hangul jamo and precomposed syllables
--
-- === __Examples__
--
-- Most code points are not Hangul syllables of any kind
--
-- >>> hangulSyllableType 'Z'
-- Nothing
--
-- Each 'LVSyllable' has a canonical decomposition into a leading jamo
-- and a vowel jamo
--
-- >>> hangulSyllableType '\xac00'
-- Just LVSyllable
-- >>> canonicalDecomposition '\xac00'
-- [toEnum 0x1100,toEnum 0x1161]
-- >>> hangulSyllableType '\x1100'
-- Just LeadingJamo
-- >>> hangulSyllableType '\x1161'
-- Just VowelJamo
--
-- Each 'LVTSyllable' has a canonical decomposition into leading,
-- vowel, and trailing jamos (here the first two are the same as in
-- example above)
--
-- >>> hangulSyllableType '\xac01'
-- Just LVTSyllable
-- >>> canonicalDecomposition '\xac01'
-- [toEnum 0x1100,toEnum 0x1161,toEnum 0x11a8]
-- >>> hangulSyllableType '\x11a8'
-- Just TrailingJamo
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
hangulSyllableType :: IsCodePoint cp => cp -> Maybe HangulSyllableType
hangulSyllableType = withCP HST.retrieve

-- | A simplified version of 'lowercaseMapping', restricted to
-- non-expanding mappings.
--
-- === __Examples__
--
-- For most code points, simple and full lowercase mappings coincide:
--
-- >>> lowercaseMapping 'A'
-- SingleCM (toEnum 0x61)
-- >>> simpleLowercaseMapping 'A'
-- toEnum 0x61
--
-- But when they differ, simple mapping discards information
--
-- >>> name '\x130'
-- "LATIN CAPITAL LETTER I WITH DOT ABOVE"
-- >>> lowercaseMapping '\x130'
-- DoubleCM (toEnum 0x69) (toEnum 0x307)
-- >>> simpleLowercaseMapping '\x130'
-- toEnum 0x69
-- >>> name '\x69'
-- "LATIN SMALL LETTER I"
-- >>> name '\x307'
-- "COMBINING DOT ABOVE"
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
simpleLowercaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleLowercaseMapping = simpleCaseMapping SLM.retrieve

-- | A simplified version of 'uppercaseMapping', restricted to
-- non-expanding mappings.
--
-- === __Examples__
--
-- In most cases, simple and full uppercase mappings coincide
--
-- >>> uppercaseMapping 'a'
-- SingleCM (toEnum 0x41)
-- >>> simpleUppercaseMapping 'A'
-- toEnum 0x41
--
-- Sometimes, however, there is no acceptable non-expanding mapping
--
-- >>> name '\xfb03'
-- "LATIN SMALL LIGATURE FFI"
-- >>> uppercaseMapping '\xfb03'
-- TripleCM (toEnum 0x46) (toEnum 0x46) (toEnum 0x49)
-- >>> simpleUppercaseMapping '\xfb03'
-- toEnum 0xfb03
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
simpleUppercaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleUppercaseMapping = simpleCaseMapping SUM.retrieve

-- | A simplified version of 'titlecaseMapping', restricted to
-- non-expanding mappings.
--
-- === __Examples__
--
-- In most cases, simple and full mappings coincide
--
-- >>> titlecaseMapping 'a'
-- SingleCM (toEnum 0x41)
-- >>> simpleTitlecaseMapping 'a'
-- toEnum 0x41
--
-- >>> name '\x01f3'
-- "LATIN SMALL LETTER DZ"
-- >>> titlecaseMapping '\x01f3'
-- SingleCM (toEnum 0x1f2)
-- >>> simpleTitlecaseMapping '\x01f3'
-- toEnum 0x1f2
-- >>> name '\x1f2'
-- "LATIN CAPITAL LETTER D WITH SMALL LETTER Z"
--
-- Sometimes, however, there is no acceptable non-expanding mapping
--
-- >>> name '\xfb03'
-- "LATIN SMALL LIGATURE FFI"
-- >>> titlecaseMapping '\xfb03'
-- TripleCM (toEnum 0x46) (toEnum 0x66) (toEnum 0x69)
-- >>> simpleTitlecaseMapping '\xfb03'
-- toEnum 0xfb03
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
simpleTitlecaseMapping :: IsCodePoint cp => cp -> CodePoint
simpleTitlecaseMapping = simpleCaseMapping STM.retrieve

-- | A simplified version of 'caseFolding', restricted to
-- non-expanding mappings.
--
-- === __Examples__
--
-- For most code points, simple and full folding coincide
--
-- >>> simpleCaseFolding 'a'
-- toEnum 0x61
-- >>> caseFolding 'A'
-- SingleCM (toEnum 0x61)
--
-- Sometimes, however, there is no acceptable non-expanding folding
--
-- >>> simpleCaseFolding '\xdf'
-- toEnum 0xdf
-- >>> caseFolding '\xdf'
-- DoubleCM (toEnum 0x73) (toEnum 0x73)
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
simpleCaseFolding :: IsCodePoint cp => cp -> CodePoint
simpleCaseFolding = simpleCaseMapping SCF.retrieve

simpleCaseMapping :: IsCodePoint cp => (Int -> Int) -> cp -> CodePoint
simpleCaseMapping f = withCP $ \cp -> CodePoint $ fromIntegral $ cp + f cp

-- | The lowercase version of the code point given
--
-- === __Examples__
--
-- Most code points have simple lowercase versions
--
-- >>> lowercaseMapping 'S'
-- SingleCM (toEnum 0x73)
-- >>> '\x73'
-- 's'
--
-- And many aren't changed by casing at all
--
-- >>> lowercaseMapping '\x30'
-- SingleCM (toEnum 0x30)
--
-- But some expand when lowercased
--
-- >>> name '\x130'
-- "LATIN CAPITAL LETTER I WITH DOT ABOVE"
-- >>> lowercaseMapping '\x130'
-- DoubleCM (toEnum 0x69) (toEnum 0x307)
-- >>> name '\x69'
-- "LATIN SMALL LETTER I"
-- >>> name '\x307'
-- "COMBINING DOT ABOVE"
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
lowercaseMapping :: IsCodePoint cp => cp -> CaseMapping
lowercaseMapping cp
  | first == 0 = SingleCM (simpleLowercaseMapping cp)
  | otherwise =
    DoubleCM (CodePoint first) $ CodePoint $ fromIntegral $ SpLM1.retrieve icp
  where
    icp = fromEnum $ toCodePoint cp
    first = fromIntegral $ SpLM0.retrieve icp

-- | The uppercase version of the code point given
--
-- === __Examples__
--
-- Most code points have simple uppercase versions
--
-- >>> uppercaseMapping 'a'
-- SingleCM (toEnum 0x41)
-- >>> '\x41'
-- 'A'
--
-- And many aren't affected by case at all
--
-- >>> uppercaseMapping '\x30'
-- SingleCM (toEnum 0x30)
--
-- But some expand when uppercased
--
-- >>> uppercaseMapping 'ß'
-- DoubleCM (toEnum 0x53) (toEnum 0x53)
-- >>> '\x53'
-- 'S'
--
-- sometimes significantly
--
-- >>> uppercaseMapping 'ﬃ'
-- TripleCM (toEnum 0x46) (toEnum 0x46) (toEnum 0x49)
-- >>> name '\x46'
-- "LATIN CAPITAL LETTER F"
-- >>> name '\x49'
-- "LATIN CAPITAL LETTER I"
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
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

-- | A titlecase version of the code point.
--
-- === __Examples__
--
-- For most code points, titlecase coincides with uppercase
--
-- >>> titlecaseMapping 'a' == uppercaseMapping 'a'
-- True
--
-- It mainly differs in ligatures
--
-- >>> uppercaseMapping 'ǳ'
-- SingleCM (toEnum 0x1f1)
-- >>> name '\x1f1'
-- "LATIN CAPITAL LETTER DZ"
-- >>> titlecaseMapping 'ǳ'
-- SingleCM (toEnum 0x1f2)
-- >>> titlecaseMapping 'Ǳ'
-- SingleCM (toEnum 0x1f2)
-- >>> name '\x1f2'
-- "LATIN CAPITAL LETTER D WITH SMALL LETTER Z"
--
-- Sometimes the titlecase mapping can consist of multiple code points
--
-- >>> titlecaseMapping 'ﬃ'
-- TripleCM (toEnum 0x46) (toEnum 0x66) (toEnum 0x69)
-- >>> "\x46\x66\x69"
-- "Ffi"
--
-- === Property type
--
-- This is an /informative/ property
--
-- @since 0.1.0.0
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

-- | Transform the code point so as to remove all case distinctions
--
-- === __Example__
--
-- In the simplest case, case-folding is equivalent to lowercasing
--
-- >>> caseFolding 'A'
-- SingleCM (toEnum 0x61)
-- >>> caseFolding 'a'
-- SingleCM (toEnum 0x61)
-- >>> '\x61'
-- 'a'
--
-- But things can get more complicated; for example, when the
-- lowercase version of the uppercase version differs from the
-- original code point…
--
-- >>> caseFolding 'ß'
-- DoubleCM (toEnum 0x73) (toEnum 0x73)
-- >>> "\x73\x73"
-- "ss"
--
-- …or multiple lowercase code points have same uppercase version.
--
-- >>> name '\x3c2'
-- "GREEK SMALL LETTER FINAL SIGMA"
-- >>> caseFolding '\x3c2'
-- SingleCM (toEnum 0x3c3)
-- >>> name '\x3c3'
-- "GREEK SMALL LETTER SIGMA"
-- >>> uppercase '\x3c2' == uppercase '\x3c3'
-- True
--
-- While case-folded code points are /usually/ lowercase, this is not
-- always true.
--
-- >>> name '\xab70'
-- "CHEROKEE SMALL LETTER A"
-- >>> caseFolding '\xab70'
-- SingleCM (toEnum 0x13a0)
-- >>> name '\x13a0'
-- "CHEROKEE LETTER A"
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
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

-- | Common return type for various case-related properties
data CaseMapping
  = SingleCM {-# UNPACK #-}!CodePoint
  | DoubleCM {-# UNPACK #-}!CodePoint {-# UNPACK #-}!CodePoint
  | TripleCM
      {-# UNPACK #-}!CodePoint
      {-# UNPACK #-}!CodePoint
      {-# UNPACK #-}!CodePoint
  deriving (Show, Eq)

-- | Classification of code points that represent numbers.
--
-- See examples for details.
--
-- === __Examples__
--
-- Most code points do /not/ represent numbers, and therefore have no
-- useful numeric properties.
--
-- >>> numeric 'A'
-- Nothing
--
-- Decimal digits, when encoded in a continuous range in ascending
-- order, starting at 0, are classified as 'Decimal'; the
-- corresponding value is always from 0 to 9.
--
-- >>> mapM_ print $ map numeric ['0'..'9']
-- Just (Decimal 0)
-- Just (Decimal 1)
-- Just (Decimal 2)
-- Just (Decimal 3)
-- Just (Decimal 4)
-- Just (Decimal 5)
-- Just (Decimal 6)
-- Just (Decimal 7)
-- Just (Decimal 8)
-- Just (Decimal 9)
-- >>> mapM_ print $ map numeric ['०'..'९']
-- Just (Decimal 0)
-- Just (Decimal 1)
-- Just (Decimal 2)
-- Just (Decimal 3)
-- Just (Decimal 4)
-- Just (Decimal 5)
-- Just (Decimal 6)
-- Just (Decimal 7)
-- Just (Decimal 8)
-- Just (Decimal 9)
--
-- Historically, digit-like code points that do not meet all of the
-- criteria above were classified as 'Digit', but such fine
-- categorisation proved to be useless, and will not be honoured in
-- the future.  The values for this category are also between 0 and 9.
--
-- >>> numeric '③'
-- Just (Digit 3)
--
-- Everything else is classified as 'Numeric'.  This is the widest
-- category, including fractional numbers,…
--
-- >>> numeric '½'
-- Just (Numeric (1 % 2))
--
-- …negative numbers,…
--
-- >>> name '\x0f33'
-- "TIBETAN DIGIT HALF ZERO"
-- >>> numeric '\x0f33'
-- Just (Numeric ((-1) % 2))
--
-- … and /very/ large numbers.
--
-- >>> name '\x16b61'
-- "PAHAWH HMONG NUMBER TRILLIONS"
-- >>> numeric '\x16b61'
-- Just (Numeric (1000000000000 % 1))
--
-- === Property type
--
-- This is a combination of two properties, @Numeric_Type@ and
-- @Numeric_Value@, both /normative/.
--
-- @since 0.1.0.0
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

-- | Classification of code points representing numbers and digits.
--
-- See 'numeric' for details and examples.
data Numeric
  = Decimal Word8
  | Digit Word8
  | Numeric (Ratio Int64)
  deriving (Show, Eq)

-- | Type of the code point's (compatibility) decomposition mapping.
--
-- The actual decomposition is defined by applying decomposition
-- mappings /recursively/; this function technically applies only to
-- the first step.  However, it should give a fairly good idea about
-- the exact nature of extra information discarded by compatibility
-- decomposition.
--
-- === __Examples__
--
-- Most code points have no decomposition and, therefore, no
-- decomposition type
--
-- >>> decompositionType 'a'
-- Nothing
--
-- Code points with canonical decomposition are marked appropriately;
-- canonical decomposition doesn't lose any information.
--
-- >>> decompositionType 'ä'
-- Just Canonical
-- >>> canonicalDecomposition 'ä'
-- [toEnum 0x61,toEnum 0x308]
--
-- Compatibility decomposition can remove font distinctions,…
--
-- >>> decompositionType 'ℌ'
-- Just Font
-- >>> compatibilityDecomposition 'ℌ'
-- [toEnum 0x48]
-- >>> '\x48'
-- 'H'
--
-- …no-break status of separators,…
--
-- >>> name '\xa0'
-- "NO-BREAK SPACE"
-- >>> decompositionType '\xa0'
-- Just NoBreak
-- >>> compatibilityDecomposition '\xa0'
-- [toEnum 0x20]
-- >>> name '\x20'
-- "SPACE"
--
-- …form distinctions,…
--
-- >>> name '\xfb54'
-- "ARABIC LETTER BEEH INITIAL FORM"
-- >>> decompositionType '\xfb54'
-- Just Initial
-- >>> map name $ compatibilityDecomposition '\xfb54'
-- ["ARABIC LETTER BEEH"]
--
-- >>> name '\xfb55'
-- "ARABIC LETTER BEEH MEDIAL FORM"
-- >>> decompositionType '\xfb55'
-- Just Medial
-- >>> map name $ compatibilityDecomposition '\xfb55'
-- ["ARABIC LETTER BEEH"]
--
-- >>> name '\xfb56'
-- "ARABIC LETTER PEH ISOLATED FORM"
-- >>> decompositionType '\xfb56'
-- Just Isolated
-- >>> map name $ compatibilityDecomposition '\xfb56'
-- ["ARABIC LETTER PEH"]
--
-- >>> name '\xfb57'
-- "ARABIC LETTER PEH FINAL FORM"
-- >>> decompositionType '\xfb57'
-- Just Final
-- >>> map name $ compatibilityDecomposition '\xfb57'
-- ["ARABIC LETTER PEH"]
--
-- …circles around symbols,…
--
-- >>> decompositionType '②'
-- Just Circle
-- >>> compatibilityDecomposition '②'
-- [toEnum 0x32]
-- >>> '\x32'
-- '2'
--
-- …subscript and superscript information,…
--
-- >>> name '\x2080'
-- "SUBSCRIPT ZERO"
-- >>> decompositionType '\x2080'
-- Just Sub
-- >>> map name $ compatibilityDecomposition '\x2080'
-- ["DIGIT ZERO"]
--
-- >>> name '\xb2'
-- "SUPERSCRIPT TWO"
-- >>> decompositionType '\xb2'
-- Just Super
-- >>> map name $ compatibilityDecomposition '\xb2'
-- ["DIGIT TWO"]
--
-- …vertical presentation forms,…
--
-- >>> name '\xfe10'
-- "PRESENTATION FORM FOR VERTICAL COMMA"
-- >>> decompositionType '\xfe10'
-- Just Vertical
-- >>> map name $ compatibilityDecomposition '\xfe10'
-- ["COMMA"]
--
-- …explicit width,…
--
-- >>> name '\xff10'
-- "FULLWIDTH DIGIT ZERO"
-- >>> decompositionType '\xff10'
-- Just Wide
-- >>> map name $ compatibilityDecomposition '\xff10'
-- ["DIGIT ZERO"]
--
-- >>> name '\xff67'
-- "HALFWIDTH KATAKANA LETTER SMALL A"
-- >>> decompositionType '\xff67'
-- Just Narrow
-- >>> map name $ compatibilityDecomposition '\xff67'
-- ["KATAKANA LETTER SMALL A"]
--
-- …small forms,…
--
-- >>> name '\xfe62'
-- "SMALL PLUS SIGN"
-- >>> decompositionType '\xfe62'
-- Just Small
-- >>> map name $ compatibilityDecomposition '\xfe62'
-- ["PLUS SIGN"]
--
-- …squared (compact) forms,…
--
-- >>> name '\x32cc'
-- "SQUARE HG"
-- >>> decompositionType '\x32cc'
-- Just Square
-- >>> map name $ compatibilityDecomposition '\x32cc'
-- ["LATIN CAPITAL LETTER H","LATIN SMALL LETTER G"]
--
-- …vulgar fraction forms,…
--
-- >>> name '\xbc'
-- "VULGAR FRACTION ONE QUARTER"
-- >>> decompositionType '\xbc'
-- Just Fraction
-- >>> map name $ compatibilityDecomposition '\xbc'
-- ["DIGIT ONE","FRACTION SLASH","DIGIT FOUR"]
--
-- …and other aspects.
--
-- >>> name '\xa8'
-- "DIAERESIS"
-- >>> decompositionType '\xa8'
-- Just Compat
-- >>> map name $ compatibilityDecomposition '\xa8'
-- ["SPACE","COMBINING DIAERESIS"]
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
decompositionType :: IsCodePoint cp => cp -> Maybe DecompositionType
{-# INLINE decompositionType #-}
decompositionType =
  withCP $ \icp ->
    if 0xac00 <= icp && icp <= 0xd7a3
      then Just Canonical
      else DT.retrieve icp

-- | Same as 'canonicalDecomposition' but the trivial decomposition is
-- represented by an empty list.
--
-- For technical reasons, this function is slightly faster that
-- 'canonicalDecomposition', and the resulting list should fuse
-- better.
--
-- === __Examples__
--
-- Trivial decompositions are represented with empty list
--
-- >>> nontrivialCanonicalDecomposition 'a'
-- []
--
-- Nontrivial ones are provided in full
--
-- >>> nontrivialCanonicalDecomposition 'ä'
-- [toEnum 0x61,toEnum 0x308]
--
-- @since 0.1.0.0
nontrivialCanonicalDecomposition :: IsCodePoint cp => cp -> [CodePoint]
{-# INLINE nontrivialCanonicalDecomposition #-}
nontrivialCanonicalDecomposition =
  withCP $ \cp ->
    if 0xAC00 <= cp && cp <= 0xD7A3
      then hangulSyllableDecomposition cp
      else let ptr = CDPtr.retrieve cp
               len = CDLen.retrieve cp
            in map (CodePoint . fromIntegral . unsafeReadPtr ptr) [0 .. len - 1]

-- | Canonical decomposition of a code point.
--
-- The code point and its canonical decomposition represent the same
-- abstract character.
--
-- === __Examples__
--
-- Most code points have trivial canonical decompositions
--
-- >>> canonicalDecomposition '\x61'
-- [toEnum 0x61]
--
-- But in some cases, there are several ways to represent the same
-- character
--
-- >>> name 'ä'
-- "LATIN SMALL LETTER A WITH DIAERESIS"
-- >>> canonicalDecomposition 'ä'
-- [toEnum 0x61,toEnum 0x308]
-- >>> name '\x61'
-- "LATIN SMALL LETTER A"
-- >>> name '\x308'
-- "COMBINING DIAERESIS"
--
-- === Property type
--
-- This is a derivative of @Decomposition_Type@ and
-- @Decomposition_Mapping@ properties, both /normative/.
--
-- @since 0.1.0.0
canonicalDecomposition :: IsCodePoint cp => cp -> [CodePoint]
canonicalDecomposition cp =
  case nontrivialCanonicalDecomposition cp of
    [] -> [toCodePoint cp]
    d -> d

-- | Same as 'compatibilityDecomposition' but the trivial decomposition is
-- represented by an empty list.
--
-- For technical reasons, this function is slightly faster that
-- 'compatibilityDecomposition', and the resulting list should fuse
-- better.
--
-- === __Examples__
--
-- Trivial decompositions are represented with empty list
--
-- >>> nontrivialCompatibilityDecomposition 'a'
-- []
--
-- Nontrivial ones are provided in full
--
-- >>> nontrivialCompatibilityDecomposition '½'
-- [toEnum 0x31,toEnum 0x2044,toEnum 0x32]
--
-- @since 0.1.0.0
nontrivialCompatibilityDecomposition :: IsCodePoint cp => cp -> [CodePoint]
{-# INLINE nontrivialCompatibilityDecomposition #-}
nontrivialCompatibilityDecomposition =
  withCP $ \cp ->
    if 0xAC00 <= cp && cp <= 0xD7A3
      then hangulSyllableDecomposition cp
      else let ptr = KDPtr.retrieve cp
               len = KDLen.retrieve cp
            in map (CodePoint . fromIntegral . unsafeReadPtr ptr) [0 .. len - 1]

-- | Compatibility decomposition of a code point.
--
-- The code point and its compatibility decomposition can differ in
-- some formatting aspects, but should otherwise be roughly equivalent.
--
-- See also 'decompositionType'.
--
-- === __Examples__
--
-- Most code points have trivial compatibility decompositions
--
-- >>> compatibilityDecomposition '\x61'
-- [toEnum 0x61]
--
-- Any code point with a canonical decomposition has a compatibility
-- decomposition as well
--
-- >>> canonicalDecomposition 'ä'
-- [toEnum 0x61,toEnum 0x308]
-- >>> compatibilityDecomposition 'ä'
-- [toEnum 0x61,toEnum 0x308]
--
-- But sometimes compatibility decomposition can discard some details
--
-- >>> compatibilityDecomposition '½'
-- [toEnum 0x31,toEnum 0x2044,toEnum 0x32]
-- >>> putStrLn "\x31\x2044\x32"
-- 1⁄2
--
-- === Property type
--
-- This is a derivative of @Decomposition_Type@ and
-- @Decomposition_Mapping@ properties, both /normative/.
--
-- @since 0.1.0.0
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

-- | A moral inverse of 'canonicalDecomposition'
--
-- === __Examples__
--
-- Most code points do not compose at all
--
-- >>> canonicalComposition 'a' 'b'
-- Nothing
--
-- In some cases, canonical composition is a direct inverse of
-- canonical decomposition
--
-- >>> canonicalDecomposition '\xe4'
-- [toEnum 0x61,toEnum 0x308]
-- >>> canonicalComposition '\x61' '\x308'
-- Just (toEnum 0xe4)
--
-- In some cases, however, it has to be applied multiple times
--
-- >>> name '\x1de'
-- "LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON"
-- >>> canonicalDecomposition '\x1de'
-- [toEnum 0x41,toEnum 0x308,toEnum 0x304]
-- >>> canonicalComposition '\x41' '\x308'
-- Just (toEnum 0xc4)
-- >>> canonicalComposition '\xc4' '\x304'
-- Just (toEnum 0x1de)
--
-- === Property type
--
-- This is a derivative of @Decomposition_Type@ and
-- @Decomposition_Mapping@ properties, both /normative/.
--
-- @since 0.1.0.0
canonicalComposition ::
     (IsCodePoint cp1, IsCodePoint cp2) => cp1 -> cp2 -> Maybe CodePoint
{-# INLINE canonicalComposition #-}
canonicalComposition cp1 cp2 =
  canonicalCompositionStart cp1 >>= flip canonicalCompositionFinish cp2

-- | Look up the composition table associated with the first code
-- point in pair.
--
-- @
--   'canonicalComposition' c1 c2 = 'canonicalCompositionStart' c1 >>= flip 'canonicalCompositionFinish' c2
-- @
--
-- The primary purpose of the function is to cache all of the relevant
-- lookups when attempting to compose with several combining code
-- points, as well as provide a fast exit if no such compositions
-- exist.
--
-- @since 0.1.0.0
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

-- | Find the composition given the token from
-- 'canonicalCompositionStart' and second code point.
--
-- See 'canonicalCompositionStart' for details.
--
-- @since 0.1.0.0
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

-- | Opaque token used by 'canonicalCompositionStart' and
-- 'canonicalCompositionFinish'
data CompositionToken
  = Generic {-# UNPACK #-}!Int
  | HangulL {-# UNPACK #-}!Int
  | HangulLV {-# UNPACK #-}!Int

-- | Can the code point occur in NFD?
--
-- Note that there is additional restriction on ordering, so it is not
-- sufficient to check that all code points satisfy this predicate.
--
-- See [Section 9 ‘Detecting Normalization Forms’ of UAX #15 ‘Unicode Normalization Forms](https://www.unicode.org/reports/tr15/tr15-48.html#Detecting_Normalization_Forms)
-- for details.
--
-- === __Examples__
--
-- Code points with no canonical decomposition can occur in NFD
--
-- >>> nfdQuickCheck 'A'
-- True
--
-- And code points with canonical decomposition can't
--
-- >>> nfdQuickCheck 'Ä'
-- False
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
nfdQuickCheck :: IsCodePoint cp => cp -> Bool
nfdQuickCheck = withCP NFDQC.retrieve

-- | Can the code point occur in NFC?
--
-- [@Just False@]: the code point /can't/ occur in NFC;
--
-- [@Just True@]: the code point /can/ occur, subject to ordering restrictions,
--   and may compose with the following code points, but not previous ones;
--
-- [@Nothing@]: whether the code point can occur in the NFC depends on
--   the context.
--
-- See [Section 9 ‘Detecting Normalization Forms’ of UAX #15 ‘Unicode Normalization Forms](https://www.unicode.org/reports/tr15/tr15-48.html#Detecting_Normalization_Forms)
-- for details.
--
-- === __Examples__
--
-- Most code points can occur in NFC
--
-- >>> nfcQuickCheck 'A'
-- Just True
--
-- If the code point can't be recomposed from its canonical
-- decomposition, it will never occur in NFC
--
-- >>> nfcQuickCheck '\x340'
-- Just False
-- >>> canonicalDecomposition '\x340'
-- [toEnum 0x300]
--
-- >>> name '\x344'
-- "COMBINING GREEK DIALYTIKA TONOS"
-- >>> nfcQuickCheck '\x344'
-- Just False
-- >>> map name $ canonicalDecomposition '\x344'
-- ["COMBINING DIAERESIS","COMBINING ACUTE ACCENT"]
--
-- If the code point could potentially combine with one of the
-- /previous/ code points in a string, its status is context-dependent
--
-- >>> name '\x300'
-- "COMBINING GRAVE ACCENT"
-- >>> nfcQuickCheck '\x300'
-- Nothing
-- >>> canonicalComposition 'A' '\x300'
-- Just (toEnum 0xc0)
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
nfcQuickCheck :: IsCodePoint cp => cp -> Maybe Bool
nfcQuickCheck = withCP NFCQC.retrieve

-- | Can the code point occur in NFKD?
--
-- Note that there is additional restriction on ordering, so it is not
-- sufficient to check that all code points satisfy this predicate.
--
-- See [Section 9 ‘Detecting Normalization Forms’ of UAX #15 ‘Unicode Normalization Forms](https://www.unicode.org/reports/tr15/tr15-48.html#Detecting_Normalization_Forms)
-- for details.
--
-- === __Examples__
--
-- Code points with no compatibility decomposition can occur in NFKD
--
-- >>> nfkdQuickCheck 'A'
-- True
--
-- And code points with compatibility decomposition can't
--
-- >>> nfkdQuickCheck '¼'
-- False
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
nfkdQuickCheck :: IsCodePoint cp => cp -> Bool
nfkdQuickCheck = withCP NFKDQC.retrieve

-- | Can the code point occur in NFKC?
--
-- [@Just False@]: the code point /can't/ occur in NFKC;
--
-- [@Just True@]: the code point /can/ occur, subject to ordering restrictions,
--   and may compose with the following code points, but not previous ones;
--
-- [@Nothing@]: whether the code point can occur in the NFKC depends on
--   the context.
--
-- See [Section 9 ‘Detecting Normalization Forms’ of UAX #15 ‘Unicode Normalization Forms](https://www.unicode.org/reports/tr15/tr15-48.html#Detecting_Normalization_Forms)
-- for details.
--
-- === __Examples__
--
-- Most code points can occur in NFKC
--
-- >>> nfkcQuickCheck 'A'
-- Just True
--
-- If the code point can't be recomposed from its compatibility
-- decomposition, it will never occur in NFKC (note that the
-- composition, in both NFC and NFKC, is canonical)
--
-- >>> nfkcQuickCheck '\xa8'
-- Just False
-- >>> compatibilityDecomposition '\xa8'
-- [toEnum 0x20,toEnum 0x308]
--
-- If the code point could potentially combine with one of the
-- /previous/ code points in a string, its status is context-dependent
--
-- >>> name '\x300'
-- "COMBINING GRAVE ACCENT"
-- >>> nfkcQuickCheck '\x300'
-- Nothing
-- >>> canonicalComposition 'A' '\x300'
-- Just (toEnum 0xc0)
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
nfkcQuickCheck :: IsCodePoint cp => cp -> Maybe Bool
nfkcQuickCheck = withCP NFKCQC.retrieve

-- | A modified form of 'caseFolding' designed for best behaviour when
-- doing caseless matching of strings interpreted as identifiers.
--
-- In addition to case folding, this function also removes
-- 'defaultIgnorableCodePoint's and normalises the mapping to NFKC.
-- Note that the string matching algorithm still needs to normalise
-- the result to NFC, as mappings for different code points may interact.
--
-- For more details, see section 3.13 ‘Default Case Algorithms’ of the
-- Unicode Standard.
--
-- === __Examples__
--
-- Most code points have single-element NFKC case foldings
--
-- >>> nfkcCaseFold 'A'
-- ShortCF (toEnum 0x61)
--
-- But they can also be rather long
--
-- >>> nfkcCaseFold '\xfdfa'
-- LongCF [toEnum 0x635,toEnum 0x644,toEnum 0x649,toEnum 0x20,toEnum 0x627,toEnum 0x644,toEnum 0x644,toEnum 0x647,toEnum 0x20,toEnum 0x639,toEnum 0x644,toEnum 0x64a,toEnum 0x647,toEnum 0x20,toEnum 0x648,toEnum 0x633,toEnum 0x644,toEnum 0x645]
--
-- Or even empty
--
-- >>> nfkcCaseFold '\xad'
-- LongCF []
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
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

-- | See 'nfkcCaseFold'
data NFKCCaseFold
  = ShortCF {-# UNPACK #-}!CodePoint -- ^ Single-element mappings
  | LongCF [CodePoint] -- ^ Mappings with any other number of code
                       -- points
  deriving (Show, Eq)

-- | Code points that are not identical to their 'nfkcCaseFold'
--
-- === __Examples__
--
-- >>> changesWhenNFKCCasefolded 'a'
-- False
-- >>> changesWhenNFKCCasefolded 'A'
-- True
-- >>> changesWhenNFKCCasefolded '¼'
-- True
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
changesWhenNFKCCasefolded :: IsCodePoint cp => cp -> Bool
changesWhenNFKCCasefolded = withCP CWNC.retrieve

-- | The joining behaviour of a code point.
--
-- See section 9.2 ‘Arabic’ of the Unicode Standard for details.
--
-- === __Examples__
--
-- Some letters only join to the character to their right /in visual order/,…
--
-- >>> name '\x0627'
-- "ARABIC LETTER ALEF"
-- >>> joiningType '\x0627'
-- RightJoining
--
-- …only to the left /in visual order/,…
--
-- >>> name '\x10acd'
-- "MANICHAEAN LETTER HETH"
-- >>> joiningType '\x10acd'
-- LeftJoining
--
-- …on both sides,…
--
-- >>> name '\x0628'
-- "ARABIC LETTER BEH"
-- >>> joiningType '\x0628'
-- DualJoining
--
-- …on both sides without changing shape themselves,…
--
-- >>> name '\x0640'
-- "ARABIC TATWEEL"
-- >>> joiningType '\x0640'
-- JoinCausing
--
-- …not at all,…
--
-- >>> joiningType '0'
-- NonJoining
--
-- or should be ignored for determination of joining behaviour
--
-- >>> name '\x0300'
-- "COMBINING GRAVE ACCENT"
-- >>> joiningType '\x0300'
-- Transparent
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
joiningType :: IsCodePoint cp => cp -> JoiningType
joiningType = withCP JT.retrieve

-- | Classification of code points based on the behaviour of their
-- letter skeletons in joining context.
--
-- See section 9.2 ‘Arabic’ of the Unicode Standard for details.
--
-- === __Examples__
--
-- Most code points have no special joining-related behaviour
--
-- >>> joiningGroup 'A'
-- Nothing
--
-- It is limited to a number of scripts, such as Arabic…
--
-- >>> name '\x62c'
-- "ARABIC LETTER JEEM"
-- >>> joiningGroup '\x62c'
-- Just Hah
--
-- …or Manichaean
--
-- >>> name '\x10ac3'
-- "MANICHAEAN LETTER GIMEL"
-- >>> joiningGroup '\x10ac3'
-- Just ManichaeanGimel
--
-- === Property type
--
-- This is a /normative/ property
--
-- @since 0.1.0.0
joiningGroup :: IsCodePoint cp => cp -> Maybe JoiningGroup
joiningGroup = withCP JG.retrieve

-- | Orientation of the code point's character in a document rendered
-- in vertical lines.
--
-- This property is intended as a reasonable default, and can be
-- overridden by higher-level protocols.
--
-- See [UAX #50 ‘Unicode Vertical Text Layout’](https://www.unicode.org/reports/tr50/)
-- for details.
--
-- === __Examples__
--
-- Taking Japanese as an example, kanji and katakana are displayed
-- upright…
--
-- >>> name '\x30a2'
-- "KATAKANA LETTER A"
-- >>> verticalOrientation '\x30a2'
-- Upright
--
-- >>> name '\x65e5'
-- "CJK UNIFIED IDEOGRAPH-65E5"
-- >>> verticalOrientation '\x65e5'
-- Upright
--
-- …while Latin letters are rotated 90°
--
-- >>> verticalOrientation 'a'
-- Rotated
--
-- Some code points require a different glyph in vertical context, but
-- can be, as a fallback, displayed upright…
--
-- >>> name '\x3041'
-- "HIRAGANA LETTER SMALL A"
-- >>> verticalOrientation '\x3041'
-- TransformedUpright
--
-- …or rotated
--
-- >>> name '\x300a'
-- "LEFT DOUBLE ANGLE BRACKET"
-- >>> verticalOrientation '\x300a'
-- TransformedRotated
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
verticalOrientation :: IsCodePoint cp => cp -> VerticalOrientation
verticalOrientation = withCP VO.retrieve

-- | Categorisation of code points for the purpose of detecting
-- word-wrapping opportunities in text.
--
-- Specifically, this property is used in
-- [UAX #14 ‘Unicode Line Breaking Algorithm’](https://www.unicode.org/reports/tr14/),
-- and is probably not useful outside of it.
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
lineBreak :: IsCodePoint cp => cp -> LineBreak
lineBreak = withCP LB.retrieve

-- | Categorisation of code points for the purpose of segmentation of text
-- into grapheme clusters.
--
-- The ‘grapheme cluster’ is an approximation to the notion of
-- user-perceived ‘character’; for details, see
-- [UAX #29 ‘Unicode Text Segmentation’](https://www.unicode.org/reports/tr29/).
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
graphemeClusterBreak :: IsCodePoint cp => cp -> GraphemeClusterBreak
graphemeClusterBreak = withCP GCB.retrieve

-- | Categorisation of code points for the purpose of segmentation of
-- text into sentences.
--
-- See
-- [UAX #29 ‘Unicode Text Segmentation’](https://www.unicode.org/reports/tr29/)
-- for details.
--
-- Keep in mind that segmentation into sentences is inherently
-- ambiguous and convention-dependent; the Unicode can only offer a
-- reasonable default.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
sentenceBreak :: IsCodePoint cp => cp -> SentenceBreak
sentenceBreak = withCP SB.retrieve

-- | Categorisation of code points for the purpose of segmentation of
-- text into words.
--
-- See
-- [UAX #29 ‘Unicode Text Segmentation’](https://www.unicode.org/reports/tr29/)
-- for details.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
wordBreak :: IsCodePoint cp => cp -> WordBreak
wordBreak = withCP WB.retrieve

-- | Categorisation of code points by their usual width in monospaced
-- fonts.
--
-- For full details, see
-- [UAX #11 ‘East Asian Width’](https://unicode.org/reports/tr11/).
--
-- Note that the actual width of the rendered character obviously
-- depends /at least/ on the font; however, to quote the UAX, ‘An
-- important class of fixed-width legacy fonts contains glyphs of just
-- two widths, with the wider glyphs twice as wide as the narrower
-- glyphs.’
--
-- Note, however, that the classification given by this property is
-- /not/ into ‘narrow’ and ‘wide;’ for the details of the actual
-- classification used, see the examples, or, better yet, UAX #11.
--
-- === __Examples__
--
-- Code points corresponding to characters traditionally rendered as
-- /wide/ in East Asian typography are classified as 'WideEAW':
--
-- >>> name '\x1100'
-- "HANGUL CHOSEONG KIYEOK"
-- >>> eastAsianWidth '\x1100'
-- WideEAW
--
-- Some code points are explicitly fullwidth versions of other code
-- points; these are classified as 'FullwidthEAW'…
--
-- >>> name '\x3000'
-- "IDEOGRAPHIC SPACE"
-- >>> eastAsianWidth '\x3000'
-- FullwidthEAW
--
-- …and given corresponding compatibility decomposition:
--
-- >>> decompositionType '\x3000'
-- Just Wide
-- >>> map name $ compatibilityDecomposition '\x3000'
-- ["SPACE"]
--
-- Likewise, code points corresponding to characters traditionally
-- rendered as /narrow/ are classified as 'NarrowEAW':
--
-- >>> eastAsianWidth '0'
-- NarrowEAW
--
-- And similarly, some code points are halfwidth versions of others:
--
-- >>> name '\xff61'
-- "HALFWIDTH IDEOGRAPHIC FULL STOP"
-- >>> eastAsianWidth '\xff61'
-- HalfwidthEAW
-- >>> decompositionType '\xff61'
-- Just Narrow
-- >>> map name $ compatibilityDecomposition '\xff61'
-- ["IDEOGRAPHIC FULL STOP"]
--
-- /plus/ U+20a9, for reasons explained in the UAX #11.
--
-- >>> eastAsianWidth '\x20a9'
-- HalfwidthEAW
-- >>> decompositionType '\x20a9'
-- Nothing
--
-- Some code points correspond to characters that can be rendered as
-- either wide or narrow depending on the context.  This category
-- includes all private use code points, as well as a number of code
-- points corresponding to characters that are classified as /wide/ in
-- legacy East Asian character sets, but are /narrow/ elsewhere.
--
-- >>> name '\xa4'
-- "CURRENCY SIGN"
-- >>> eastAsianWidth '\xa4'
-- AmbiguousEAW
--
-- Finally, a significant number of code points were never used in
-- East Asian typography.  The notion of width thus doesn't really
-- apply to them, but in practice they are typically rendered as narrow.
--
-- >>> name '\xa9'
-- "COPYRIGHT SIGN"
-- >>> eastAsianWidth '\xa9'
-- NeutralEAW
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
eastAsianWidth :: IsCodePoint cp => cp -> EastAsianWidth
eastAsianWidth = withCP EAW.retrieve

-- | Classification of code points for the purposes of
-- [UAX #9 ‘Unicode Bidirectional Algorithm’](https://www.unicode.org/reports/tr9/)
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
bidiClass :: IsCodePoint cp => cp -> BidiClass
bidiClass = withCP BCl.retrieve

-- | A class of code points that represent characters that should be
-- mirrored horizontally in right to left text.
--
-- === __Examples__
--
-- Some examples of characters with this property are brackets of all kinds…
--
-- >>> bidiMirrored '('
-- True
-- >>> bidiMirrored ')'
-- True
-- >>> bidiMirrored '['
-- True
-- >>> bidiMirrored '{'
-- True
--
-- …comparison signs…
--
-- >>> bidiMirrored '<'
-- True
-- >>> bidiMirrored '≤'
-- True
--
-- …some other mathematical notation…
--
-- >>> bidiMirrored '∃'
-- True
-- >>> bidiMirrored '∈'
-- True
--
-- …and a lot more.  But most ordinary characters are not in this category
--
-- >>> bidiMirrored 'A'
-- False
-- >>> bidiMirrored '2'
-- False
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
bidiMirrored :: IsCodePoint cp => cp -> Bool
bidiMirrored = withCP BM.retrieve

-- | For some 'bidiMirrored' code points, there is another code point
-- that is typically rendered with a glyph that is a mirror image of
-- the original.  This function provides them where they exist.
--
-- === __Examples__
--
-- Parentheses and comparison operators are processed predictably
--
-- >>> name <$> bidiMirroringGlyph '('
-- Just "RIGHT PARENTHESIS"
-- >>> name <$> bidiMirroringGlyph '<'
-- Just "GREATER-THAN SIGN"
-- >>> name <$> bidiMirroringGlyph '≥'
-- Just "LESS-THAN OR EQUAL TO"
--
-- But keep in mind that many 'bidiMirrored' code points do /not/ have
-- acceptable mirrored pairs, so the use of this function is not a
-- substitute for proper support for mirroring in the rendering engine:
--
-- >>> bidiMirrored '∃'
-- True
-- >>> bidiMirroringGlyph '∃'
-- Nothing
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
bidiMirroringGlyph :: IsCodePoint cp => cp -> Maybe CodePoint
bidiMirroringGlyph =
  withCP $ \cp ->
    let diff = BMG.retrieve cp
     in if diff == 0
          then Nothing
          else Just $ CodePoint $ fromIntegral $ cp + diff

-- | The code point of the paired bracket.
--
-- This function is used for parenthesis matching in
-- [UAX #9 ‘Unicode Bidirectional Algorithm’](https://www.unicode.org/reports/tr9/).
-- Not to be confused with 'bidiMirroringGlyph'.
--
-- === __Examples__
--
-- >>> name $ bidiPairedBracket '('
-- "RIGHT PARENTHESIS"
-- >>> name $ bidiPairedBracket ')'
-- "LEFT PARENTHESIS"
-- >>> name $ bidiPairedBracket '['
-- "RIGHT SQUARE BRACKET"
--
-- Non-parentheses code points are currently returned unchanged
--
-- >>> bidiPairedBracket '\x20'
-- toEnum 0x20
--
-- ==== __Why?__
--
-- As far as the author is able to tell, the Unicode Character
-- Database and associated Annexes never really specify the default
-- value of this property for code points that are not listed
-- explicitly.  The current semantics are inferred from the use of @#@
-- for them in the XML version of the UCD, which in every /other/
-- context means identity mapping, but is undocumented in this one.
--
-- It's also /slightly/ faster this way.
--
-- === Property type
--
-- This is a /normative/ property.
--
-- @since 0.1.0.0
bidiPairedBracket :: IsCodePoint cp => cp -> CodePoint
bidiPairedBracket =
  withCP $ \cp ->
    let diff = BPB.retrieve cp
     in CodePoint $ fromIntegral $ cp + diff

-- | The type of the parenthesis.
--
-- Used in conjunction with 'bidiPairedBracket'
--
-- === __Examples__
--
-- >>> all (\c -> bidiPairedBracketType c == Just Open) "([{"
-- True
-- >>> all (\c -> bidiPairedBracketType c == Just Close) "}])"
-- True
-- >>> all (\c -> bidiPairedBracketType c == Nothing) "aA0"
-- True
--
-- === Property type
--
-- This is a /normative/ property.
bidiPairedBracketType :: IsCodePoint cp => cp -> Maybe BidiPairedBracketType
bidiPairedBracketType = withCP $ \cp -> BPBT.retrieve cp

-- | A mapping from most CJK radicals and strokes to most reasonably
-- equivalent unified ideograph.
--
-- === __Examples__
--
-- >>> name '\x2e81'
-- "CJK RADICAL CLIFF"
-- >>> equivalentUnifiedIdeograph '\x2e81'
-- Just (toEnum 0x5382)
-- >>> name '\x5382'
-- "CJK UNIFIED IDEOGRAPH-5382"
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
equivalentUnifiedIdeograph :: IsCodePoint cp => cp -> Maybe CodePoint
equivalentUnifiedIdeograph =
  withCP $ fmap (CodePoint . fromIntegral) . EUI.retrieve

-- | The name that the code point had in Unicode 1.0, if it's
-- different from 'name'.
--
-- === __Examples__
--
-- Control codes had names
--
-- >>> name '\x0a'
-- ""
-- >>> unicode1Name '\x0a'
-- "LINE FEED (LF)"
--
-- Some other code points have been renamed as well
--
-- >>> name '\xc0'
-- "LATIN CAPITAL LETTER A WITH GRAVE"
-- >>> unicode1Name '\xc0'
-- "LATIN CAPITAL LETTER A GRAVE"
--
-- >>> name '\''
-- "APOSTROPHE"
-- >>> unicode1Name '\''
-- "APOSTROPHE-QUOTE"
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
unicode1Name :: IsCodePoint cp => cp -> ByteString
unicode1Name =
  withCP $ \cp -> mkByteString (U1NL.retrieve cp) (U1NP.retrieve cp)

-- | The property providing suggested positioning of various dependent
-- characters in Indic scripts.
--
-- See the file @IndicPositionalCategory.txt@ in the Unicode Character
-- Database for details.
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
indicPositionalCategory :: IsCodePoint cp => cp -> Maybe IndicPositionalCategory
indicPositionalCategory = withCP IPC.retrieve

-- | Informal definition of structural categories of syllabic
-- components in Indic scripts.
--
-- See the file @IndicSyllabicCategory.txt@ in the Unicode Character
-- Database for details.
--
-- === __Examples__
--
-- Non-Indic code points are classified as 'Other'
--
-- >>> indicSyllabicCategory 'A'
-- Other
--
-- === Property type
--
-- This is an /informative/ property.
--
-- @since 0.1.0.0
indicSyllabicCategory :: IsCodePoint cp => cp -> IndicSyllabicCategory
indicSyllabicCategory = withCP ISC.retrieve

withCP :: IsCodePoint cp => (Int -> a) -> cp -> a
withCP f = f . fromEnum . toCodePoint
