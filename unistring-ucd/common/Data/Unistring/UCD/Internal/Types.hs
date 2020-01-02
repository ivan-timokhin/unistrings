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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Unistring.UCD.Internal.Types
  ( Age(..)
  , BidiClass(..)
  , HangulSyllableType(..)
  , NameAliasType(..)
  , Block(..)
  , Script(..)
  , EnumeratedProperty(..)
  , DecompositionType(..)
  , JoiningGroup(..)
  , JoiningType(..)
  , VerticalOrientation(..)
  , LineBreak(..)
  , GraphemeClusterBreak(..)
  , SentenceBreak(..)
  , WordBreak(..)
  , EastAsianWidth(..)
  , BidiPairedBracketType(..)
  , IndicPositionalCategory(..)
  , IndicSyllabicCategory(..)
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Char
  ( GeneralCategory(ClosePunctuation, ConnectorPunctuation, Control,
                CurrencySymbol, DashPunctuation, DecimalNumber, EnclosingMark,
                FinalQuote, Format, InitialQuote, LetterNumber, LineSeparator,
                LowercaseLetter, MathSymbol, ModifierLetter, ModifierSymbol,
                NonSpacingMark, NotAssigned, OpenPunctuation, OtherLetter,
                OtherNumber, OtherPunctuation, OtherSymbol, ParagraphSeparator,
                PrivateUse, Space, SpacingCombiningMark, Surrogate,
                TitlecaseLetter, UppercaseLetter)
  )
import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic)

-- | Access to property value aliases, as defined in the
-- @PropertyValueAliases.txt@ file in the Unicode Character Database.
--
-- Specifically, this class offers access to the second and third
-- columns in the file (typically, abbreviated and full name); while
-- certain property values have more name aliases, they are not
-- accessible via this class.
class (Enum p, Bounded p) =>
      EnumeratedProperty p
  where
  -- | The name given in the third column of
  -- @PropertyValueAliases.txt@; typically, a full name of the
  -- property value.
  --
  -- === __Examples__
  --
  -- >>> fullPropertyValueName ArabicPresentationFormsABlock
  -- "Arabic_Presentation_Forms_A"
  --
  -- @since 0.1.0.0
  fullPropertyValueName :: p -> ByteString
  -- | The name given in the second column of
  -- @PropertyValueAliases.txt@; typically, an abbreviated name of the
  -- property value.
  --
  -- === __Examples__
  --
  -- >>> abbreviatedPropertyValueName ArabicPresentationFormsABlock
  -- "Arabic_PF_A"
  --
  -- @since 0.1.0.0
  abbreviatedPropertyValueName :: p -> ByteString

instance EnumeratedProperty GeneralCategory where
  fullPropertyValueName g =
    case g of
      UppercaseLetter -> "Uppercase_Letter"
      LowercaseLetter -> "Lowercase_Letter"
      TitlecaseLetter -> "Titlecase_Letter"
      ModifierLetter -> "Modifier_Letter"
      OtherLetter -> "Other_Letter"
      NonSpacingMark -> "Nonspacing_Mark"
      SpacingCombiningMark -> "Spacing_Mark"
      EnclosingMark -> "Enclosing_Mark"
      DecimalNumber -> "Decimal_Number"
      LetterNumber -> "Letter_Number"
      OtherNumber -> "Other_Number"
      ConnectorPunctuation -> "Connector_Punctuation"
      DashPunctuation -> "Dash_Punctuation"
      OpenPunctuation -> "Open_Punctuation"
      ClosePunctuation -> "Close_Punctuation"
      InitialQuote -> "Initial_Punctuation"
      FinalQuote -> "Final_Punctuation"
      OtherPunctuation -> "Other_Punctuation"
      MathSymbol -> "Math_Symbol"
      CurrencySymbol -> "Currency_Symbol"
      ModifierSymbol -> "Modifier_Symbol"
      OtherSymbol -> "Other_Symbol"
      Space -> "Space_Separator"
      LineSeparator -> "Line_Separator"
      ParagraphSeparator -> "Paragraph_Separator"
      Control -> "Control"
      Format -> "Format"
      Surrogate -> "Surrogate"
      PrivateUse -> "Private_Use"
      NotAssigned -> "Unassigned"
  abbreviatedPropertyValueName g =
    case g of
      UppercaseLetter -> "Lu"
      LowercaseLetter -> "Ll"
      TitlecaseLetter -> "Lt"
      ModifierLetter -> "Lm"
      OtherLetter -> "Lo"
      NonSpacingMark -> "Mn"
      SpacingCombiningMark -> "Mc"
      EnclosingMark -> "Me"
      DecimalNumber -> "Nd"
      LetterNumber -> "Nl"
      OtherNumber -> "No"
      ConnectorPunctuation -> "Pc"
      DashPunctuation -> "Pd"
      OpenPunctuation -> "Ps"
      ClosePunctuation -> "Pe"
      InitialQuote -> "Pi"
      FinalQuote -> "Pf"
      OtherPunctuation -> "Po"
      MathSymbol -> "Sm"
      CurrencySymbol -> "Sc"
      ModifierSymbol -> "Sk"
      OtherSymbol -> "So"
      Space -> "Zs"
      LineSeparator -> "Zl"
      ParagraphSeparator -> "Zp"
      Control -> "Cc"
      Format -> "Cf"
      Surrogate -> "Cs"
      PrivateUse -> "Co"
      NotAssigned -> "Cn"

-- | The Unicode Standard version in which the code point was
-- assigned /or/ reserved.
--
-- See 'Data.Unistring.UCD.age' for more details.
--
-- @since 0.1.0.0
data Age
  = V1_1 -- ^ @since 0.1.0.0
  | V2_0 -- ^ @since 0.1.0.0
  | V2_1 -- ^ @since 0.1.0.0
  | V3_0 -- ^ @since 0.1.0.0
  | V3_1 -- ^ @since 0.1.0.0
  | V3_2 -- ^ @since 0.1.0.0
  | V4_0 -- ^ @since 0.1.0.0
  | V4_1 -- ^ @since 0.1.0.0
  | V5_0 -- ^ @since 0.1.0.0
  | V5_1 -- ^ @since 0.1.0.0
  | V5_2 -- ^ @since 0.1.0.0
  | V6_0 -- ^ @since 0.1.0.0
  | V6_1 -- ^ @since 0.1.0.0
  | V6_2 -- ^ @since 0.1.0.0
  | V6_3 -- ^ @since 0.1.0.0
  | V7_0 -- ^ @since 0.1.0.0
  | V8_0 -- ^ @since 0.1.0.0
  | V9_0 -- ^ @since 0.1.0.0
  | V10_0 -- ^ @since 0.1.0.0
  | V11_0 -- ^ @since 0.1.0.0
  | V12_0 -- ^ @since 0.1.0.0
  | V12_1 -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty Age where
  fullPropertyValueName a =
    case a of
      V1_1 -> "V1_1"
      V2_0 -> "V2_0"
      V2_1 -> "V2_1"
      V3_0 -> "V3_0"
      V3_1 -> "V3_1"
      V3_2 -> "V3_2"
      V4_0 -> "V4_0"
      V4_1 -> "V4_1"
      V5_0 -> "V5_0"
      V5_1 -> "V5_1"
      V5_2 -> "V5_2"
      V6_0 -> "V6_0"
      V6_1 -> "V6_1"
      V6_2 -> "V6_2"
      V6_3 -> "V6_3"
      V7_0 -> "V7_0"
      V8_0 -> "V8_0"
      V9_0 -> "V9_0"
      V10_0 -> "V10_0"
      V11_0 -> "V11_0"
      V12_0 -> "V12_0"
      V12_1 -> "V12_1"
  abbreviatedPropertyValueName a =
    case a of
      V1_1 -> "1.1"
      V2_0 -> "2.0"
      V2_1 -> "2.1"
      V3_0 -> "3.0"
      V3_1 -> "3.1"
      V3_2 -> "3.2"
      V4_0 -> "4.0"
      V4_1 -> "4.1"
      V5_0 -> "5.0"
      V5_1 -> "5.1"
      V5_2 -> "5.2"
      V6_0 -> "6.0"
      V6_1 -> "6.1"
      V6_2 -> "6.2"
      V6_3 -> "6.3"
      V7_0 -> "7.0"
      V8_0 -> "8.0"
      V9_0 -> "9.0"
      V10_0 -> "10.0"
      V11_0 -> "11.0"
      V12_0 -> "12.0"
      V12_1 -> "12.1"

-- | See 'Data.Unistring.UCD.bidiClass'
--
-- @since 0.1.0.0
data BidiClass
  = LeftToRightBC -- ^ @since 0.1.0.0
  | RightToLeftBC -- ^ @since 0.1.0.0
  | ArabicLetterBC -- ^ @since 0.1.0.0
  | EuropeanNumberBC -- ^ @since 0.1.0.0
  | EuropeanSeparatorBC -- ^ @since 0.1.0.0
  | EuropeanTerminatorBC -- ^ @since 0.1.0.0
  | ArabicNumberBC -- ^ @since 0.1.0.0
  | CommonSeparatorBC -- ^ @since 0.1.0.0
  | NonspacingMarkBC -- ^ @since 0.1.0.0
  | BoundaryNeutralBC -- ^ @since 0.1.0.0
  | ParagraphSeparatorBC -- ^ @since 0.1.0.0
  | SegmentSeparatorBC -- ^ @since 0.1.0.0
  | WhiteSpaceBC -- ^ @since 0.1.0.0
  | OtherNeutralBC -- ^ @since 0.1.0.0
  | LeftToRightEmbeddingBC -- ^ @since 0.1.0.0
  | LeftToRightOverrideBC -- ^ @since 0.1.0.0
  | RightToLeftEmbeddingBC -- ^ @since 0.1.0.0
  | RightToLeftOverrideBC -- ^ @since 0.1.0.0
  | PopDirectionalFormatBC -- ^ @since 0.1.0.0
  | LeftToRightIsolateBC -- ^ @since 0.1.0.0
  | RightToLeftIsolateBC -- ^ @since 0.1.0.0
  | FirstStrongIsolateBC -- ^ @since 0.1.0.0
  | PopDirectionalIsolateBC -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty BidiClass where
  fullPropertyValueName b =
    case b of
      ArabicLetterBC -> "Arabic_Letter"
      ArabicNumberBC -> "Arabic_Number"
      ParagraphSeparatorBC -> "Paragraph_Separator"
      BoundaryNeutralBC -> "Boundary_Neutral"
      CommonSeparatorBC -> "Common_Separator"
      EuropeanNumberBC -> "European_Number"
      EuropeanSeparatorBC -> "European_Separator"
      EuropeanTerminatorBC -> "European_Terminator"
      FirstStrongIsolateBC -> "First_Strong_Isolate"
      LeftToRightBC -> "Left_To_Right"
      LeftToRightEmbeddingBC -> "Left_To_Right_Embedding"
      LeftToRightIsolateBC -> "Left_To_Right_Isolate"
      LeftToRightOverrideBC -> "Left_To_Right_Override"
      NonspacingMarkBC -> "Nonspacing_Mark"
      OtherNeutralBC -> "Other_Neutral"
      PopDirectionalFormatBC -> "Pop_Directional_Format"
      PopDirectionalIsolateBC -> "Pop_Directional_Isolate"
      RightToLeftBC -> "Right_To_Left"
      RightToLeftEmbeddingBC -> "Right_To_Left_Embedding"
      RightToLeftIsolateBC -> "Right_To_Left_Isolate"
      RightToLeftOverrideBC -> "Right_To_Left_Override"
      SegmentSeparatorBC -> "Segment_Separator"
      WhiteSpaceBC -> "White_Space"
  abbreviatedPropertyValueName b =
    case b of
      ArabicLetterBC -> "AL"
      ArabicNumberBC -> "AN"
      ParagraphSeparatorBC -> "B"
      BoundaryNeutralBC -> "BN"
      CommonSeparatorBC -> "CS"
      EuropeanNumberBC -> "EN"
      EuropeanSeparatorBC -> "ES"
      EuropeanTerminatorBC -> "ET"
      FirstStrongIsolateBC -> "FSI"
      LeftToRightBC -> "L"
      LeftToRightEmbeddingBC -> "LRE"
      LeftToRightIsolateBC -> "LRI"
      LeftToRightOverrideBC -> "LRO"
      NonspacingMarkBC -> "NSM"
      OtherNeutralBC -> "ON"
      PopDirectionalFormatBC -> "PDF"
      PopDirectionalIsolateBC -> "PDI"
      RightToLeftBC -> "R"
      RightToLeftEmbeddingBC -> "RLE"
      RightToLeftIsolateBC -> "RLI"
      RightToLeftOverrideBC -> "RLO"
      SegmentSeparatorBC -> "S"
      WhiteSpaceBC -> "WS"

-- | Types of Hangul jamo and precomposed syllables.
--
-- See 'Data.Unistring.UCD.hangulSyllableType' for details.
--
-- @since 0.1.0.0
data HangulSyllableType
  = LeadingJamo -- ^ @since 0.1.0.0
  | VowelJamo -- ^ @since 0.1.0.0
  | TrailingJamo -- ^ @since 0.1.0.0
  | LVSyllable -- ^ @since 0.1.0.0
  | LVTSyllable -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty HangulSyllableType where
  fullPropertyValueName h =
    case h of
      LeadingJamo -> "Leading_Jamo"
      VowelJamo -> "Vowel_Jamo"
      TrailingJamo -> "Trailing_Jamo"
      LVSyllable -> "LV_Syllable"
      LVTSyllable -> "LVT_Syllable"
  abbreviatedPropertyValueName h =
    case h of
      LeadingJamo -> "L"
      VowelJamo -> "V"
      TrailingJamo -> "T"
      LVSyllable -> "LV"
      LVTSyllable -> "LVT"

-- | Type (or purpose) of the given name alias.
--
-- For more details, see 'Data.Unistring.UCD.nameAliases'
--
-- @since 0.1.0.0
data NameAliasType
  = CorrectionAlias
  -- ^ Corrections for serious problems in code point names
  --
  -- @since 0.1.0.0
  | ControlAlias
  -- ^ ISO 6429 names for C0 and C1 control functions, and other
  -- commonly occurring names for control codes
  --
  -- @since 0.1.0.0
  | AlternateAlias
  -- ^ Widely used alternate names for format characters
  --
  -- @since 0.1.0.0
  | FigmentAlias
  -- ^ Several documented labels for C1 control code points which were
  -- never actually approved in any standard
  --
  -- @since 0.1.0.0
  | AbbreviationAlias
  -- ^ Commonly occurring abbreviations (or acronyms) for control
  -- codes, format characters, spaces, and variation selectors
  --
  -- @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

-- | A Unicode allocation block.
--
-- For more details, see 'Data.Unistring.UCD.block'
--
-- @since 0.1.0.0
data Block
  = BasicLatinBlock -- ^ @since 0.1.0.0
  | Latin1SupplementBlock -- ^ @since 0.1.0.0
  | LatinExtendedABlock -- ^ @since 0.1.0.0
  | LatinExtendedBBlock -- ^ @since 0.1.0.0
  | IPAExtensionsBlock -- ^ @since 0.1.0.0
  | SpacingModifierLettersBlock -- ^ @since 0.1.0.0
  | CombiningDiacriticalMarksBlock -- ^ @since 0.1.0.0
  | GreekAndCopticBlock -- ^ @since 0.1.0.0
  | CyrillicBlock -- ^ @since 0.1.0.0
  | CyrillicSupplementBlock -- ^ @since 0.1.0.0
  | ArmenianBlock -- ^ @since 0.1.0.0
  | HebrewBlock -- ^ @since 0.1.0.0
  | ArabicBlock -- ^ @since 0.1.0.0
  | SyriacBlock -- ^ @since 0.1.0.0
  | ArabicSupplementBlock -- ^ @since 0.1.0.0
  | ThaanaBlock -- ^ @since 0.1.0.0
  | NKoBlock -- ^ @since 0.1.0.0
  | SamaritanBlock -- ^ @since 0.1.0.0
  | MandaicBlock -- ^ @since 0.1.0.0
  | SyriacSupplementBlock -- ^ @since 0.1.0.0
  | ArabicExtendedABlock -- ^ @since 0.1.0.0
  | DevanagariBlock -- ^ @since 0.1.0.0
  | BengaliBlock -- ^ @since 0.1.0.0
  | GurmukhiBlock -- ^ @since 0.1.0.0
  | GujaratiBlock -- ^ @since 0.1.0.0
  | OriyaBlock -- ^ @since 0.1.0.0
  | TamilBlock -- ^ @since 0.1.0.0
  | TeluguBlock -- ^ @since 0.1.0.0
  | KannadaBlock -- ^ @since 0.1.0.0
  | MalayalamBlock -- ^ @since 0.1.0.0
  | SinhalaBlock -- ^ @since 0.1.0.0
  | ThaiBlock -- ^ @since 0.1.0.0
  | LaoBlock -- ^ @since 0.1.0.0
  | TibetanBlock -- ^ @since 0.1.0.0
  | MyanmarBlock -- ^ @since 0.1.0.0
  | GeorgianBlock -- ^ @since 0.1.0.0
  | HangulJamoBlock -- ^ @since 0.1.0.0
  | EthiopicBlock -- ^ @since 0.1.0.0
  | EthiopicSupplementBlock -- ^ @since 0.1.0.0
  | CherokeeBlock -- ^ @since 0.1.0.0
  | UnifiedCanadianAboriginalSyllabicsBlock -- ^ @since 0.1.0.0
  | OghamBlock -- ^ @since 0.1.0.0
  | RunicBlock -- ^ @since 0.1.0.0
  | TagalogBlock -- ^ @since 0.1.0.0
  | HanunooBlock -- ^ @since 0.1.0.0
  | BuhidBlock -- ^ @since 0.1.0.0
  | TagbanwaBlock -- ^ @since 0.1.0.0
  | KhmerBlock -- ^ @since 0.1.0.0
  | MongolianBlock -- ^ @since 0.1.0.0
  | UnifiedCanadianAboriginalSyllabicsExtendedBlock -- ^ @since 0.1.0.0
  | LimbuBlock -- ^ @since 0.1.0.0
  | TaiLeBlock -- ^ @since 0.1.0.0
  | NewTaiLueBlock -- ^ @since 0.1.0.0
  | KhmerSymbolsBlock -- ^ @since 0.1.0.0
  | BugineseBlock -- ^ @since 0.1.0.0
  | TaiThamBlock -- ^ @since 0.1.0.0
  | CombiningDiacriticalMarksExtendedBlock -- ^ @since 0.1.0.0
  | BalineseBlock -- ^ @since 0.1.0.0
  | SundaneseBlock -- ^ @since 0.1.0.0
  | BatakBlock -- ^ @since 0.1.0.0
  | LepchaBlock -- ^ @since 0.1.0.0
  | OlChikiBlock -- ^ @since 0.1.0.0
  | CyrillicExtendedCBlock -- ^ @since 0.1.0.0
  | GeorgianExtendedBlock -- ^ @since 0.1.0.0
  | SundaneseSupplementBlock -- ^ @since 0.1.0.0
  | VedicExtensionsBlock -- ^ @since 0.1.0.0
  | PhoneticExtensionsBlock -- ^ @since 0.1.0.0
  | PhoneticExtensionsSupplementBlock -- ^ @since 0.1.0.0
  | CombiningDiacriticalMarksSupplementBlock -- ^ @since 0.1.0.0
  | LatinExtendedAdditionalBlock -- ^ @since 0.1.0.0
  | GreekExtendedBlock -- ^ @since 0.1.0.0
  | GeneralPunctuationBlock -- ^ @since 0.1.0.0
  | SuperscriptsAndSubscriptsBlock -- ^ @since 0.1.0.0
  | CurrencySymbolsBlock -- ^ @since 0.1.0.0
  | CombiningDiacriticalMarksForSymbolsBlock -- ^ @since 0.1.0.0
  | LetterlikeSymbolsBlock -- ^ @since 0.1.0.0
  | NumberFormsBlock -- ^ @since 0.1.0.0
  | ArrowsBlock -- ^ @since 0.1.0.0
  | MathematicalOperatorsBlock -- ^ @since 0.1.0.0
  | MiscellaneousTechnicalBlock -- ^ @since 0.1.0.0
  | ControlPicturesBlock -- ^ @since 0.1.0.0
  | OpticalCharacterRecognitionBlock -- ^ @since 0.1.0.0
  | EnclosedAlphanumericsBlock -- ^ @since 0.1.0.0
  | BoxDrawingBlock -- ^ @since 0.1.0.0
  | BlockElementsBlock -- ^ @since 0.1.0.0
  | GeometricShapesBlock -- ^ @since 0.1.0.0
  | MiscellaneousSymbolsBlock -- ^ @since 0.1.0.0
  | DingbatsBlock -- ^ @since 0.1.0.0
  | MiscellaneousMathematicalSymbolsABlock -- ^ @since 0.1.0.0
  | SupplementalArrowsABlock -- ^ @since 0.1.0.0
  | BraillePatternsBlock -- ^ @since 0.1.0.0
  | SupplementalArrowsBBlock -- ^ @since 0.1.0.0
  | MiscellaneousMathematicalSymbolsBBlock -- ^ @since 0.1.0.0
  | SupplementalMathematicalOperatorsBlock -- ^ @since 0.1.0.0
  | MiscellaneousSymbolsAndArrowsBlock -- ^ @since 0.1.0.0
  | GlagoliticBlock -- ^ @since 0.1.0.0
  | LatinExtendedCBlock -- ^ @since 0.1.0.0
  | CopticBlock -- ^ @since 0.1.0.0
  | GeorgianSupplementBlock -- ^ @since 0.1.0.0
  | TifinaghBlock -- ^ @since 0.1.0.0
  | EthiopicExtendedBlock -- ^ @since 0.1.0.0
  | CyrillicExtendedABlock -- ^ @since 0.1.0.0
  | SupplementalPunctuationBlock -- ^ @since 0.1.0.0
  | CJKRadicalsSupplementBlock -- ^ @since 0.1.0.0
  | KangxiRadicalsBlock -- ^ @since 0.1.0.0
  | IdeographicDescriptionCharactersBlock -- ^ @since 0.1.0.0
  | CJKSymbolsAndPunctuationBlock -- ^ @since 0.1.0.0
  | HiraganaBlock -- ^ @since 0.1.0.0
  | KatakanaBlock -- ^ @since 0.1.0.0
  | BopomofoBlock -- ^ @since 0.1.0.0
  | HangulCompatibilityJamoBlock -- ^ @since 0.1.0.0
  | KanbunBlock -- ^ @since 0.1.0.0
  | BopomofoExtendedBlock -- ^ @since 0.1.0.0
  | CJKStrokesBlock -- ^ @since 0.1.0.0
  | KatakanaPhoneticExtensionsBlock -- ^ @since 0.1.0.0
  | EnclosedCJKLettersAndMonthsBlock -- ^ @since 0.1.0.0
  | CJKCompatibilityBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionABlock -- ^ @since 0.1.0.0
  | YijingHexagramSymbolsBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsBlock -- ^ @since 0.1.0.0
  | YiSyllablesBlock -- ^ @since 0.1.0.0
  | YiRadicalsBlock -- ^ @since 0.1.0.0
  | LisuBlock -- ^ @since 0.1.0.0
  | VaiBlock -- ^ @since 0.1.0.0
  | CyrillicExtendedBBlock -- ^ @since 0.1.0.0
  | BamumBlock -- ^ @since 0.1.0.0
  | ModifierToneLettersBlock -- ^ @since 0.1.0.0
  | LatinExtendedDBlock -- ^ @since 0.1.0.0
  | SylotiNagriBlock -- ^ @since 0.1.0.0
  | CommonIndicNumberFormsBlock -- ^ @since 0.1.0.0
  | PhagsPaBlock -- ^ @since 0.1.0.0
  | SaurashtraBlock -- ^ @since 0.1.0.0
  | DevanagariExtendedBlock -- ^ @since 0.1.0.0
  | KayahLiBlock -- ^ @since 0.1.0.0
  | RejangBlock -- ^ @since 0.1.0.0
  | HangulJamoExtendedABlock -- ^ @since 0.1.0.0
  | JavaneseBlock -- ^ @since 0.1.0.0
  | MyanmarExtendedBBlock -- ^ @since 0.1.0.0
  | ChamBlock -- ^ @since 0.1.0.0
  | MyanmarExtendedABlock -- ^ @since 0.1.0.0
  | TaiVietBlock -- ^ @since 0.1.0.0
  | MeeteiMayekExtensionsBlock -- ^ @since 0.1.0.0
  | EthiopicExtendedABlock -- ^ @since 0.1.0.0
  | LatinExtendedEBlock -- ^ @since 0.1.0.0
  | CherokeeSupplementBlock -- ^ @since 0.1.0.0
  | MeeteiMayekBlock -- ^ @since 0.1.0.0
  | HangulSyllablesBlock -- ^ @since 0.1.0.0
  | HangulJamoExtendedBBlock -- ^ @since 0.1.0.0
  | HighSurrogatesBlock -- ^ @since 0.1.0.0
  | HighPrivateUseSurrogatesBlock -- ^ @since 0.1.0.0
  | LowSurrogatesBlock -- ^ @since 0.1.0.0
  | PrivateUseAreaBlock -- ^ @since 0.1.0.0
  | CJKCompatibilityIdeographsBlock -- ^ @since 0.1.0.0
  | AlphabeticPresentationFormsBlock -- ^ @since 0.1.0.0
  | ArabicPresentationFormsABlock -- ^ @since 0.1.0.0
  | VariationSelectorsBlock -- ^ @since 0.1.0.0
  | VerticalFormsBlock -- ^ @since 0.1.0.0
  | CombiningHalfMarksBlock -- ^ @since 0.1.0.0
  | CJKCompatibilityFormsBlock -- ^ @since 0.1.0.0
  | SmallFormVariantsBlock -- ^ @since 0.1.0.0
  | ArabicPresentationFormsBBlock -- ^ @since 0.1.0.0
  | HalfwidthAndFullwidthFormsBlock -- ^ @since 0.1.0.0
  | SpecialsBlock -- ^ @since 0.1.0.0
  | LinearBSyllabaryBlock -- ^ @since 0.1.0.0
  | LinearBIdeogramsBlock -- ^ @since 0.1.0.0
  | AegeanNumbersBlock -- ^ @since 0.1.0.0
  | AncientGreekNumbersBlock -- ^ @since 0.1.0.0
  | AncientSymbolsBlock -- ^ @since 0.1.0.0
  | PhaistosDiscBlock -- ^ @since 0.1.0.0
  | LycianBlock -- ^ @since 0.1.0.0
  | CarianBlock -- ^ @since 0.1.0.0
  | CopticEpactNumbersBlock -- ^ @since 0.1.0.0
  | OldItalicBlock -- ^ @since 0.1.0.0
  | GothicBlock -- ^ @since 0.1.0.0
  | OldPermicBlock -- ^ @since 0.1.0.0
  | UgariticBlock -- ^ @since 0.1.0.0
  | OldPersianBlock -- ^ @since 0.1.0.0
  | DeseretBlock -- ^ @since 0.1.0.0
  | ShavianBlock -- ^ @since 0.1.0.0
  | OsmanyaBlock -- ^ @since 0.1.0.0
  | OsageBlock -- ^ @since 0.1.0.0
  | ElbasanBlock -- ^ @since 0.1.0.0
  | CaucasianAlbanianBlock -- ^ @since 0.1.0.0
  | LinearABlock -- ^ @since 0.1.0.0
  | CypriotSyllabaryBlock -- ^ @since 0.1.0.0
  | ImperialAramaicBlock -- ^ @since 0.1.0.0
  | PalmyreneBlock -- ^ @since 0.1.0.0
  | NabataeanBlock -- ^ @since 0.1.0.0
  | HatranBlock -- ^ @since 0.1.0.0
  | PhoenicianBlock -- ^ @since 0.1.0.0
  | LydianBlock -- ^ @since 0.1.0.0
  | MeroiticHieroglyphsBlock -- ^ @since 0.1.0.0
  | MeroiticCursiveBlock -- ^ @since 0.1.0.0
  | KharoshthiBlock -- ^ @since 0.1.0.0
  | OldSouthArabianBlock -- ^ @since 0.1.0.0
  | OldNorthArabianBlock -- ^ @since 0.1.0.0
  | ManichaeanBlock -- ^ @since 0.1.0.0
  | AvestanBlock -- ^ @since 0.1.0.0
  | InscriptionalParthianBlock -- ^ @since 0.1.0.0
  | InscriptionalPahlaviBlock -- ^ @since 0.1.0.0
  | PsalterPahlaviBlock -- ^ @since 0.1.0.0
  | OldTurkicBlock -- ^ @since 0.1.0.0
  | OldHungarianBlock -- ^ @since 0.1.0.0
  | HanifiRohingyaBlock -- ^ @since 0.1.0.0
  | RumiNumeralSymbolsBlock -- ^ @since 0.1.0.0
  | OldSogdianBlock -- ^ @since 0.1.0.0
  | SogdianBlock -- ^ @since 0.1.0.0
  | ElymaicBlock -- ^ @since 0.1.0.0
  | BrahmiBlock -- ^ @since 0.1.0.0
  | KaithiBlock -- ^ @since 0.1.0.0
  | SoraSompengBlock -- ^ @since 0.1.0.0
  | ChakmaBlock -- ^ @since 0.1.0.0
  | MahajaniBlock -- ^ @since 0.1.0.0
  | SharadaBlock -- ^ @since 0.1.0.0
  | SinhalaArchaicNumbersBlock -- ^ @since 0.1.0.0
  | KhojkiBlock -- ^ @since 0.1.0.0
  | MultaniBlock -- ^ @since 0.1.0.0
  | KhudawadiBlock -- ^ @since 0.1.0.0
  | GranthaBlock -- ^ @since 0.1.0.0
  | NewaBlock -- ^ @since 0.1.0.0
  | TirhutaBlock -- ^ @since 0.1.0.0
  | SiddhamBlock -- ^ @since 0.1.0.0
  | ModiBlock -- ^ @since 0.1.0.0
  | MongolianSupplementBlock -- ^ @since 0.1.0.0
  | TakriBlock -- ^ @since 0.1.0.0
  | AhomBlock -- ^ @since 0.1.0.0
  | DograBlock -- ^ @since 0.1.0.0
  | WarangCitiBlock -- ^ @since 0.1.0.0
  | NandinagariBlock -- ^ @since 0.1.0.0
  | ZanabazarSquareBlock -- ^ @since 0.1.0.0
  | SoyomboBlock -- ^ @since 0.1.0.0
  | PauCinHauBlock -- ^ @since 0.1.0.0
  | BhaiksukiBlock -- ^ @since 0.1.0.0
  | MarchenBlock -- ^ @since 0.1.0.0
  | MasaramGondiBlock -- ^ @since 0.1.0.0
  | GunjalaGondiBlock -- ^ @since 0.1.0.0
  | MakasarBlock -- ^ @since 0.1.0.0
  | TamilSupplementBlock -- ^ @since 0.1.0.0
  | CuneiformBlock -- ^ @since 0.1.0.0
  | CuneiformNumbersAndPunctuationBlock -- ^ @since 0.1.0.0
  | EarlyDynasticCuneiformBlock -- ^ @since 0.1.0.0
  | EgyptianHieroglyphsBlock -- ^ @since 0.1.0.0
  | EgyptianHieroglyphFormatControlsBlock -- ^ @since 0.1.0.0
  | AnatolianHieroglyphsBlock -- ^ @since 0.1.0.0
  | BamumSupplementBlock -- ^ @since 0.1.0.0
  | MroBlock -- ^ @since 0.1.0.0
  | BassaVahBlock -- ^ @since 0.1.0.0
  | PahawhHmongBlock -- ^ @since 0.1.0.0
  | MedefaidrinBlock -- ^ @since 0.1.0.0
  | MiaoBlock -- ^ @since 0.1.0.0
  | IdeographicSymbolsAndPunctuationBlock -- ^ @since 0.1.0.0
  | TangutBlock -- ^ @since 0.1.0.0
  | TangutComponentsBlock -- ^ @since 0.1.0.0
  | KanaSupplementBlock -- ^ @since 0.1.0.0
  | KanaExtendedABlock -- ^ @since 0.1.0.0
  | SmallKanaExtensionBlock -- ^ @since 0.1.0.0
  | NushuBlock -- ^ @since 0.1.0.0
  | DuployanBlock -- ^ @since 0.1.0.0
  | ShorthandFormatControlsBlock -- ^ @since 0.1.0.0
  | ByzantineMusicalSymbolsBlock -- ^ @since 0.1.0.0
  | MusicalSymbolsBlock -- ^ @since 0.1.0.0
  | AncientGreekMusicalNotationBlock -- ^ @since 0.1.0.0
  | MayanNumeralsBlock -- ^ @since 0.1.0.0
  | TaiXuanJingSymbolsBlock -- ^ @since 0.1.0.0
  | CountingRodNumeralsBlock -- ^ @since 0.1.0.0
  | MathematicalAlphanumericSymbolsBlock -- ^ @since 0.1.0.0
  | SuttonSignWritingBlock -- ^ @since 0.1.0.0
  | GlagoliticSupplementBlock -- ^ @since 0.1.0.0
  | NyiakengPuachueHmongBlock -- ^ @since 0.1.0.0
  | WanchoBlock -- ^ @since 0.1.0.0
  | MendeKikakuiBlock -- ^ @since 0.1.0.0
  | AdlamBlock -- ^ @since 0.1.0.0
  | IndicSiyaqNumbersBlock -- ^ @since 0.1.0.0
  | OttomanSiyaqNumbersBlock -- ^ @since 0.1.0.0
  | ArabicMathematicalAlphabeticSymbolsBlock -- ^ @since 0.1.0.0
  | MahjongTilesBlock -- ^ @since 0.1.0.0
  | DominoTilesBlock -- ^ @since 0.1.0.0
  | PlayingCardsBlock -- ^ @since 0.1.0.0
  | EnclosedAlphanumericSupplementBlock -- ^ @since 0.1.0.0
  | EnclosedIdeographicSupplementBlock -- ^ @since 0.1.0.0
  | MiscellaneousSymbolsAndPictographsBlock -- ^ @since 0.1.0.0
  | EmoticonsBlock -- ^ @since 0.1.0.0
  | OrnamentalDingbatsBlock -- ^ @since 0.1.0.0
  | TransportAndMapSymbolsBlock -- ^ @since 0.1.0.0
  | AlchemicalSymbolsBlock -- ^ @since 0.1.0.0
  | GeometricShapesExtendedBlock -- ^ @since 0.1.0.0
  | SupplementalArrowsCBlock -- ^ @since 0.1.0.0
  | SupplementalSymbolsAndPictographsBlock -- ^ @since 0.1.0.0
  | ChessSymbolsBlock -- ^ @since 0.1.0.0
  | SymbolsAndPictographsExtendedABlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionBBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionCBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionDBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionEBlock -- ^ @since 0.1.0.0
  | CJKUnifiedIdeographsExtensionFBlock -- ^ @since 0.1.0.0
  | CJKCompatibilityIdeographsSupplementBlock -- ^ @since 0.1.0.0
  | TagsBlock -- ^ @since 0.1.0.0
  | VariationSelectorsSupplementBlock -- ^ @since 0.1.0.0
  | SupplementaryPrivateUseAreaABlock -- ^ @since 0.1.0.0
  | SupplementaryPrivateUseAreaBBlock -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty Block where
  fullPropertyValueName b =
    case b of
      AdlamBlock -> "Adlam"
      AegeanNumbersBlock -> "Aegean_Numbers"
      AhomBlock -> "Ahom"
      AlchemicalSymbolsBlock -> "Alchemical_Symbols"
      AlphabeticPresentationFormsBlock -> "Alphabetic_Presentation_Forms"
      AnatolianHieroglyphsBlock -> "Anatolian_Hieroglyphs"
      AncientGreekMusicalNotationBlock -> "Ancient_Greek_Musical_Notation"
      AncientGreekNumbersBlock -> "Ancient_Greek_Numbers"
      AncientSymbolsBlock -> "Ancient_Symbols"
      ArabicBlock -> "Arabic"
      ArabicExtendedABlock -> "Arabic_Extended_A"
      ArabicMathematicalAlphabeticSymbolsBlock ->
        "Arabic_Mathematical_Alphabetic_Symbols"
      ArabicPresentationFormsABlock -> "Arabic_Presentation_Forms_A"
      ArabicPresentationFormsBBlock -> "Arabic_Presentation_Forms_B"
      ArabicSupplementBlock -> "Arabic_Supplement"
      ArmenianBlock -> "Armenian"
      ArrowsBlock -> "Arrows"
      AvestanBlock -> "Avestan"
      BalineseBlock -> "Balinese"
      BamumBlock -> "Bamum"
      BamumSupplementBlock -> "Bamum_Supplement"
      BasicLatinBlock -> "Basic_Latin"
      BassaVahBlock -> "Bassa_Vah"
      BatakBlock -> "Batak"
      BengaliBlock -> "Bengali"
      BhaiksukiBlock -> "Bhaiksuki"
      BlockElementsBlock -> "Block_Elements"
      BopomofoBlock -> "Bopomofo"
      BopomofoExtendedBlock -> "Bopomofo_Extended"
      BoxDrawingBlock -> "Box_Drawing"
      BrahmiBlock -> "Brahmi"
      BraillePatternsBlock -> "Braille_Patterns"
      BugineseBlock -> "Buginese"
      BuhidBlock -> "Buhid"
      ByzantineMusicalSymbolsBlock -> "Byzantine_Musical_Symbols"
      CJKCompatibilityBlock -> "CJK_Compatibility"
      CJKCompatibilityFormsBlock -> "CJK_Compatibility_Forms"
      CJKCompatibilityIdeographsBlock -> "CJK_Compatibility_Ideographs"
      CJKCompatibilityIdeographsSupplementBlock ->
        "CJK_Compatibility_Ideographs_Supplement"
      CJKRadicalsSupplementBlock -> "CJK_Radicals_Supplement"
      CJKStrokesBlock -> "CJK_Strokes"
      CJKSymbolsAndPunctuationBlock -> "CJK_Symbols_And_Punctuation"
      CJKUnifiedIdeographsBlock -> "CJK_Unified_Ideographs"
      CJKUnifiedIdeographsExtensionABlock ->
        "CJK_Unified_Ideographs_Extension_A"
      CJKUnifiedIdeographsExtensionBBlock ->
        "CJK_Unified_Ideographs_Extension_B"
      CJKUnifiedIdeographsExtensionCBlock ->
        "CJK_Unified_Ideographs_Extension_C"
      CJKUnifiedIdeographsExtensionDBlock ->
        "CJK_Unified_Ideographs_Extension_D"
      CJKUnifiedIdeographsExtensionEBlock ->
        "CJK_Unified_Ideographs_Extension_E"
      CJKUnifiedIdeographsExtensionFBlock ->
        "CJK_Unified_Ideographs_Extension_F"
      CarianBlock -> "Carian"
      CaucasianAlbanianBlock -> "Caucasian_Albanian"
      ChakmaBlock -> "Chakma"
      ChamBlock -> "Cham"
      CherokeeBlock -> "Cherokee"
      CherokeeSupplementBlock -> "Cherokee_Supplement"
      ChessSymbolsBlock -> "Chess_Symbols"
      CombiningDiacriticalMarksBlock -> "Combining_Diacritical_Marks"
      CombiningDiacriticalMarksExtendedBlock ->
        "Combining_Diacritical_Marks_Extended"
      CombiningDiacriticalMarksSupplementBlock ->
        "Combining_Diacritical_Marks_Supplement"
      CombiningDiacriticalMarksForSymbolsBlock ->
        "Combining_Diacritical_Marks_For_Symbols"
      CombiningHalfMarksBlock -> "Combining_Half_Marks"
      CommonIndicNumberFormsBlock -> "Common_Indic_Number_Forms"
      ControlPicturesBlock -> "Control_Pictures"
      CopticBlock -> "Coptic"
      CopticEpactNumbersBlock -> "Coptic_Epact_Numbers"
      CountingRodNumeralsBlock -> "Counting_Rod_Numerals"
      CuneiformBlock -> "Cuneiform"
      CuneiformNumbersAndPunctuationBlock -> "Cuneiform_Numbers_And_Punctuation"
      CurrencySymbolsBlock -> "Currency_Symbols"
      CypriotSyllabaryBlock -> "Cypriot_Syllabary"
      CyrillicBlock -> "Cyrillic"
      CyrillicExtendedABlock -> "Cyrillic_Extended_A"
      CyrillicExtendedBBlock -> "Cyrillic_Extended_B"
      CyrillicExtendedCBlock -> "Cyrillic_Extended_C"
      CyrillicSupplementBlock -> "Cyrillic_Supplement"
      DeseretBlock -> "Deseret"
      DevanagariBlock -> "Devanagari"
      DevanagariExtendedBlock -> "Devanagari_Extended"
      DingbatsBlock -> "Dingbats"
      DograBlock -> "Dogra"
      DominoTilesBlock -> "Domino_Tiles"
      DuployanBlock -> "Duployan"
      EarlyDynasticCuneiformBlock -> "Early_Dynastic_Cuneiform"
      EgyptianHieroglyphFormatControlsBlock ->
        "Egyptian_Hieroglyph_Format_Controls"
      EgyptianHieroglyphsBlock -> "Egyptian_Hieroglyphs"
      ElbasanBlock -> "Elbasan"
      ElymaicBlock -> "Elymaic"
      EmoticonsBlock -> "Emoticons"
      EnclosedAlphanumericSupplementBlock -> "Enclosed_Alphanumeric_Supplement"
      EnclosedAlphanumericsBlock -> "Enclosed_Alphanumerics"
      EnclosedCJKLettersAndMonthsBlock -> "Enclosed_CJK_Letters_And_Months"
      EnclosedIdeographicSupplementBlock -> "Enclosed_Ideographic_Supplement"
      EthiopicBlock -> "Ethiopic"
      EthiopicExtendedBlock -> "Ethiopic_Extended"
      EthiopicExtendedABlock -> "Ethiopic_Extended_A"
      EthiopicSupplementBlock -> "Ethiopic_Supplement"
      GeneralPunctuationBlock -> "General_Punctuation"
      GeometricShapesBlock -> "Geometric_Shapes"
      GeometricShapesExtendedBlock -> "Geometric_Shapes_Extended"
      GeorgianBlock -> "Georgian"
      GeorgianExtendedBlock -> "Georgian_Extended"
      GeorgianSupplementBlock -> "Georgian_Supplement"
      GlagoliticBlock -> "Glagolitic"
      GlagoliticSupplementBlock -> "Glagolitic_Supplement"
      GothicBlock -> "Gothic"
      GranthaBlock -> "Grantha"
      GreekAndCopticBlock -> "Greek_And_Coptic"
      GreekExtendedBlock -> "Greek_Extended"
      GujaratiBlock -> "Gujarati"
      GunjalaGondiBlock -> "Gunjala_Gondi"
      GurmukhiBlock -> "Gurmukhi"
      HalfwidthAndFullwidthFormsBlock -> "Halfwidth_And_Fullwidth_Forms"
      HangulCompatibilityJamoBlock -> "Hangul_Compatibility_Jamo"
      HangulJamoBlock -> "Hangul_Jamo"
      HangulJamoExtendedABlock -> "Hangul_Jamo_Extended_A"
      HangulJamoExtendedBBlock -> "Hangul_Jamo_Extended_B"
      HangulSyllablesBlock -> "Hangul_Syllables"
      HanifiRohingyaBlock -> "Hanifi_Rohingya"
      HanunooBlock -> "Hanunoo"
      HatranBlock -> "Hatran"
      HebrewBlock -> "Hebrew"
      HighPrivateUseSurrogatesBlock -> "High_Private_Use_Surrogates"
      HighSurrogatesBlock -> "High_Surrogates"
      HiraganaBlock -> "Hiragana"
      IPAExtensionsBlock -> "IPA_Extensions"
      IdeographicDescriptionCharactersBlock ->
        "Ideographic_Description_Characters"
      IdeographicSymbolsAndPunctuationBlock ->
        "Ideographic_Symbols_And_Punctuation"
      ImperialAramaicBlock -> "Imperial_Aramaic"
      IndicSiyaqNumbersBlock -> "Indic_Siyaq_Numbers"
      InscriptionalPahlaviBlock -> "Inscriptional_Pahlavi"
      InscriptionalParthianBlock -> "Inscriptional_Parthian"
      JavaneseBlock -> "Javanese"
      KaithiBlock -> "Kaithi"
      KanaExtendedABlock -> "Kana_Extended_A"
      KanaSupplementBlock -> "Kana_Supplement"
      KanbunBlock -> "Kanbun"
      KangxiRadicalsBlock -> "Kangxi_Radicals"
      KannadaBlock -> "Kannada"
      KatakanaBlock -> "Katakana"
      KatakanaPhoneticExtensionsBlock -> "Katakana_Phonetic_Extensions"
      KayahLiBlock -> "Kayah_Li"
      KharoshthiBlock -> "Kharoshthi"
      KhmerBlock -> "Khmer"
      KhmerSymbolsBlock -> "Khmer_Symbols"
      KhojkiBlock -> "Khojki"
      KhudawadiBlock -> "Khudawadi"
      LaoBlock -> "Lao"
      Latin1SupplementBlock -> "Latin_1_Supplement"
      LatinExtendedABlock -> "Latin_Extended_A"
      LatinExtendedAdditionalBlock -> "Latin_Extended_Additional"
      LatinExtendedBBlock -> "Latin_Extended_B"
      LatinExtendedCBlock -> "Latin_Extended_C"
      LatinExtendedDBlock -> "Latin_Extended_D"
      LatinExtendedEBlock -> "Latin_Extended_E"
      LepchaBlock -> "Lepcha"
      LetterlikeSymbolsBlock -> "Letterlike_Symbols"
      LimbuBlock -> "Limbu"
      LinearABlock -> "Linear_A"
      LinearBIdeogramsBlock -> "Linear_B_Ideograms"
      LinearBSyllabaryBlock -> "Linear_B_Syllabary"
      LisuBlock -> "Lisu"
      LowSurrogatesBlock -> "Low_Surrogates"
      LycianBlock -> "Lycian"
      LydianBlock -> "Lydian"
      MahajaniBlock -> "Mahajani"
      MahjongTilesBlock -> "Mahjong_Tiles"
      MakasarBlock -> "Makasar"
      MalayalamBlock -> "Malayalam"
      MandaicBlock -> "Mandaic"
      ManichaeanBlock -> "Manichaean"
      MarchenBlock -> "Marchen"
      MasaramGondiBlock -> "Masaram_Gondi"
      MathematicalAlphanumericSymbolsBlock ->
        "Mathematical_Alphanumeric_Symbols"
      MathematicalOperatorsBlock -> "Mathematical_Operators"
      MayanNumeralsBlock -> "Mayan_Numerals"
      MedefaidrinBlock -> "Medefaidrin"
      MeeteiMayekBlock -> "Meetei_Mayek"
      MeeteiMayekExtensionsBlock -> "Meetei_Mayek_Extensions"
      MendeKikakuiBlock -> "Mende_Kikakui"
      MeroiticCursiveBlock -> "Meroitic_Cursive"
      MeroiticHieroglyphsBlock -> "Meroitic_Hieroglyphs"
      MiaoBlock -> "Miao"
      MiscellaneousMathematicalSymbolsABlock ->
        "Miscellaneous_Mathematical_Symbols_A"
      MiscellaneousMathematicalSymbolsBBlock ->
        "Miscellaneous_Mathematical_Symbols_B"
      MiscellaneousSymbolsAndArrowsBlock -> "Miscellaneous_Symbols_And_Arrows"
      MiscellaneousSymbolsAndPictographsBlock ->
        "Miscellaneous_Symbols_And_Pictographs"
      MiscellaneousSymbolsBlock -> "Miscellaneous_Symbols"
      MiscellaneousTechnicalBlock -> "Miscellaneous_Technical"
      ModiBlock -> "Modi"
      ModifierToneLettersBlock -> "Modifier_Tone_Letters"
      MongolianBlock -> "Mongolian"
      MongolianSupplementBlock -> "Mongolian_Supplement"
      MroBlock -> "Mro"
      MultaniBlock -> "Multani"
      MusicalSymbolsBlock -> "Musical_Symbols"
      MyanmarBlock -> "Myanmar"
      MyanmarExtendedABlock -> "Myanmar_Extended_A"
      MyanmarExtendedBBlock -> "Myanmar_Extended_B"
      NKoBlock -> "NKo"
      NabataeanBlock -> "Nabataean"
      NandinagariBlock -> "Nandinagari"
      NewTaiLueBlock -> "New_Tai_Lue"
      NewaBlock -> "Newa"
      NumberFormsBlock -> "Number_Forms"
      NushuBlock -> "Nushu"
      NyiakengPuachueHmongBlock -> "Nyiakeng_Puachue_Hmong"
      OghamBlock -> "Ogham"
      OlChikiBlock -> "Ol_Chiki"
      OldHungarianBlock -> "Old_Hungarian"
      OldItalicBlock -> "Old_Italic"
      OldNorthArabianBlock -> "Old_North_Arabian"
      OldPermicBlock -> "Old_Permic"
      OldPersianBlock -> "Old_Persian"
      OldSogdianBlock -> "Old_Sogdian"
      OldSouthArabianBlock -> "Old_South_Arabian"
      OldTurkicBlock -> "Old_Turkic"
      OpticalCharacterRecognitionBlock -> "Optical_Character_Recognition"
      OriyaBlock -> "Oriya"
      OrnamentalDingbatsBlock -> "Ornamental_Dingbats"
      OsageBlock -> "Osage"
      OsmanyaBlock -> "Osmanya"
      OttomanSiyaqNumbersBlock -> "Ottoman_Siyaq_Numbers"
      PahawhHmongBlock -> "Pahawh_Hmong"
      PalmyreneBlock -> "Palmyrene"
      PauCinHauBlock -> "Pau_Cin_Hau"
      PhagsPaBlock -> "Phags_Pa"
      PhaistosDiscBlock -> "Phaistos_Disc"
      PhoenicianBlock -> "Phoenician"
      PhoneticExtensionsBlock -> "Phonetic_Extensions"
      PhoneticExtensionsSupplementBlock -> "Phonetic_Extensions_Supplement"
      PlayingCardsBlock -> "Playing_Cards"
      PrivateUseAreaBlock -> "Private_Use_Area"
      PsalterPahlaviBlock -> "Psalter_Pahlavi"
      RejangBlock -> "Rejang"
      RumiNumeralSymbolsBlock -> "Rumi_Numeral_Symbols"
      RunicBlock -> "Runic"
      SamaritanBlock -> "Samaritan"
      SaurashtraBlock -> "Saurashtra"
      SharadaBlock -> "Sharada"
      ShavianBlock -> "Shavian"
      ShorthandFormatControlsBlock -> "Shorthand_Format_Controls"
      SiddhamBlock -> "Siddham"
      SinhalaArchaicNumbersBlock -> "Sinhala_Archaic_Numbers"
      SinhalaBlock -> "Sinhala"
      SmallFormVariantsBlock -> "Small_Form_Variants"
      SmallKanaExtensionBlock -> "Small_Kana_Extension"
      SogdianBlock -> "Sogdian"
      SoraSompengBlock -> "Sora_Sompeng"
      SoyomboBlock -> "Soyombo"
      SpacingModifierLettersBlock -> "Spacing_Modifier_Letters"
      SpecialsBlock -> "Specials"
      SundaneseBlock -> "Sundanese"
      SundaneseSupplementBlock -> "Sundanese_Supplement"
      SuperscriptsAndSubscriptsBlock -> "Superscripts_And_Subscripts"
      SupplementalArrowsABlock -> "Supplemental_Arrows_A"
      SupplementalArrowsBBlock -> "Supplemental_Arrows_B"
      SupplementalArrowsCBlock -> "Supplemental_Arrows_C"
      SupplementalMathematicalOperatorsBlock ->
        "Supplemental_Mathematical_Operators"
      SupplementalPunctuationBlock -> "Supplemental_Punctuation"
      SupplementalSymbolsAndPictographsBlock ->
        "Supplemental_Symbols_And_Pictographs"
      SupplementaryPrivateUseAreaABlock -> "Supplementary_Private_Use_Area_A"
      SupplementaryPrivateUseAreaBBlock -> "Supplementary_Private_Use_Area_B"
      SuttonSignWritingBlock -> "Sutton_SignWriting"
      SylotiNagriBlock -> "Syloti_Nagri"
      SymbolsAndPictographsExtendedABlock ->
        "Symbols_And_Pictographs_Extended_A"
      SyriacBlock -> "Syriac"
      SyriacSupplementBlock -> "Syriac_Supplement"
      TagalogBlock -> "Tagalog"
      TagbanwaBlock -> "Tagbanwa"
      TagsBlock -> "Tags"
      TaiLeBlock -> "Tai_Le"
      TaiThamBlock -> "Tai_Tham"
      TaiVietBlock -> "Tai_Viet"
      TaiXuanJingSymbolsBlock -> "Tai_Xuan_Jing_Symbols"
      TakriBlock -> "Takri"
      TamilBlock -> "Tamil"
      TamilSupplementBlock -> "Tamil_Supplement"
      TangutBlock -> "Tangut"
      TangutComponentsBlock -> "Tangut_Components"
      TeluguBlock -> "Telugu"
      ThaanaBlock -> "Thaana"
      ThaiBlock -> "Thai"
      TibetanBlock -> "Tibetan"
      TifinaghBlock -> "Tifinagh"
      TirhutaBlock -> "Tirhuta"
      TransportAndMapSymbolsBlock -> "Transport_And_Map_Symbols"
      UgariticBlock -> "Ugaritic"
      UnifiedCanadianAboriginalSyllabicsBlock ->
        "Unified_Canadian_Aboriginal_Syllabics"
      UnifiedCanadianAboriginalSyllabicsExtendedBlock ->
        "Unified_Canadian_Aboriginal_Syllabics_Extended"
      VaiBlock -> "Vai"
      VariationSelectorsBlock -> "Variation_Selectors"
      VariationSelectorsSupplementBlock -> "Variation_Selectors_Supplement"
      VedicExtensionsBlock -> "Vedic_Extensions"
      VerticalFormsBlock -> "Vertical_Forms"
      WanchoBlock -> "Wancho"
      WarangCitiBlock -> "Warang_Citi"
      YiRadicalsBlock -> "Yi_Radicals"
      YiSyllablesBlock -> "Yi_Syllables"
      YijingHexagramSymbolsBlock -> "Yijing_Hexagram_Symbols"
      ZanabazarSquareBlock -> "Zanabazar_Square"
  abbreviatedPropertyValueName b =
    case b of
      AdlamBlock -> "Adlam"
      AegeanNumbersBlock -> "Aegean_Numbers"
      AhomBlock -> "Ahom"
      AlchemicalSymbolsBlock -> "Alchemical"
      AlphabeticPresentationFormsBlock -> "Alphabetic_PF"
      AnatolianHieroglyphsBlock -> "Anatolian_Hieroglyphs"
      AncientGreekMusicalNotationBlock -> "Ancient_Greek_Music"
      AncientGreekNumbersBlock -> "Ancient_Greek_Numbers"
      AncientSymbolsBlock -> "Ancient_Symbols"
      ArabicBlock -> "Arabic"
      ArabicExtendedABlock -> "Arabic_Ext_A"
      ArabicMathematicalAlphabeticSymbolsBlock -> "Arabic_Math"
      ArabicPresentationFormsABlock -> "Arabic_PF_A"
      ArabicPresentationFormsBBlock -> "Arabic_PF_B"
      ArabicSupplementBlock -> "Arabic_Sup"
      ArmenianBlock -> "Armenian"
      ArrowsBlock -> "Arrows"
      AvestanBlock -> "Avestan"
      BalineseBlock -> "Balinese"
      BamumBlock -> "Bamum"
      BamumSupplementBlock -> "Bamum_Sup"
      BasicLatinBlock -> "ASCII"
      BassaVahBlock -> "Bassa_Vah"
      BatakBlock -> "Batak"
      BengaliBlock -> "Bengali"
      BhaiksukiBlock -> "Bhaiksuki"
      BlockElementsBlock -> "Block_Elements"
      BopomofoBlock -> "Bopomofo"
      BopomofoExtendedBlock -> "Bopomofo_Ext"
      BoxDrawingBlock -> "Box_Drawing"
      BrahmiBlock -> "Brahmi"
      BraillePatternsBlock -> "Braille"
      BugineseBlock -> "Buginese"
      BuhidBlock -> "Buhid"
      ByzantineMusicalSymbolsBlock -> "Byzantine_Music"
      CJKCompatibilityBlock -> "CJK_Compat"
      CJKCompatibilityFormsBlock -> "CJK_Compat_Forms"
      CJKCompatibilityIdeographsBlock -> "CJK_Compat_Ideographs"
      CJKCompatibilityIdeographsSupplementBlock -> "CJK_Compat_Ideographs_Sup"
      CJKRadicalsSupplementBlock -> "CJK_Radicals_Sup"
      CJKStrokesBlock -> "CJK_Strokes"
      CJKSymbolsAndPunctuationBlock -> "CJK_Symbols"
      CJKUnifiedIdeographsBlock -> "CJK"
      CJKUnifiedIdeographsExtensionABlock -> "CJK_Ext_A"
      CJKUnifiedIdeographsExtensionBBlock -> "CJK_Ext_B"
      CJKUnifiedIdeographsExtensionCBlock -> "CJK_Ext_C"
      CJKUnifiedIdeographsExtensionDBlock -> "CJK_Ext_D"
      CJKUnifiedIdeographsExtensionEBlock -> "CJK_Ext_E"
      CJKUnifiedIdeographsExtensionFBlock -> "CJK_Ext_F"
      CarianBlock -> "Carian"
      CaucasianAlbanianBlock -> "Caucasian_Albanian"
      ChakmaBlock -> "Chakma"
      ChamBlock -> "Cham"
      CherokeeBlock -> "Cherokee"
      CherokeeSupplementBlock -> "Cherokee_Sup"
      ChessSymbolsBlock -> "Chess_Symbols"
      CombiningDiacriticalMarksBlock -> "Diacriticals"
      CombiningDiacriticalMarksExtendedBlock -> "Diacriticals_Ext"
      CombiningDiacriticalMarksSupplementBlock -> "Diacriticals_Sup"
      CombiningDiacriticalMarksForSymbolsBlock -> "Diacriticals_For_Symbols"
      CombiningHalfMarksBlock -> "Half_Marks"
      CommonIndicNumberFormsBlock -> "Indic_Number_Forms"
      ControlPicturesBlock -> "Control_Pictures"
      CopticBlock -> "Coptic"
      CopticEpactNumbersBlock -> "Coptic_Epact_Numbers"
      CountingRodNumeralsBlock -> "Counting_Rod"
      CuneiformBlock -> "Cuneiform"
      CuneiformNumbersAndPunctuationBlock -> "Cuneiform_Numbers"
      CurrencySymbolsBlock -> "Currency_Symbols"
      CypriotSyllabaryBlock -> "Cypriot_Syllabary"
      CyrillicBlock -> "Cyrillic"
      CyrillicExtendedABlock -> "Cyrillic_Ext_A"
      CyrillicExtendedBBlock -> "Cyrillic_Ext_B"
      CyrillicExtendedCBlock -> "Cyrillic_Ext_C"
      CyrillicSupplementBlock -> "Cyrillic_Sup"
      DeseretBlock -> "Deseret"
      DevanagariBlock -> "Devanagari"
      DevanagariExtendedBlock -> "Devanagari_Ext"
      DingbatsBlock -> "Dingbats"
      DograBlock -> "Dogra"
      DominoTilesBlock -> "Domino"
      DuployanBlock -> "Duployan"
      EarlyDynasticCuneiformBlock -> "Early_Dynastic_Cuneiform"
      EgyptianHieroglyphFormatControlsBlock ->
        "Egyptian_Hieroglyph_Format_Controls"
      EgyptianHieroglyphsBlock -> "Egyptian_Hieroglyphs"
      ElbasanBlock -> "Elbasan"
      ElymaicBlock -> "Elymaic"
      EmoticonsBlock -> "Emoticons"
      EnclosedAlphanumericSupplementBlock -> "Enclosed_Alphanum_Sup"
      EnclosedAlphanumericsBlock -> "Enclosed_Alphanum"
      EnclosedCJKLettersAndMonthsBlock -> "Enclosed_CJK"
      EnclosedIdeographicSupplementBlock -> "Enclosed_Ideographic_Sup"
      EthiopicBlock -> "Ethiopic"
      EthiopicExtendedABlock -> "Ethiopic_Ext_A"
      EthiopicExtendedBlock -> "Ethiopic_Ext"
      EthiopicSupplementBlock -> "Ethiopic_Sup"
      GeneralPunctuationBlock -> "Punctuation"
      GeometricShapesBlock -> "Geometric_Shapes"
      GeometricShapesExtendedBlock -> "Geometric_Shapes_Ext"
      GeorgianBlock -> "Georgian"
      GeorgianExtendedBlock -> "Georgian_Ext"
      GeorgianSupplementBlock -> "Georgian_Sup"
      GlagoliticBlock -> "Glagolitic"
      GlagoliticSupplementBlock -> "Glagolitic_Sup"
      GothicBlock -> "Gothic"
      GranthaBlock -> "Grantha"
      GreekAndCopticBlock -> "Greek"
      GreekExtendedBlock -> "Greek_Ext"
      GujaratiBlock -> "Gujarati"
      GunjalaGondiBlock -> "Gunjala_Gondi"
      GurmukhiBlock -> "Gurmukhi"
      HalfwidthAndFullwidthFormsBlock -> "Half_And_Full_Forms"
      HangulCompatibilityJamoBlock -> "Compat_Jamo"
      HangulJamoBlock -> "Jamo"
      HangulJamoExtendedABlock -> "Jamo_Ext_A"
      HangulJamoExtendedBBlock -> "Jamo_Ext_B"
      HangulSyllablesBlock -> "Hangul"
      HanifiRohingyaBlock -> "Hanifi_Rohingya"
      HanunooBlock -> "Hanunoo"
      HatranBlock -> "Hatran"
      HebrewBlock -> "Hebrew"
      HighPrivateUseSurrogatesBlock -> "High_PU_Surrogates"
      HighSurrogatesBlock -> "High_Surrogates"
      HiraganaBlock -> "Hiragana"
      IPAExtensionsBlock -> "IPA_Ext"
      IdeographicDescriptionCharactersBlock -> "IDC"
      IdeographicSymbolsAndPunctuationBlock -> "Ideographic_Symbols"
      ImperialAramaicBlock -> "Imperial_Aramaic"
      IndicSiyaqNumbersBlock -> "Indic_Siyaq_Numbers"
      InscriptionalPahlaviBlock -> "Inscriptional_Pahlavi"
      InscriptionalParthianBlock -> "Inscriptional_Parthian"
      JavaneseBlock -> "Javanese"
      KaithiBlock -> "Kaithi"
      KanaExtendedABlock -> "Kana_Ext_A"
      KanaSupplementBlock -> "Kana_Sup"
      KanbunBlock -> "Kanbun"
      KangxiRadicalsBlock -> "Kangxi"
      KannadaBlock -> "Kannada"
      KatakanaBlock -> "Katakana"
      KatakanaPhoneticExtensionsBlock -> "Katakana_Ext"
      KayahLiBlock -> "Kayah_Li"
      KharoshthiBlock -> "Kharoshthi"
      KhmerBlock -> "Khmer"
      KhmerSymbolsBlock -> "Khmer_Symbols"
      KhojkiBlock -> "Khojki"
      KhudawadiBlock -> "Khudawadi"
      LaoBlock -> "Lao"
      Latin1SupplementBlock -> "Latin_1_Sup"
      LatinExtendedABlock -> "Latin_Ext_A"
      LatinExtendedAdditionalBlock -> "Latin_Ext_Additional"
      LatinExtendedBBlock -> "Latin_Ext_B"
      LatinExtendedCBlock -> "Latin_Ext_C"
      LatinExtendedDBlock -> "Latin_Ext_D"
      LatinExtendedEBlock -> "Latin_Ext_E"
      LepchaBlock -> "Lepcha"
      LetterlikeSymbolsBlock -> "Letterlike_Symbols"
      LimbuBlock -> "Limbu"
      LinearABlock -> "Linear_A"
      LinearBIdeogramsBlock -> "Linear_B_Ideograms"
      LinearBSyllabaryBlock -> "Linear_B_Syllabary"
      LisuBlock -> "Lisu"
      LowSurrogatesBlock -> "Low_Surrogates"
      LycianBlock -> "Lycian"
      LydianBlock -> "Lydian"
      MahajaniBlock -> "Mahajani"
      MahjongTilesBlock -> "Mahjong"
      MakasarBlock -> "Makasar"
      MalayalamBlock -> "Malayalam"
      MandaicBlock -> "Mandaic"
      ManichaeanBlock -> "Manichaean"
      MarchenBlock -> "Marchen"
      MasaramGondiBlock -> "Masaram_Gondi"
      MathematicalAlphanumericSymbolsBlock -> "Math_Alphanum"
      MathematicalOperatorsBlock -> "Math_Operators"
      MayanNumeralsBlock -> "Mayan_Numerals"
      MedefaidrinBlock -> "Medefaidrin"
      MeeteiMayekBlock -> "Meetei_Mayek"
      MeeteiMayekExtensionsBlock -> "Meetei_Mayek_Ext"
      MendeKikakuiBlock -> "Mende_Kikakui"
      MeroiticCursiveBlock -> "Meroitic_Cursive"
      MeroiticHieroglyphsBlock -> "Meroitic_Hieroglyphs"
      MiaoBlock -> "Miao"
      MiscellaneousMathematicalSymbolsABlock -> "Misc_Math_Symbols_A"
      MiscellaneousMathematicalSymbolsBBlock -> "Misc_Math_Symbols_B"
      MiscellaneousSymbolsAndArrowsBlock -> "Misc_Arrows"
      MiscellaneousSymbolsAndPictographsBlock -> "Misc_Pictographs"
      MiscellaneousSymbolsBlock -> "Misc_Symbols"
      MiscellaneousTechnicalBlock -> "Misc_Technical"
      ModiBlock -> "Modi"
      ModifierToneLettersBlock -> "Modifier_Tone_Letters"
      MongolianBlock -> "Mongolian"
      MongolianSupplementBlock -> "Mongolian_Sup"
      MroBlock -> "Mro"
      MultaniBlock -> "Multani"
      MusicalSymbolsBlock -> "Music"
      MyanmarBlock -> "Myanmar"
      MyanmarExtendedABlock -> "Myanmar_Ext_A"
      MyanmarExtendedBBlock -> "Myanmar_Ext_B"
      NKoBlock -> "NKo"
      NabataeanBlock -> "Nabataean"
      NandinagariBlock -> "Nandinagari"
      NewTaiLueBlock -> "New_Tai_Lue"
      NewaBlock -> "Newa"
      NumberFormsBlock -> "Number_Forms"
      NushuBlock -> "Nushu"
      NyiakengPuachueHmongBlock -> "Nyiakeng_Puachue_Hmong"
      OghamBlock -> "Ogham"
      OlChikiBlock -> "Ol_Chiki"
      OldHungarianBlock -> "Old_Hungarian"
      OldItalicBlock -> "Old_Italic"
      OldNorthArabianBlock -> "Old_North_Arabian"
      OldPermicBlock -> "Old_Permic"
      OldPersianBlock -> "Old_Persian"
      OldSogdianBlock -> "Old_Sogdian"
      OldSouthArabianBlock -> "Old_South_Arabian"
      OldTurkicBlock -> "Old_Turkic"
      OpticalCharacterRecognitionBlock -> "OCR"
      OriyaBlock -> "Oriya"
      OrnamentalDingbatsBlock -> "Ornamental_Dingbats"
      OsageBlock -> "Osage"
      OsmanyaBlock -> "Osmanya"
      OttomanSiyaqNumbersBlock -> "Ottoman_Siyaq_Numbers"
      PahawhHmongBlock -> "Pahawh_Hmong"
      PalmyreneBlock -> "Palmyrene"
      PauCinHauBlock -> "Pau_Cin_Hau"
      PhagsPaBlock -> "Phags_Pa"
      PhaistosDiscBlock -> "Phaistos"
      PhoenicianBlock -> "Phoenician"
      PhoneticExtensionsBlock -> "Phonetic_Ext"
      PhoneticExtensionsSupplementBlock -> "Phonetic_Ext_Sup"
      PlayingCardsBlock -> "Playing_Cards"
      PrivateUseAreaBlock -> "PUA"
      PsalterPahlaviBlock -> "Psalter_Pahlavi"
      RejangBlock -> "Rejang"
      RumiNumeralSymbolsBlock -> "Rumi"
      RunicBlock -> "Runic"
      SamaritanBlock -> "Samaritan"
      SaurashtraBlock -> "Saurashtra"
      SharadaBlock -> "Sharada"
      ShavianBlock -> "Shavian"
      ShorthandFormatControlsBlock -> "Shorthand_Format_Controls"
      SiddhamBlock -> "Siddham"
      SinhalaArchaicNumbersBlock -> "Sinhala_Archaic_Numbers"
      SinhalaBlock -> "Sinhala"
      SmallFormVariantsBlock -> "Small_Forms"
      SmallKanaExtensionBlock -> "Small_Kana_Ext"
      SogdianBlock -> "Sogdian"
      SoraSompengBlock -> "Sora_Sompeng"
      SoyomboBlock -> "Soyombo"
      SpacingModifierLettersBlock -> "Modifier_Letters"
      SpecialsBlock -> "Specials"
      SundaneseBlock -> "Sundanese"
      SundaneseSupplementBlock -> "Sundanese_Sup"
      SuperscriptsAndSubscriptsBlock -> "Super_And_Sub"
      SupplementalArrowsABlock -> "Sup_Arrows_A"
      SupplementalArrowsBBlock -> "Sup_Arrows_B"
      SupplementalArrowsCBlock -> "Sup_Arrows_C"
      SupplementalMathematicalOperatorsBlock -> "Sup_Math_Operators"
      SupplementalPunctuationBlock -> "Sup_Punctuation"
      SupplementalSymbolsAndPictographsBlock -> "Sup_Symbols_And_Pictographs"
      SupplementaryPrivateUseAreaABlock -> "Sup_PUA_A"
      SupplementaryPrivateUseAreaBBlock -> "Sup_PUA_B"
      SuttonSignWritingBlock -> "Sutton_SignWriting"
      SylotiNagriBlock -> "Syloti_Nagri"
      SymbolsAndPictographsExtendedABlock -> "Symbols_And_Pictographs_Ext_A"
      SyriacBlock -> "Syriac"
      SyriacSupplementBlock -> "Syriac_Sup"
      TagalogBlock -> "Tagalog"
      TagbanwaBlock -> "Tagbanwa"
      TagsBlock -> "Tags"
      TaiLeBlock -> "Tai_Le"
      TaiThamBlock -> "Tai_Tham"
      TaiVietBlock -> "Tai_Viet"
      TaiXuanJingSymbolsBlock -> "Tai_Xuan_Jing"
      TakriBlock -> "Takri"
      TamilBlock -> "Tamil"
      TamilSupplementBlock -> "Tamil_Sup"
      TangutBlock -> "Tangut"
      TangutComponentsBlock -> "Tangut_Components"
      TeluguBlock -> "Telugu"
      ThaanaBlock -> "Thaana"
      ThaiBlock -> "Thai"
      TibetanBlock -> "Tibetan"
      TifinaghBlock -> "Tifinagh"
      TirhutaBlock -> "Tirhuta"
      TransportAndMapSymbolsBlock -> "Transport_And_Map"
      UgariticBlock -> "Ugaritic"
      UnifiedCanadianAboriginalSyllabicsBlock -> "UCAS"
      UnifiedCanadianAboriginalSyllabicsExtendedBlock -> "UCAS_Ext"
      VaiBlock -> "Vai"
      VariationSelectorsBlock -> "VS"
      VariationSelectorsSupplementBlock -> "VS_Sup"
      VedicExtensionsBlock -> "Vedic_Ext"
      VerticalFormsBlock -> "Vertical_Forms"
      WanchoBlock -> "Wancho"
      WarangCitiBlock -> "Warang_Citi"
      YiRadicalsBlock -> "Yi_Radicals"
      YiSyllablesBlock -> "Yi_Syllables"
      YijingHexagramSymbolsBlock -> "Yijing"
      ZanabazarSquareBlock -> "Zanabazar_Square"

-- | A catalogue of scripts encoded in Unicode
--
-- See 'Data.Unistring.UCD.script' for more details
--
-- @since 0.1.0.0
data Script
  = AdlamScript -- ^ @since 0.1.0.0
  | AhomScript -- ^ @since 0.1.0.0
  | AnatolianHieroglyphsScript -- ^ @since 0.1.0.0
  | ArabicScript -- ^ @since 0.1.0.0
  | ArmenianScript -- ^ @since 0.1.0.0
  | AvestanScript -- ^ @since 0.1.0.0
  | BalineseScript -- ^ @since 0.1.0.0
  | BamumScript -- ^ @since 0.1.0.0
  | BassaVahScript -- ^ @since 0.1.0.0
  | BatakScript -- ^ @since 0.1.0.0
  | BengaliScript -- ^ @since 0.1.0.0
  | BhaiksukiScript -- ^ @since 0.1.0.0
  | BopomofoScript -- ^ @since 0.1.0.0
  | BrahmiScript -- ^ @since 0.1.0.0
  | BrailleScript -- ^ @since 0.1.0.0
  | BugineseScript -- ^ @since 0.1.0.0
  | BuhidScript -- ^ @since 0.1.0.0
  | CanadianAboriginalScript -- ^ @since 0.1.0.0
  | CarianScript -- ^ @since 0.1.0.0
  | CaucasianAlbanianScript -- ^ @since 0.1.0.0
  | ChakmaScript -- ^ @since 0.1.0.0
  | ChamScript -- ^ @since 0.1.0.0
  | CherokeeScript -- ^ @since 0.1.0.0
  | CommonScript -- ^ @since 0.1.0.0
  | CopticScript -- ^ @since 0.1.0.0
  | CuneiformScript -- ^ @since 0.1.0.0
  | CypriotScript -- ^ @since 0.1.0.0
  | CyrillicScript -- ^ @since 0.1.0.0
  | DeseretScript -- ^ @since 0.1.0.0
  | DevanagariScript -- ^ @since 0.1.0.0
  | DograScript -- ^ @since 0.1.0.0
  | DuployanScript -- ^ @since 0.1.0.0
  | EgyptianHieroglyphsScript -- ^ @since 0.1.0.0
  | ElbasanScript -- ^ @since 0.1.0.0
  | ElymaicScript -- ^ @since 0.1.0.0
  | EthiopicScript -- ^ @since 0.1.0.0
  | GeorgianScript -- ^ @since 0.1.0.0
  | GlagoliticScript -- ^ @since 0.1.0.0
  | GothicScript -- ^ @since 0.1.0.0
  | GranthaScript -- ^ @since 0.1.0.0
  | GreekScript -- ^ @since 0.1.0.0
  | GujaratiScript -- ^ @since 0.1.0.0
  | GunjalaGondiScript -- ^ @since 0.1.0.0
  | GurmukhiScript -- ^ @since 0.1.0.0
  | HanScript -- ^ @since 0.1.0.0
  | HangulScript -- ^ @since 0.1.0.0
  | HanifiRohingyaScript -- ^ @since 0.1.0.0
  | HanunooScript -- ^ @since 0.1.0.0
  | HatranScript -- ^ @since 0.1.0.0
  | HebrewScript -- ^ @since 0.1.0.0
  | HiraganaScript -- ^ @since 0.1.0.0
  | ImperialAramaicScript -- ^ @since 0.1.0.0
  | InheritedScript -- ^ @since 0.1.0.0
  | InscriptionalPahlaviScript -- ^ @since 0.1.0.0
  | InscriptionalParthianScript -- ^ @since 0.1.0.0
  | JavaneseScript -- ^ @since 0.1.0.0
  | KaithiScript -- ^ @since 0.1.0.0
  | KannadaScript -- ^ @since 0.1.0.0
  | KatakanaScript -- ^ @since 0.1.0.0
  | KayahLiScript -- ^ @since 0.1.0.0
  | KharoshthiScript -- ^ @since 0.1.0.0
  | KhmerScript -- ^ @since 0.1.0.0
  | KhojkiScript -- ^ @since 0.1.0.0
  | KhudawadiScript -- ^ @since 0.1.0.0
  | LaoScript -- ^ @since 0.1.0.0
  | LatinScript -- ^ @since 0.1.0.0
  | LepchaScript -- ^ @since 0.1.0.0
  | LimbuScript -- ^ @since 0.1.0.0
  | LinearAScript -- ^ @since 0.1.0.0
  | LinearBScript -- ^ @since 0.1.0.0
  | LisuScript -- ^ @since 0.1.0.0
  | LycianScript -- ^ @since 0.1.0.0
  | LydianScript -- ^ @since 0.1.0.0
  | MahajaniScript -- ^ @since 0.1.0.0
  | MakasarScript -- ^ @since 0.1.0.0
  | MalayalamScript -- ^ @since 0.1.0.0
  | MandaicScript -- ^ @since 0.1.0.0
  | ManichaeanScript -- ^ @since 0.1.0.0
  | MarchenScript -- ^ @since 0.1.0.0
  | MasaramGondiScript -- ^ @since 0.1.0.0
  | MedefaidrinScript -- ^ @since 0.1.0.0
  | MeeteiMayekScript -- ^ @since 0.1.0.0
  | MendeKikakuiScript -- ^ @since 0.1.0.0
  | MeroiticCursiveScript -- ^ @since 0.1.0.0
  | MeroiticHieroglyphsScript -- ^ @since 0.1.0.0
  | MiaoScript -- ^ @since 0.1.0.0
  | ModiScript -- ^ @since 0.1.0.0
  | MongolianScript -- ^ @since 0.1.0.0
  | MroScript -- ^ @since 0.1.0.0
  | MultaniScript -- ^ @since 0.1.0.0
  | MyanmarScript -- ^ @since 0.1.0.0
  | NabataeanScript -- ^ @since 0.1.0.0
  | NandinagariScript -- ^ @since 0.1.0.0
  | NewaScript -- ^ @since 0.1.0.0
  | NewTaiLueScript -- ^ @since 0.1.0.0
  | NkoScript -- ^ @since 0.1.0.0
  | NushuScript -- ^ @since 0.1.0.0
  | NyiakengPuachueHmongScript -- ^ @since 0.1.0.0
  | OghamScript -- ^ @since 0.1.0.0
  | OlChikiScript -- ^ @since 0.1.0.0
  | OldHungarianScript -- ^ @since 0.1.0.0
  | OldItalicScript -- ^ @since 0.1.0.0
  | OldNorthArabianScript -- ^ @since 0.1.0.0
  | OldPermicScript -- ^ @since 0.1.0.0
  | OldPersianScript -- ^ @since 0.1.0.0
  | OldSogdianScript -- ^ @since 0.1.0.0
  | OldSouthArabianScript -- ^ @since 0.1.0.0
  | OldTurkicScript -- ^ @since 0.1.0.0
  | OriyaScript -- ^ @since 0.1.0.0
  | OsageScript -- ^ @since 0.1.0.0
  | OsmanyaScript -- ^ @since 0.1.0.0
  | PahawhHmongScript -- ^ @since 0.1.0.0
  | PalmyreneScript -- ^ @since 0.1.0.0
  | PauCinHauScript -- ^ @since 0.1.0.0
  | PhagsPaScript -- ^ @since 0.1.0.0
  | PhoenicianScript -- ^ @since 0.1.0.0
  | PsalterPahlaviScript -- ^ @since 0.1.0.0
  | RejangScript -- ^ @since 0.1.0.0
  | RunicScript -- ^ @since 0.1.0.0
  | SamaritanScript -- ^ @since 0.1.0.0
  | SaurashtraScript -- ^ @since 0.1.0.0
  | SharadaScript -- ^ @since 0.1.0.0
  | ShavianScript -- ^ @since 0.1.0.0
  | SiddhamScript -- ^ @since 0.1.0.0
  | SignWritingScript -- ^ @since 0.1.0.0
  | SinhalaScript -- ^ @since 0.1.0.0
  | SogdianScript -- ^ @since 0.1.0.0
  | SoraSompengScript -- ^ @since 0.1.0.0
  | SoyomboScript -- ^ @since 0.1.0.0
  | SundaneseScript -- ^ @since 0.1.0.0
  | SylotiNagriScript -- ^ @since 0.1.0.0
  | SyriacScript -- ^ @since 0.1.0.0
  | TagalogScript -- ^ @since 0.1.0.0
  | TagbanwaScript -- ^ @since 0.1.0.0
  | TaiLeScript -- ^ @since 0.1.0.0
  | TaiThamScript -- ^ @since 0.1.0.0
  | TaiVietScript -- ^ @since 0.1.0.0
  | TakriScript -- ^ @since 0.1.0.0
  | TamilScript -- ^ @since 0.1.0.0
  | TangutScript -- ^ @since 0.1.0.0
  | TeluguScript -- ^ @since 0.1.0.0
  | ThaanaScript -- ^ @since 0.1.0.0
  | ThaiScript -- ^ @since 0.1.0.0
  | TibetanScript -- ^ @since 0.1.0.0
  | TifinaghScript -- ^ @since 0.1.0.0
  | TirhutaScript -- ^ @since 0.1.0.0
  | UgariticScript -- ^ @since 0.1.0.0
  | UnknownScript -- ^ @since 0.1.0.0
  | VaiScript -- ^ @since 0.1.0.0
  | WanchoScript -- ^ @since 0.1.0.0
  | WarangCitiScript -- ^ @since 0.1.0.0
  | YiScript -- ^ @since 0.1.0.0
  | ZanabazarSquareScript -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty Script where
  fullPropertyValueName s =
    case s of
      AdlamScript -> "Adlam"
      AhomScript -> "Ahom"
      AnatolianHieroglyphsScript -> "Anatolian_Hieroglyphs"
      ArabicScript -> "Arabic"
      ArmenianScript -> "Armenian"
      AvestanScript -> "Avestan"
      BalineseScript -> "Balinese"
      BamumScript -> "Bamum"
      BassaVahScript -> "Bassa_Vah"
      BatakScript -> "Batak"
      BengaliScript -> "Bengali"
      BhaiksukiScript -> "Bhaiksuki"
      BopomofoScript -> "Bopomofo"
      BrahmiScript -> "Brahmi"
      BrailleScript -> "Braille"
      BugineseScript -> "Buginese"
      BuhidScript -> "Buhid"
      CanadianAboriginalScript -> "Canadian_Aboriginal"
      CarianScript -> "Carian"
      CaucasianAlbanianScript -> "Caucasian_Albanian"
      ChakmaScript -> "Chakma"
      ChamScript -> "Cham"
      CherokeeScript -> "Cherokee"
      CommonScript -> "Common"
      CopticScript -> "Coptic"
      CuneiformScript -> "Cuneiform"
      CypriotScript -> "Cypriot"
      CyrillicScript -> "Cyrillic"
      DeseretScript -> "Deseret"
      DevanagariScript -> "Devanagari"
      DograScript -> "Dogra"
      DuployanScript -> "Duployan"
      EgyptianHieroglyphsScript -> "Egyptian_Hieroglyphs"
      ElbasanScript -> "Elbasan"
      ElymaicScript -> "Elymaic"
      EthiopicScript -> "Ethiopic"
      GeorgianScript -> "Georgian"
      GlagoliticScript -> "Glagolitic"
      GothicScript -> "Gothic"
      GranthaScript -> "Grantha"
      GreekScript -> "Greek"
      GujaratiScript -> "Gujarati"
      GunjalaGondiScript -> "Gunjala_Gondi"
      GurmukhiScript -> "Gurmukhi"
      HanScript -> "Han"
      HangulScript -> "Hangul"
      HanifiRohingyaScript -> "Hanifi_Rohingya"
      HanunooScript -> "Hanunoo"
      HatranScript -> "Hatran"
      HebrewScript -> "Hebrew"
      HiraganaScript -> "Hiragana"
      ImperialAramaicScript -> "Imperial_Aramaic"
      InheritedScript -> "Inherited"
      InscriptionalPahlaviScript -> "Inscriptional_Pahlavi"
      InscriptionalParthianScript -> "Inscriptional_Parthian"
      JavaneseScript -> "Javanese"
      KaithiScript -> "Kaithi"
      KannadaScript -> "Kannada"
      KatakanaScript -> "Katakana"
      KayahLiScript -> "Kayah_Li"
      KharoshthiScript -> "Kharoshthi"
      KhmerScript -> "Khmer"
      KhojkiScript -> "Khojki"
      KhudawadiScript -> "Khudawadi"
      LaoScript -> "Lao"
      LatinScript -> "Latin"
      LepchaScript -> "Lepcha"
      LimbuScript -> "Limbu"
      LinearAScript -> "Linear_A"
      LinearBScript -> "Linear_B"
      LisuScript -> "Lisu"
      LycianScript -> "Lycian"
      LydianScript -> "Lydian"
      MahajaniScript -> "Mahajani"
      MakasarScript -> "Makasar"
      MalayalamScript -> "Malayalam"
      MandaicScript -> "Mandaic"
      ManichaeanScript -> "Manichaean"
      MarchenScript -> "Marchen"
      MasaramGondiScript -> "Masaram_Gondi"
      MedefaidrinScript -> "Medefaidrin"
      MeeteiMayekScript -> "Meetei_Mayek"
      MendeKikakuiScript -> "Mende_Kikakui"
      MeroiticCursiveScript -> "Meroitic_Cursive"
      MeroiticHieroglyphsScript -> "Meroitic_Hieroglyphs"
      MiaoScript -> "Miao"
      ModiScript -> "Modi"
      MongolianScript -> "Mongolian"
      MroScript -> "Mro"
      MultaniScript -> "Multani"
      MyanmarScript -> "Myanmar"
      NabataeanScript -> "Nabataean"
      NandinagariScript -> "Nandinagari"
      NewTaiLueScript -> "New_Tai_Lue"
      NewaScript -> "Newa"
      NkoScript -> "Nko"
      NushuScript -> "Nushu"
      NyiakengPuachueHmongScript -> "Nyiakeng_Puachue_Hmong"
      OghamScript -> "Ogham"
      OlChikiScript -> "Ol_Chiki"
      OldHungarianScript -> "Old_Hungarian"
      OldItalicScript -> "Old_Italic"
      OldNorthArabianScript -> "Old_North_Arabian"
      OldPermicScript -> "Old_Permic"
      OldPersianScript -> "Old_Persian"
      OldSogdianScript -> "Old_Sogdian"
      OldSouthArabianScript -> "Old_South_Arabian"
      OldTurkicScript -> "Old_Turkic"
      OriyaScript -> "Oriya"
      OsageScript -> "Osage"
      OsmanyaScript -> "Osmanya"
      PahawhHmongScript -> "Pahawh_Hmong"
      PalmyreneScript -> "Palmyrene"
      PauCinHauScript -> "Pau_Cin_Hau"
      PhagsPaScript -> "Phags_Pa"
      PhoenicianScript -> "Phoenician"
      PsalterPahlaviScript -> "Psalter_Pahlavi"
      RejangScript -> "Rejang"
      RunicScript -> "Runic"
      SamaritanScript -> "Samaritan"
      SaurashtraScript -> "Saurashtra"
      SharadaScript -> "Sharada"
      ShavianScript -> "Shavian"
      SiddhamScript -> "Siddham"
      SignWritingScript -> "SignWriting"
      SinhalaScript -> "Sinhala"
      SogdianScript -> "Sogdian"
      SoraSompengScript -> "Sora_Sompeng"
      SoyomboScript -> "Soyombo"
      SundaneseScript -> "Sundanese"
      SylotiNagriScript -> "Syloti_Nagri"
      SyriacScript -> "Syriac"
      TagalogScript -> "Tagalog"
      TagbanwaScript -> "Tagbanwa"
      TaiLeScript -> "Tai_Le"
      TaiThamScript -> "Tai_Tham"
      TaiVietScript -> "Tai_Viet"
      TakriScript -> "Takri"
      TamilScript -> "Tamil"
      TangutScript -> "Tangut"
      TeluguScript -> "Telugu"
      ThaanaScript -> "Thaana"
      ThaiScript -> "Thai"
      TibetanScript -> "Tibetan"
      TifinaghScript -> "Tifinagh"
      TirhutaScript -> "Tirhuta"
      UgariticScript -> "Ugaritic"
      UnknownScript -> "Unknown"
      VaiScript -> "Vai"
      WanchoScript -> "Wancho"
      WarangCitiScript -> "Warang_Citi"
      YiScript -> "Yi"
      ZanabazarSquareScript -> "Zanabazar_Square"
  abbreviatedPropertyValueName s =
    case s of
      AdlamScript -> "Adlm"
      AhomScript -> "Ahom"
      AnatolianHieroglyphsScript -> "Hluw"
      ArabicScript -> "Arab"
      ArmenianScript -> "Armn"
      AvestanScript -> "Avst"
      BalineseScript -> "Bali"
      BamumScript -> "Bamu"
      BassaVahScript -> "Bass"
      BatakScript -> "Batk"
      BengaliScript -> "Beng"
      BhaiksukiScript -> "Bhks"
      BopomofoScript -> "Bopo"
      BrahmiScript -> "Brah"
      BrailleScript -> "Brai"
      BugineseScript -> "Bugi"
      BuhidScript -> "Buhd"
      CanadianAboriginalScript -> "Cans"
      CarianScript -> "Cari"
      CaucasianAlbanianScript -> "Aghb"
      ChakmaScript -> "Cakm"
      ChamScript -> "Cham"
      CherokeeScript -> "Cher"
      CommonScript -> "Zyyy"
      CopticScript -> "Copt"
      CuneiformScript -> "Xsux"
      CypriotScript -> "Cprt"
      CyrillicScript -> "Cyrl"
      DeseretScript -> "Dsrt"
      DevanagariScript -> "Deva"
      DograScript -> "Dogr"
      DuployanScript -> "Dupl"
      EgyptianHieroglyphsScript -> "Egyp"
      ElbasanScript -> "Elba"
      ElymaicScript -> "Elym"
      EthiopicScript -> "Ethi"
      GeorgianScript -> "Geor"
      GlagoliticScript -> "Glag"
      GothicScript -> "Goth"
      GranthaScript -> "Gran"
      GreekScript -> "Grek"
      GujaratiScript -> "Gujr"
      GunjalaGondiScript -> "Gong"
      GurmukhiScript -> "Guru"
      HanScript -> "Hani"
      HangulScript -> "Hang"
      HanifiRohingyaScript -> "Rohg"
      HanunooScript -> "Hano"
      HatranScript -> "Hatr"
      HebrewScript -> "Hebr"
      HiraganaScript -> "Hira"
      ImperialAramaicScript -> "Armi"
      InheritedScript -> "Zinh"
      InscriptionalPahlaviScript -> "Phli"
      InscriptionalParthianScript -> "Prti"
      JavaneseScript -> "Java"
      KaithiScript -> "Kthi"
      KannadaScript -> "Knda"
      KatakanaScript -> "Kana"
      KayahLiScript -> "Kali"
      KharoshthiScript -> "Khar"
      KhmerScript -> "Khmr"
      KhojkiScript -> "Khoj"
      KhudawadiScript -> "Sind"
      LaoScript -> "Laoo"
      LatinScript -> "Latn"
      LepchaScript -> "Lepc"
      LimbuScript -> "Limb"
      LinearAScript -> "Lina"
      LinearBScript -> "Linb"
      LisuScript -> "Lisu"
      LycianScript -> "Lyci"
      LydianScript -> "Lydi"
      MahajaniScript -> "Mahj"
      MakasarScript -> "Maka"
      MalayalamScript -> "Mlym"
      MandaicScript -> "Mand"
      ManichaeanScript -> "Mani"
      MarchenScript -> "Marc"
      MasaramGondiScript -> "Gonm"
      MedefaidrinScript -> "Medf"
      MeeteiMayekScript -> "Mtei"
      MendeKikakuiScript -> "Mend"
      MeroiticCursiveScript -> "Merc"
      MeroiticHieroglyphsScript -> "Mero"
      MiaoScript -> "Plrd"
      ModiScript -> "Modi"
      MongolianScript -> "Mong"
      MroScript -> "Mroo"
      MultaniScript -> "Mult"
      MyanmarScript -> "Mymr"
      NabataeanScript -> "Nbat"
      NandinagariScript -> "Nand"
      NewaScript -> "Newa"
      NewTaiLueScript -> "Talu"
      NkoScript -> "Nkoo"
      NushuScript -> "Nshu"
      NyiakengPuachueHmongScript -> "Hmnp"
      OghamScript -> "Ogam"
      OlChikiScript -> "Olck"
      OldHungarianScript -> "Hung"
      OldItalicScript -> "Ital"
      OldNorthArabianScript -> "Narb"
      OldPermicScript -> "Perm"
      OldPersianScript -> "Xpeo"
      OldSogdianScript -> "Sogo"
      OldSouthArabianScript -> "Sarb"
      OldTurkicScript -> "Orkh"
      OriyaScript -> "Orya"
      OsageScript -> "Osge"
      OsmanyaScript -> "Osma"
      PahawhHmongScript -> "Hmng"
      PalmyreneScript -> "Palm"
      PauCinHauScript -> "Pauc"
      PhagsPaScript -> "Phag"
      PhoenicianScript -> "Phnx"
      PsalterPahlaviScript -> "Phlp"
      RejangScript -> "Rjng"
      RunicScript -> "Runr"
      SamaritanScript -> "Samr"
      SaurashtraScript -> "Saur"
      SharadaScript -> "Shrd"
      ShavianScript -> "Shaw"
      SiddhamScript -> "Sidd"
      SignWritingScript -> "Sgnw"
      SinhalaScript -> "Sinh"
      SogdianScript -> "Sogd"
      SoraSompengScript -> "Sora"
      SoyomboScript -> "Soyo"
      SundaneseScript -> "Sund"
      SylotiNagriScript -> "Sylo"
      SyriacScript -> "Syrc"
      TagalogScript -> "Tglg"
      TagbanwaScript -> "Tagb"
      TaiLeScript -> "Tale"
      TaiThamScript -> "Lana"
      TaiVietScript -> "Tavt"
      TakriScript -> "Takr"
      TamilScript -> "Taml"
      TangutScript -> "Tang"
      TeluguScript -> "Telu"
      ThaanaScript -> "Thaa"
      ThaiScript -> "Thai"
      TibetanScript -> "Tibt"
      TifinaghScript -> "Tfng"
      TirhutaScript -> "Tirh"
      UgariticScript -> "Ugar"
      UnknownScript -> "Zzzz"
      VaiScript -> "Vaii"
      WanchoScript -> "Wcho"
      WarangCitiScript -> "Wara"
      YiScript -> "Yiii"
      ZanabazarSquareScript -> "Zanb"

-- | Type of compatibility decomposition mapping, if it exists.
--
-- See 'Data.Unistring.UCD.decompositionType' for details.
data DecompositionType
  = Canonical -- ^ @since 0.1.0.0
  | Compat -- ^ @since 0.1.0.0
  | Circle -- ^ @since 0.1.0.0
  | Final -- ^ @since 0.1.0.0
  | Font -- ^ @since 0.1.0.0
  | Fraction -- ^ @since 0.1.0.0
  | Initial -- ^ @since 0.1.0.0
  | Isolated -- ^ @since 0.1.0.0
  | Medial -- ^ @since 0.1.0.0
  | Narrow -- ^ @since 0.1.0.0
  | NoBreak -- ^ @since 0.1.0.0
  | Small -- ^ @since 0.1.0.0
  | Square -- ^ @since 0.1.0.0
  | Sub -- ^ @since 0.1.0.0
  | Super -- ^ @since 0.1.0.0
  | Vertical -- ^ @since 0.1.0.0
  | Wide -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty DecompositionType where
  abbreviatedPropertyValueName dt =
    case dt of
      Canonical -> "Can"
      Compat -> "Com"
      Circle -> "Enc"
      Final -> "Fin"
      Font -> "Font"
      Fraction -> "Fra"
      Initial -> "Init"
      Isolated -> "Iso"
      Medial -> "Med"
      Narrow -> "Nar"
      NoBreak -> "Nb"
      Small -> "Sml"
      Square -> "Sqr"
      Sub -> "Sub"
      Super -> "Sup"
      Vertical -> "Vert"
      Wide -> "Wide"
  fullPropertyValueName dt =
    case dt of
      Canonical -> "Canonical"
      Compat -> "Compat"
      Circle -> "Circle"
      Final -> "Final"
      Font -> "Font"
      Fraction -> "Fraction"
      Initial -> "Initial"
      Isolated -> "Isolated"
      Medial -> "Medial"
      Narrow -> "Narrow"
      NoBreak -> "Nobreak"
      Small -> "Small"
      Square -> "Square"
      Sub -> "Sub"
      Super -> "Super"
      Vertical -> "Vertical"
      Wide -> "Wide"

-- | See 'Data.Unistring.UCD.joiningGroup'
data JoiningGroup
  = AfricanFeh -- ^ @since 0.1.0.0
  | AfricanNoon -- ^ @since 0.1.0.0
  | AfricanQaf -- ^ @since 0.1.0.0
  | Ain -- ^ @since 0.1.0.0
  | Alaph -- ^ @since 0.1.0.0
  | Alef -- ^ @since 0.1.0.0
  | Beh -- ^ @since 0.1.0.0
  | Beth -- ^ @since 0.1.0.0
  | BurushaskiYehBarree -- ^ @since 0.1.0.0
  | Dal -- ^ @since 0.1.0.0
  | DalathRish -- ^ @since 0.1.0.0
  | E -- ^ @since 0.1.0.0
  | FarsiYeh -- ^ @since 0.1.0.0
  | Fe -- ^ @since 0.1.0.0
  | Feh -- ^ @since 0.1.0.0
  | FinalSemkath -- ^ @since 0.1.0.0
  | Gaf -- ^ @since 0.1.0.0
  | Gamal -- ^ @since 0.1.0.0
  | Hah -- ^ @since 0.1.0.0
  | HanifiRohingyaKinnaYa -- ^ @since 0.1.0.0
  | HanifiRohingyaPa -- ^ @since 0.1.0.0
  | He -- ^ @since 0.1.0.0
  | Heh -- ^ @since 0.1.0.0
  | HehGoal -- ^ @since 0.1.0.0
  | Heth -- ^ @since 0.1.0.0
  | Kaf -- ^ @since 0.1.0.0
  | Kaph -- ^ @since 0.1.0.0
  | Khaph -- ^ @since 0.1.0.0
  | KnottedHeh -- ^ @since 0.1.0.0
  | Lam -- ^ @since 0.1.0.0
  | Lamadh -- ^ @since 0.1.0.0
  | MalayalamBha -- ^ @since 0.1.0.0
  | MalayalamJa -- ^ @since 0.1.0.0
  | MalayalamLla -- ^ @since 0.1.0.0
  | MalayalamLlla -- ^ @since 0.1.0.0
  | MalayalamNga -- ^ @since 0.1.0.0
  | MalayalamNna -- ^ @since 0.1.0.0
  | MalayalamNnna -- ^ @since 0.1.0.0
  | MalayalamNya -- ^ @since 0.1.0.0
  | MalayalamRa -- ^ @since 0.1.0.0
  | MalayalamSsa -- ^ @since 0.1.0.0
  | MalayalamTta -- ^ @since 0.1.0.0
  | ManichaeanAleph -- ^ @since 0.1.0.0
  | ManichaeanAyin -- ^ @since 0.1.0.0
  | ManichaeanBeth -- ^ @since 0.1.0.0
  | ManichaeanDaleth -- ^ @since 0.1.0.0
  | ManichaeanDhamedh -- ^ @since 0.1.0.0
  | ManichaeanFive -- ^ @since 0.1.0.0
  | ManichaeanGimel -- ^ @since 0.1.0.0
  | ManichaeanHeth -- ^ @since 0.1.0.0
  | ManichaeanHundred -- ^ @since 0.1.0.0
  | ManichaeanKaph -- ^ @since 0.1.0.0
  | ManichaeanLamedh -- ^ @since 0.1.0.0
  | ManichaeanMem -- ^ @since 0.1.0.0
  | ManichaeanNun -- ^ @since 0.1.0.0
  | ManichaeanOne -- ^ @since 0.1.0.0
  | ManichaeanPe -- ^ @since 0.1.0.0
  | ManichaeanQoph -- ^ @since 0.1.0.0
  | ManichaeanResh -- ^ @since 0.1.0.0
  | ManichaeanSadhe -- ^ @since 0.1.0.0
  | ManichaeanSamekh -- ^ @since 0.1.0.0
  | ManichaeanTaw -- ^ @since 0.1.0.0
  | ManichaeanTen -- ^ @since 0.1.0.0
  | ManichaeanTeth -- ^ @since 0.1.0.0
  | ManichaeanThamedh -- ^ @since 0.1.0.0
  | ManichaeanTwenty -- ^ @since 0.1.0.0
  | ManichaeanWaw -- ^ @since 0.1.0.0
  | ManichaeanYodh -- ^ @since 0.1.0.0
  | ManichaeanZayin -- ^ @since 0.1.0.0
  | Meem -- ^ @since 0.1.0.0
  | Mim -- ^ @since 0.1.0.0
  | Noon -- ^ @since 0.1.0.0
  | Nun -- ^ @since 0.1.0.0
  | Nya -- ^ @since 0.1.0.0
  | Pe -- ^ @since 0.1.0.0
  | Qaf -- ^ @since 0.1.0.0
  | Qaph -- ^ @since 0.1.0.0
  | Reh -- ^ @since 0.1.0.0
  | ReversedPe -- ^ @since 0.1.0.0
  | RohingyaYeh -- ^ @since 0.1.0.0
  | Sad -- ^ @since 0.1.0.0
  | Sadhe -- ^ @since 0.1.0.0
  | Seen -- ^ @since 0.1.0.0
  | Semkath -- ^ @since 0.1.0.0
  | Shin -- ^ @since 0.1.0.0
  | StraightWaw -- ^ @since 0.1.0.0
  | SwashKaf -- ^ @since 0.1.0.0
  | SyriacWaw -- ^ @since 0.1.0.0
  | Tah -- ^ @since 0.1.0.0
  | Taw -- ^ @since 0.1.0.0
  | TehMarbuta -- ^ @since 0.1.0.0
  | TehMarbutaGoal -- ^ @since 0.1.0.0
  | Teth -- ^ @since 0.1.0.0
  | Waw -- ^ @since 0.1.0.0
  | Yeh -- ^ @since 0.1.0.0
  | YehBarree -- ^ @since 0.1.0.0
  | YehWithTail -- ^ @since 0.1.0.0
  | Yudh -- ^ @since 0.1.0.0
  | YudhHe -- ^ @since 0.1.0.0
  | Zain -- ^ @since 0.1.0.0
  | Zhain -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty JoiningGroup where
  abbreviatedPropertyValueName jg =
    case jg of
      AfricanFeh -> "African_Feh"
      AfricanNoon -> "African_Noon"
      AfricanQaf -> "African_Qaf"
      Ain -> "Ain"
      Alaph -> "Alaph"
      Alef -> "Alef"
      Beh -> "Beh"
      Beth -> "Beth"
      BurushaskiYehBarree -> "Burushaski_Yeh_Barree"
      Dal -> "Dal"
      DalathRish -> "Dalath_Rish"
      E -> "E"
      FarsiYeh -> "Farsi_Yeh"
      Fe -> "Fe"
      Feh -> "Feh"
      FinalSemkath -> "Final_Semkath"
      Gaf -> "Gaf"
      Gamal -> "Gamal"
      Hah -> "Hah"
      HanifiRohingyaKinnaYa -> "Hanifi_Rohingya_Kinna_Ya"
      HanifiRohingyaPa -> "Hanifi_Rohingya_Pa"
      He -> "He"
      Heh -> "Heh"
      HehGoal -> "Heh_Goal"
      Heth -> "Heth"
      Kaf -> "Kaf"
      Kaph -> "Kaph"
      Khaph -> "Khaph"
      KnottedHeh -> "Knotted_Heh"
      Lam -> "Lam"
      Lamadh -> "Lamadh"
      MalayalamBha -> "Malayalam_Bha"
      MalayalamJa -> "Malayalam_Ja"
      MalayalamLla -> "Malayalam_Lla"
      MalayalamLlla -> "Malayalam_Llla"
      MalayalamNga -> "Malayalam_Nga"
      MalayalamNna -> "Malayalam_Nna"
      MalayalamNnna -> "Malayalam_Nnna"
      MalayalamNya -> "Malayalam_Nya"
      MalayalamRa -> "Malayalam_Ra"
      MalayalamSsa -> "Malayalam_Ssa"
      MalayalamTta -> "Malayalam_Tta"
      ManichaeanAleph -> "Manichaean_Aleph"
      ManichaeanAyin -> "Manichaean_Ayin"
      ManichaeanBeth -> "Manichaean_Beth"
      ManichaeanDaleth -> "Manichaean_Daleth"
      ManichaeanDhamedh -> "Manichaean_Dhamedh"
      ManichaeanFive -> "Manichaean_Five"
      ManichaeanGimel -> "Manichaean_Gimel"
      ManichaeanHeth -> "Manichaean_Heth"
      ManichaeanHundred -> "Manichaean_Hundred"
      ManichaeanKaph -> "Manichaean_Kaph"
      ManichaeanLamedh -> "Manichaean_Lamedh"
      ManichaeanMem -> "Manichaean_Mem"
      ManichaeanNun -> "Manichaean_Nun"
      ManichaeanOne -> "Manichaean_One"
      ManichaeanPe -> "Manichaean_Pe"
      ManichaeanQoph -> "Manichaean_Qoph"
      ManichaeanResh -> "Manichaean_Resh"
      ManichaeanSadhe -> "Manichaean_Sadhe"
      ManichaeanSamekh -> "Manichaean_Samekh"
      ManichaeanTaw -> "Manichaean_Taw"
      ManichaeanTen -> "Manichaean_Ten"
      ManichaeanTeth -> "Manichaean_Teth"
      ManichaeanThamedh -> "Manichaean_Thamedh"
      ManichaeanTwenty -> "Manichaean_Twenty"
      ManichaeanWaw -> "Manichaean_Waw"
      ManichaeanYodh -> "Manichaean_Yodh"
      ManichaeanZayin -> "Manichaean_Zayin"
      Meem -> "Meem"
      Mim -> "Mim"
      Noon -> "Noon"
      Nun -> "Nun"
      Nya -> "Nya"
      Pe -> "Pe"
      Qaf -> "Qaf"
      Qaph -> "Qaph"
      Reh -> "Reh"
      ReversedPe -> "Reversed_Pe"
      RohingyaYeh -> "Rohingya_Yeh"
      Sad -> "Sad"
      Sadhe -> "Sadhe"
      Seen -> "Seen"
      Semkath -> "Semkath"
      Shin -> "Shin"
      StraightWaw -> "Straight_Waw"
      SwashKaf -> "Swash_Kaf"
      SyriacWaw -> "Syriac_Waw"
      Tah -> "Tah"
      Taw -> "Taw"
      TehMarbuta -> "Teh_Marbuta"
      TehMarbutaGoal -> "Teh_Marbuta_Goal"
      Teth -> "Teth"
      Waw -> "Waw"
      Yeh -> "Yeh"
      YehBarree -> "Yeh_Barree"
      YehWithTail -> "Yeh_With_Tail"
      Yudh -> "Yudh"
      YudhHe -> "Yudh_He"
      Zain -> "Zain"
      Zhain -> "Zhain"
  fullPropertyValueName TehMarbutaGoal = "Hamza_On_Heh_Goal"
  fullPropertyValueName jg = abbreviatedPropertyValueName jg

-- | See 'Data.Unistring.UCD.joiningType'
data JoiningType
  = JoinCausing -- ^ @since 0.1.0.0
  | DualJoining -- ^ @since 0.1.0.0
  | LeftJoining -- ^ @since 0.1.0.0
  | RightJoining -- ^ @since 0.1.0.0
  | Transparent -- ^ @since 0.1.0.0
  | NonJoining -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty JoiningType where
  fullPropertyValueName jg =
    case jg of
      JoinCausing -> "Join_Causing"
      DualJoining -> "Dual_Joining"
      LeftJoining -> "Left_Joining"
      RightJoining -> "Right_Joining"
      Transparent -> "Transparent"
      NonJoining -> "Non_Joining"
  abbreviatedPropertyValueName jg =
    case jg of
      JoinCausing -> "C"
      DualJoining -> "D"
      LeftJoining -> "L"
      RightJoining -> "R"
      Transparent -> "T"
      NonJoining -> "U"

-- | Default orientation in a vertical context.
--
-- See 'Data.Unistring.UCD.verticalOrientation'
data VerticalOrientation
  = Upright -- ^ @since 0.1.0.0
  | Rotated -- ^ @since 0.1.0.0
  | TransformedUpright -- ^ @since 0.1.0.0
  | TransformedRotated -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty VerticalOrientation where
  fullPropertyValueName vo =
    case vo of
      Upright -> "Upright"
      Rotated -> "Rotated"
      TransformedUpright -> "Transformed_Upright"
      TransformedRotated -> "Transformed_Rotated"
  abbreviatedPropertyValueName vo =
    case vo of
      Upright -> "U"
      Rotated -> "R"
      TransformedUpright -> "Tu"
      TransformedRotated -> "Tr"

-- | See 'Data.Unistring.UCD.lineBreak'
--
-- @since 0.1.0.0
data LineBreak
  = AmbiguousLB -- ^ @since 0.1.0.0
  | AlphabeticLB -- ^ @since 0.1.0.0
  | BreakBothLB -- ^ @since 0.1.0.0
  | BreakAfterLB -- ^ @since 0.1.0.0
  | BreakBeforeLB -- ^ @since 0.1.0.0
  | MandatoryBreakLB -- ^ @since 0.1.0.0
  | ContingentBreakLB -- ^ @since 0.1.0.0
  | ConditionalJapaneseStarterLB -- ^ @since 0.1.0.0
  | ClosePunctuationLB -- ^ @since 0.1.0.0
  | CombiningMarkLB -- ^ @since 0.1.0.0
  | CloseParenthesisLB -- ^ @since 0.1.0.0
  | CarriageReturnLB -- ^ @since 0.1.0.0
  | EBaseLB -- ^ @since 0.1.0.0
  | EModifierLB -- ^ @since 0.1.0.0
  | ExclamationLB -- ^ @since 0.1.0.0
  | GlueLB -- ^ @since 0.1.0.0
  | H2LB -- ^ @since 0.1.0.0
  | H3LB -- ^ @since 0.1.0.0
  | HebrewLetterLB -- ^ @since 0.1.0.0
  | HyphenLB -- ^ @since 0.1.0.0
  | IdeographicLB -- ^ @since 0.1.0.0
  | InseparableLB -- ^ @since 0.1.0.0
  | InfixNumericLB -- ^ @since 0.1.0.0
  | JLLB -- ^ @since 0.1.0.0
  | JTLB -- ^ @since 0.1.0.0
  | JVLB -- ^ @since 0.1.0.0
  | LineFeedLB -- ^ @since 0.1.0.0
  | NextLineLB -- ^ @since 0.1.0.0
  | NonstarterLB -- ^ @since 0.1.0.0
  | NumericLB -- ^ @since 0.1.0.0
  | OpenPunctuationLB -- ^ @since 0.1.0.0
  | PostfixNumericLB -- ^ @since 0.1.0.0
  | PrefixNumericLB -- ^ @since 0.1.0.0
  | QuotationLB -- ^ @since 0.1.0.0
  | RegionalIndicatorLB -- ^ @since 0.1.0.0
  | ComplexContextLB -- ^ @since 0.1.0.0
  | SurrogateLB -- ^ @since 0.1.0.0
  | SpaceLB -- ^ @since 0.1.0.0
  | BreakSymbolsLB -- ^ @since 0.1.0.0
  | WordJoinerLB -- ^ @since 0.1.0.0
  | UnknownLB -- ^ @since 0.1.0.0
  | ZWSpaceLB -- ^ @since 0.1.0.0
  | ZWJLB -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty LineBreak where
  fullPropertyValueName lb =
    case lb of
      AmbiguousLB -> "Ambiguous"
      AlphabeticLB -> "Alphabetic"
      BreakBothLB -> "Break_Both"
      BreakAfterLB -> "Break_After"
      BreakBeforeLB -> "Break_Before"
      MandatoryBreakLB -> "Mandatory_Break"
      ContingentBreakLB -> "Contingent_Break"
      ConditionalJapaneseStarterLB -> "Conditional_Japanese_Starter"
      ClosePunctuationLB -> "Close_Punctuation"
      CombiningMarkLB -> "Combining_Mark"
      CloseParenthesisLB -> "Close_Parenthesis"
      CarriageReturnLB -> "Carriage_Return"
      EBaseLB -> "E_Base"
      EModifierLB -> "E_Modifier"
      ExclamationLB -> "Exclamation"
      GlueLB -> "Glue"
      H2LB -> "H2"
      H3LB -> "H3"
      HebrewLetterLB -> "Hebrew_Letter"
      HyphenLB -> "Hyphen"
      IdeographicLB -> "Ideographic"
      InseparableLB -> "Inseparable"
      InfixNumericLB -> "Infix_Numeric"
      JLLB -> "JL"
      JTLB -> "JT"
      JVLB -> "JV"
      LineFeedLB -> "Line_Feed"
      NextLineLB -> "Next_Line"
      NonstarterLB -> "Nonstarter"
      NumericLB -> "Numeric"
      OpenPunctuationLB -> "Open_Punctuation"
      PostfixNumericLB -> "Postfix_Numeric"
      PrefixNumericLB -> "Prefix_Numeric"
      QuotationLB -> "Quotation"
      RegionalIndicatorLB -> "Regional_Indicator"
      ComplexContextLB -> "Complex_Context"
      SurrogateLB -> "Surrogate"
      SpaceLB -> "Space"
      BreakSymbolsLB -> "Break_Symbols"
      WordJoinerLB -> "Word_Joiner"
      UnknownLB -> "Unknown"
      ZWSpaceLB -> "ZWSpace"
      ZWJLB -> "ZWJ"
  abbreviatedPropertyValueName lb =
    case lb of
      AmbiguousLB -> "AI"
      AlphabeticLB -> "AL"
      BreakBothLB -> "B2"
      BreakAfterLB -> "BA"
      BreakBeforeLB -> "BB"
      MandatoryBreakLB -> "BK"
      ContingentBreakLB -> "CB"
      ConditionalJapaneseStarterLB -> "CJ"
      ClosePunctuationLB -> "CL"
      CombiningMarkLB -> "CM"
      CloseParenthesisLB -> "CP"
      CarriageReturnLB -> "CR"
      EBaseLB -> "EB"
      EModifierLB -> "EM"
      ExclamationLB -> "EX"
      GlueLB -> "GL"
      H2LB -> "H2"
      H3LB -> "H3"
      HebrewLetterLB -> "HL"
      HyphenLB -> "HY"
      IdeographicLB -> "ID"
      InseparableLB -> "IN"
      InfixNumericLB -> "IS"
      JLLB -> "JL"
      JTLB -> "JT"
      JVLB -> "JV"
      LineFeedLB -> "LF"
      NextLineLB -> "NL"
      NonstarterLB -> "NS"
      NumericLB -> "NU"
      OpenPunctuationLB -> "OP"
      PostfixNumericLB -> "PO"
      PrefixNumericLB -> "PR"
      QuotationLB -> "QU"
      RegionalIndicatorLB -> "RI"
      ComplexContextLB -> "SA"
      SurrogateLB -> "SG"
      SpaceLB -> "SP"
      BreakSymbolsLB -> "SY"
      WordJoinerLB -> "WJ"
      UnknownLB -> "XX"
      ZWSpaceLB -> "ZW"
      ZWJLB -> "ZWJ"

-- | See 'Data.Unistring.UCD.graphemeClusterBreak'
--
-- @since 0.1.0.0
data GraphemeClusterBreak
  = ControlGCB -- ^ @since 0.1.0.0
  | CRGCB -- ^ @since 0.1.0.0
  | ExtendGCB -- ^ @since 0.1.0.0
  | LGCB -- ^ @since 0.1.0.0
  | LFGCB -- ^ @since 0.1.0.0
  | LVGCB -- ^ @since 0.1.0.0
  | LVTGCB -- ^ @since 0.1.0.0
  | PrependGCB -- ^ @since 0.1.0.0
  | RegionalIndicatorGCB -- ^ @since 0.1.0.0
  | SpacingMarkGCB -- ^ @since 0.1.0.0
  | TGCB -- ^ @since 0.1.0.0
  | VGCB -- ^ @since 0.1.0.0
  | OtherGCB -- ^ @since 0.1.0.0
  | ZWJGCB -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty GraphemeClusterBreak where
  abbreviatedPropertyValueName gcb =
    case gcb of
      ControlGCB -> "CN"
      CRGCB -> "CR"
      ExtendGCB -> "EX"
      LGCB -> "L"
      LFGCB -> "LF"
      LVGCB -> "LV"
      LVTGCB -> "LVT"
      PrependGCB -> "PP"
      RegionalIndicatorGCB -> "RI"
      SpacingMarkGCB -> "SM"
      TGCB -> "T"
      VGCB -> "V"
      OtherGCB -> "XX"
      ZWJGCB -> "ZWJ"
  fullPropertyValueName gcb =
    case gcb of
      ControlGCB -> "Control"
      CRGCB -> "CR"
      ExtendGCB -> "Extend"
      LGCB -> "L"
      LFGCB -> "LF"
      LVGCB -> "LV"
      LVTGCB -> "LVT"
      PrependGCB -> "Prepend"
      RegionalIndicatorGCB -> "Regional_Indicator"
      SpacingMarkGCB -> "SpacingMark"
      TGCB -> "T"
      VGCB -> "V"
      OtherGCB -> "Other"
      ZWJGCB -> "ZWJ"

-- | See 'Data.Unistring.UCD.sentenceBreak'
--
-- @since 0.1.0.0
data SentenceBreak
  = ATermSB -- ^ @since 0.1.0.0
  | CloseSB -- ^ @since 0.1.0.0
  | CRSB -- ^ @since 0.1.0.0
  | ExtendSB -- ^ @since 0.1.0.0
  | FormatSB -- ^ @since 0.1.0.0
  | OLetterSB -- ^ @since 0.1.0.0
  | LFSB -- ^ @since 0.1.0.0
  | LowerSB -- ^ @since 0.1.0.0
  | NumericSB -- ^ @since 0.1.0.0
  | SContinueSB -- ^ @since 0.1.0.0
  | SepSB -- ^ @since 0.1.0.0
  | SpSB -- ^ @since 0.1.0.0
  | STermSB -- ^ @since 0.1.0.0
  | UpperSB -- ^ @since 0.1.0.0
  | OtherSB -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty SentenceBreak where
  abbreviatedPropertyValueName sb =
    case sb of
      ATermSB -> "AT"
      CloseSB -> "CL"
      CRSB -> "CR"
      ExtendSB -> "EX"
      FormatSB -> "FO"
      OLetterSB -> "LE"
      LFSB -> "LF"
      LowerSB -> "LO"
      NumericSB -> "NU"
      SContinueSB -> "SC"
      SepSB -> "SE"
      SpSB -> "SP"
      STermSB -> "ST"
      UpperSB -> "UP"
      OtherSB -> "XX"
  fullPropertyValueName sb =
    case sb of
      ATermSB -> "ATerm"
      CloseSB -> "Close"
      CRSB -> "CR"
      ExtendSB -> "Extend"
      FormatSB -> "Format"
      OLetterSB -> "OLetter"
      LFSB -> "LF"
      LowerSB -> "Lower"
      NumericSB -> "Numeric"
      SContinueSB -> "SContinue"
      SepSB -> "Sep"
      SpSB -> "Sp"
      STermSB -> "STerm"
      UpperSB -> "Upper"
      OtherSB -> "Other"

-- | See 'Data.Unistring.UCD.wordBreak'
--
-- @since 0.1.0.0
data WordBreak
  = CRWB -- ^ @since 0.1.0.0
  | DoubleQuoteWB -- ^ @since 0.1.0.0
  | ExtendNumLetWB -- ^ @since 0.1.0.0
  | ExtendWB -- ^ @since 0.1.0.0
  | FormatWB -- ^ @since 0.1.0.0
  | HebrewLetterWB -- ^ @since 0.1.0.0
  | KatakanaWB -- ^ @since 0.1.0.0
  | ALetterWB -- ^ @since 0.1.0.0
  | LFWB -- ^ @since 0.1.0.0
  | MidNumLetWB -- ^ @since 0.1.0.0
  | MidLetterWB -- ^ @since 0.1.0.0
  | MidNumWB -- ^ @since 0.1.0.0
  | NewlineWB -- ^ @since 0.1.0.0
  | NumericWB -- ^ @since 0.1.0.0
  | RegionalIndicatorWB -- ^ @since 0.1.0.0
  | SingleQuoteWB -- ^ @since 0.1.0.0
  | WSegSpaceWB -- ^ @since 0.1.0.0
  | OtherWB -- ^ @since 0.1.0.0
  | ZWJWB -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty WordBreak where
  abbreviatedPropertyValueName wb =
    case wb of
      CRWB -> "CR"
      DoubleQuoteWB -> "DQ"
      ExtendNumLetWB -> "EX"
      ExtendWB -> "Extend"
      FormatWB -> "FO"
      HebrewLetterWB -> "HL"
      KatakanaWB -> "KA"
      ALetterWB -> "LE"
      LFWB -> "LF"
      MidNumLetWB -> "MB"
      MidLetterWB -> "ML"
      MidNumWB -> "MN"
      NewlineWB -> "NL"
      NumericWB -> "NU"
      RegionalIndicatorWB -> "RI"
      SingleQuoteWB -> "SQ"
      WSegSpaceWB -> "WSegSpace"
      OtherWB -> "XX"
      ZWJWB -> "ZWJ"
  fullPropertyValueName wb =
    case wb of
      CRWB -> "CR"
      DoubleQuoteWB -> "Double_Quote"
      ExtendNumLetWB -> "ExtendNumLet"
      ExtendWB -> "Extend"
      FormatWB -> "Format"
      HebrewLetterWB -> "Hebrew_Letter"
      KatakanaWB -> "Katakana"
      ALetterWB -> "ALetter"
      LFWB -> "LF"
      MidNumLetWB -> "MidNumLet"
      MidLetterWB -> "MidLetter"
      MidNumWB -> "MidNum"
      NewlineWB -> "Newline"
      NumericWB -> "Numeric"
      RegionalIndicatorWB -> "Regional_Indicator"
      SingleQuoteWB -> "Single_Quote"
      WSegSpaceWB -> "WSegSpace"
      OtherWB -> "Other"
      ZWJWB -> "ZWJ"

-- | See 'Data.Unistring.UCD.eastAsianWidth'
--
-- @since 0.1.0.0
data EastAsianWidth
  = AmbiguousEAW -- ^ @since 0.1.0.0
  | FullwidthEAW -- ^ @since 0.1.0.0
  | HalfwidthEAW -- ^ @since 0.1.0.0
  | NeutralEAW -- ^ @since 0.1.0.0
  | NarrowEAW -- ^ @since 0.1.0.0
  | WideEAW -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty EastAsianWidth where
  abbreviatedPropertyValueName eaw =
    case eaw of
      AmbiguousEAW -> "A"
      FullwidthEAW -> "F"
      HalfwidthEAW -> "H"
      NeutralEAW -> "N"
      NarrowEAW -> "Na"
      WideEAW -> "W"
  fullPropertyValueName eaw =
    case eaw of
      AmbiguousEAW -> "Ambiguous"
      FullwidthEAW -> "Fullwidth"
      HalfwidthEAW -> "Halfwidth"
      NeutralEAW -> "Neutral"
      NarrowEAW -> "Narrow"
      WideEAW -> "Wide"

-- | Type of the parenthesis.
--
-- See also 'Data.Unistring.UCD.bidiPairedBracketType'
--
-- @since 0.1.0.0
data BidiPairedBracketType
  = Close
  -- ^ Closing parentheses, such as @)@, @]@, or @⸩@
  --
  -- @since 0.1.0.0
  | Open
  -- ^ Opening parentheses, such as @(@, @[@, or @⸨@
  --
  -- @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty BidiPairedBracketType where
  abbreviatedPropertyValueName bpbt =
    case bpbt of
      Open -> "o"
      Close -> "c"
  fullPropertyValueName bpbt =
    case bpbt of
      Open -> "Open"
      Close -> "Close"

-- | See 'Data.Unistring.UCD.indicPositionalCategory'
--
-- @since 0.1.0.0
data IndicPositionalCategory
  = BottomIPC -- ^ @since 0.1.0.0
  | BottomAndLeftIPC -- ^ @since 0.1.0.0
  | BottomAndRightIPC -- ^ @since 0.1.0.0
  | LeftIPC -- ^ @since 0.1.0.0
  | LeftAndRightIPC -- ^ @since 0.1.0.0
  | OverstruckIPC -- ^ @since 0.1.0.0
  | RightIPC -- ^ @since 0.1.0.0
  | TopIPC -- ^ @since 0.1.0.0
  | TopAndBottomIPC -- ^ @since 0.1.0.0
  | TopAndBottomAndRightIPC -- ^ @since 0.1.0.0
  | TopAndLeftIPC -- ^ @since 0.1.0.0
  | TopAndLeftAndRightIPC -- ^ @since 0.1.0.0
  | TopAndRightIPC -- ^ @since 0.1.0.0
  | VisualOrderLeftIPC -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty IndicPositionalCategory where
  fullPropertyValueName ipc =
    case ipc of
      BottomIPC -> "Bottom"
      BottomAndLeftIPC -> "Bottom_And_Left"
      BottomAndRightIPC -> "Bottom_And_Right"
      LeftIPC -> "Left"
      LeftAndRightIPC -> "Left_And_Right"
      OverstruckIPC -> "Overstruck"
      RightIPC -> "Right"
      TopIPC -> "Top"
      TopAndBottomIPC -> "Top_And_Bottom"
      TopAndBottomAndRightIPC -> "Top_And_Bottom_And_Right"
      TopAndLeftIPC -> "Top_And_Left"
      TopAndLeftAndRightIPC -> "Top_And_Left_And_Right"
      TopAndRightIPC -> "Top_And_Right"
      VisualOrderLeftIPC -> "Visual_Order_Left"
  abbreviatedPropertyValueName = fullPropertyValueName

-- | See 'Data.Unistring.UCD.indicSyllabicCategory'
--
-- @since 0.1.0.0
data IndicSyllabicCategory
  = Avagraha -- ^ @since 0.1.0.0
  | Bindu -- ^ @since 0.1.0.0
  | BrahmiJoiningNumber -- ^ @since 0.1.0.0
  | CantillationMark -- ^ @since 0.1.0.0
  | Consonant -- ^ @since 0.1.0.0
  | ConsonantDead -- ^ @since 0.1.0.0
  | ConsonantFinal -- ^ @since 0.1.0.0
  | ConsonantHeadLetter -- ^ @since 0.1.0.0
  | ConsonantInitialPostfixed -- ^ @since 0.1.0.0
  | ConsonantKiller -- ^ @since 0.1.0.0
  | ConsonantMedial -- ^ @since 0.1.0.0
  | ConsonantPlaceholder -- ^ @since 0.1.0.0
  | ConsonantPrecedingRepha -- ^ @since 0.1.0.0
  | ConsonantPrefixed -- ^ @since 0.1.0.0
  | ConsonantSubjoined -- ^ @since 0.1.0.0
  | ConsonantSucceedingRepha -- ^ @since 0.1.0.0
  | ConsonantWithStacker -- ^ @since 0.1.0.0
  | GeminationMark -- ^ @since 0.1.0.0
  | InvisibleStacker -- ^ @since 0.1.0.0
  | Joiner -- ^ @since 0.1.0.0
  | ModifyingLetter -- ^ @since 0.1.0.0
  | NonJoiner -- ^ @since 0.1.0.0
  | Nukta -- ^ @since 0.1.0.0
  | Number -- ^ @since 0.1.0.0
  | NumberJoiner -- ^ @since 0.1.0.0
  | Other -- ^ @since 0.1.0.0
  | PureKiller -- ^ @since 0.1.0.0
  | RegisterShifter -- ^ @since 0.1.0.0
  | SyllableModifier -- ^ @since 0.1.0.0
  | ToneLetter -- ^ @since 0.1.0.0
  | ToneMark -- ^ @since 0.1.0.0
  | Virama -- ^ @since 0.1.0.0
  | Visarga -- ^ @since 0.1.0.0
  | Vowel -- ^ @since 0.1.0.0
  | VowelDependent -- ^ @since 0.1.0.0
  | VowelIndependent -- ^ @since 0.1.0.0
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

instance EnumeratedProperty IndicSyllabicCategory where
  fullPropertyValueName isc =
    case isc of
      Avagraha -> "Avagraha"
      Bindu -> "Bindu"
      BrahmiJoiningNumber -> "Brahmi_Joining_Number"
      CantillationMark -> "Cantillation_Mark"
      Consonant -> "Consonant"
      ConsonantDead -> "Consonant_Dead"
      ConsonantFinal -> "Consonant_Final"
      ConsonantHeadLetter -> "Consonant_Head_Letter"
      ConsonantInitialPostfixed -> "Consonant_Initial_Postfixed"
      ConsonantKiller -> "Consonant_Killer"
      ConsonantMedial -> "Consonant_Medial"
      ConsonantPlaceholder -> "Consonant_Placeholder"
      ConsonantPrecedingRepha -> "Consonant_Preceding_Repha"
      ConsonantPrefixed -> "Consonant_Prefixed"
      ConsonantSubjoined -> "Consonant_Subjoined"
      ConsonantSucceedingRepha -> "Consonant_Succeeding_Repha"
      ConsonantWithStacker -> "Consonant_With_Stacker"
      GeminationMark -> "Gemination_Mark"
      InvisibleStacker -> "Invisible_Stacker"
      Joiner -> "Joiner"
      ModifyingLetter -> "Modifying_Letter"
      NonJoiner -> "Non_Joiner"
      Nukta -> "Nukta"
      Number -> "Number"
      NumberJoiner -> "Number_Joiner"
      Other -> "Other"
      PureKiller -> "Pure_Killer"
      RegisterShifter -> "Register_Shifter"
      SyllableModifier -> "Syllable_Modifier"
      ToneLetter -> "Tone_Letter"
      ToneMark -> "Tone_Mark"
      Virama -> "Virama"
      Visarga -> "Visarga"
      Vowel -> "Vowel"
      VowelDependent -> "Vowel_Dependent"
      VowelIndependent -> "Vowel_Independent"
  abbreviatedPropertyValueName = fullPropertyValueName
