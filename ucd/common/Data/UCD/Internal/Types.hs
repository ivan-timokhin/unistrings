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

module Data.UCD.Internal.Types
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

class (Enum p, Bounded p) =>
      EnumeratedProperty p
  where
  fullPropertyValueName :: p -> ByteString
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

data Age
  = V1_1
  | V2_0
  | V2_1
  | V3_0
  | V3_1
  | V3_2
  | V4_0
  | V4_1
  | V5_0
  | V5_1
  | V5_2
  | V6_0
  | V6_1
  | V6_2
  | V6_3
  | V7_0
  | V8_0
  | V9_0
  | V10_0
  | V11_0
  | V12_0
  | V12_1
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

data BidiClass
  = LeftToRightBC
  | RightToLeftBC
  | ArabicLetterBC
  | EuropeanNumberBC
  | EuropeanSeparatorBC
  | EuropeanTerminatorBC
  | ArabicNumberBC
  | CommonSeparatorBC
  | NonspacingMarkBC
  | BoundaryNeutralBC
  | ParagraphSeparatorBC
  | SegmentSeparatorBC
  | WhiteSpaceBC
  | OtherNeutralBC
  | LeftToRightEmbeddingBC
  | LeftToRightOverrideBC
  | RightToLeftEmbeddingBC
  | RightToLeftOverrideBC
  | PopDirectionalFormatBC
  | LeftToRightIsolateBC
  | RightToLeftIsolateBC
  | FirstStrongIsolateBC
  | PopDirectionalIsolateBC
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

data HangulSyllableType
  = LeadingJamo
  | VowelJamo
  | TrailingJamo
  | LVSyllable
  | LVTSyllable
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

data NameAliasType
  = CorrectionAlias
  | ControlAlias
  | AlternateAlias
  | FigmentAlias
  | AbbreviationAlias
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Generic, Ix)

data Block
  = BasicLatinBlock
  | Latin1SupplementBlock
  | LatinExtendedABlock
  | LatinExtendedBBlock
  | IPAExtensionsBlock
  | SpacingModifierLettersBlock
  | CombiningDiacriticalMarksBlock
  | GreekAndCopticBlock
  | CyrillicBlock
  | CyrillicSupplementBlock
  | ArmenianBlock
  | HebrewBlock
  | ArabicBlock
  | SyriacBlock
  | ArabicSupplementBlock
  | ThaanaBlock
  | NKoBlock
  | SamaritanBlock
  | MandaicBlock
  | SyriacSupplementBlock
  | ArabicExtendedABlock
  | DevanagariBlock
  | BengaliBlock
  | GurmukhiBlock
  | GujaratiBlock
  | OriyaBlock
  | TamilBlock
  | TeluguBlock
  | KannadaBlock
  | MalayalamBlock
  | SinhalaBlock
  | ThaiBlock
  | LaoBlock
  | TibetanBlock
  | MyanmarBlock
  | GeorgianBlock
  | HangulJamoBlock
  | EthiopicBlock
  | EthiopicSupplementBlock
  | CherokeeBlock
  | UnifiedCanadianAboriginalSyllabicsBlock
  | OghamBlock
  | RunicBlock
  | TagalogBlock
  | HanunooBlock
  | BuhidBlock
  | TagbanwaBlock
  | KhmerBlock
  | MongolianBlock
  | UnifiedCanadianAboriginalSyllabicsExtendedBlock
  | LimbuBlock
  | TaiLeBlock
  | NewTaiLueBlock
  | KhmerSymbolsBlock
  | BugineseBlock
  | TaiThamBlock
  | CombiningDiacriticalMarksExtendedBlock
  | BalineseBlock
  | SundaneseBlock
  | BatakBlock
  | LepchaBlock
  | OlChikiBlock
  | CyrillicExtendedCBlock
  | GeorgianExtendedBlock
  | SundaneseSupplementBlock
  | VedicExtensionsBlock
  | PhoneticExtensionsBlock
  | PhoneticExtensionsSupplementBlock
  | CombiningDiacriticalMarksSupplementBlock
  | LatinExtendedAdditionalBlock
  | GreekExtendedBlock
  | GeneralPunctuationBlock
  | SuperscriptsAndSubscriptsBlock
  | CurrencySymbolsBlock
  | CombiningDiacriticalMarksForSymbolsBlock
  | LetterlikeSymbolsBlock
  | NumberFormsBlock
  | ArrowsBlock
  | MathematicalOperatorsBlock
  | MiscellaneousTechnicalBlock
  | ControlPicturesBlock
  | OpticalCharacterRecognitionBlock
  | EnclosedAlphanumericsBlock
  | BoxDrawingBlock
  | BlockElementsBlock
  | GeometricShapesBlock
  | MiscellaneousSymbolsBlock
  | DingbatsBlock
  | MiscellaneousMathematicalSymbolsABlock
  | SupplementalArrowsABlock
  | BraillePatternsBlock
  | SupplementalArrowsBBlock
  | MiscellaneousMathematicalSymbolsBBlock
  | SupplementalMathematicalOperatorsBlock
  | MiscellaneousSymbolsAndArrowsBlock
  | GlagoliticBlock
  | LatinExtendedCBlock
  | CopticBlock
  | GeorgianSupplementBlock
  | TifinaghBlock
  | EthiopicExtendedBlock
  | CyrillicExtendedABlock
  | SupplementalPunctuationBlock
  | CJKRadicalsSupplementBlock
  | KangxiRadicalsBlock
  | IdeographicDescriptionCharactersBlock
  | CJKSymbolsAndPunctuationBlock
  | HiraganaBlock
  | KatakanaBlock
  | BopomofoBlock
  | HangulCompatibilityJamoBlock
  | KanbunBlock
  | BopomofoExtendedBlock
  | CJKStrokesBlock
  | KatakanaPhoneticExtensionsBlock
  | EnclosedCJKLettersAndMonthsBlock
  | CJKCompatibilityBlock
  | CJKUnifiedIdeographsExtensionABlock
  | YijingHexagramSymbolsBlock
  | CJKUnifiedIdeographsBlock
  | YiSyllablesBlock
  | YiRadicalsBlock
  | LisuBlock
  | VaiBlock
  | CyrillicExtendedBBlock
  | BamumBlock
  | ModifierToneLettersBlock
  | LatinExtendedDBlock
  | SylotiNagriBlock
  | CommonIndicNumberFormsBlock
  | PhagsPaBlock
  | SaurashtraBlock
  | DevanagariExtendedBlock
  | KayahLiBlock
  | RejangBlock
  | HangulJamoExtendedABlock
  | JavaneseBlock
  | MyanmarExtendedBBlock
  | ChamBlock
  | MyanmarExtendedABlock
  | TaiVietBlock
  | MeeteiMayekExtensionsBlock
  | EthiopicExtendedABlock
  | LatinExtendedEBlock
  | CherokeeSupplementBlock
  | MeeteiMayekBlock
  | HangulSyllablesBlock
  | HangulJamoExtendedBBlock
  | HighSurrogatesBlock
  | HighPrivateUseSurrogatesBlock
  | LowSurrogatesBlock
  | PrivateUseAreaBlock
  | CJKCompatibilityIdeographsBlock
  | AlphabeticPresentationFormsBlock
  | ArabicPresentationFormsABlock
  | VariationSelectorsBlock
  | VerticalFormsBlock
  | CombiningHalfMarksBlock
  | CJKCompatibilityFormsBlock
  | SmallFormVariantsBlock
  | ArabicPresentationFormsBBlock
  | HalfwidthAndFullwidthFormsBlock
  | SpecialsBlock
  | LinearBSyllabaryBlock
  | LinearBIdeogramsBlock
  | AegeanNumbersBlock
  | AncientGreekNumbersBlock
  | AncientSymbolsBlock
  | PhaistosDiscBlock
  | LycianBlock
  | CarianBlock
  | CopticEpactNumbersBlock
  | OldItalicBlock
  | GothicBlock
  | OldPermicBlock
  | UgariticBlock
  | OldPersianBlock
  | DeseretBlock
  | ShavianBlock
  | OsmanyaBlock
  | OsageBlock
  | ElbasanBlock
  | CaucasianAlbanianBlock
  | LinearABlock
  | CypriotSyllabaryBlock
  | ImperialAramaicBlock
  | PalmyreneBlock
  | NabataeanBlock
  | HatranBlock
  | PhoenicianBlock
  | LydianBlock
  | MeroiticHieroglyphsBlock
  | MeroiticCursiveBlock
  | KharoshthiBlock
  | OldSouthArabianBlock
  | OldNorthArabianBlock
  | ManichaeanBlock
  | AvestanBlock
  | InscriptionalParthianBlock
  | InscriptionalPahlaviBlock
  | PsalterPahlaviBlock
  | OldTurkicBlock
  | OldHungarianBlock
  | HanifiRohingyaBlock
  | RumiNumeralSymbolsBlock
  | OldSogdianBlock
  | SogdianBlock
  | ElymaicBlock
  | BrahmiBlock
  | KaithiBlock
  | SoraSompengBlock
  | ChakmaBlock
  | MahajaniBlock
  | SharadaBlock
  | SinhalaArchaicNumbersBlock
  | KhojkiBlock
  | MultaniBlock
  | KhudawadiBlock
  | GranthaBlock
  | NewaBlock
  | TirhutaBlock
  | SiddhamBlock
  | ModiBlock
  | MongolianSupplementBlock
  | TakriBlock
  | AhomBlock
  | DograBlock
  | WarangCitiBlock
  | NandinagariBlock
  | ZanabazarSquareBlock
  | SoyomboBlock
  | PauCinHauBlock
  | BhaiksukiBlock
  | MarchenBlock
  | MasaramGondiBlock
  | GunjalaGondiBlock
  | MakasarBlock
  | TamilSupplementBlock
  | CuneiformBlock
  | CuneiformNumbersAndPunctuationBlock
  | EarlyDynasticCuneiformBlock
  | EgyptianHieroglyphsBlock
  | EgyptianHieroglyphFormatControlsBlock
  | AnatolianHieroglyphsBlock
  | BamumSupplementBlock
  | MroBlock
  | BassaVahBlock
  | PahawhHmongBlock
  | MedefaidrinBlock
  | MiaoBlock
  | IdeographicSymbolsAndPunctuationBlock
  | TangutBlock
  | TangutComponentsBlock
  | KanaSupplementBlock
  | KanaExtendedABlock
  | SmallKanaExtensionBlock
  | NushuBlock
  | DuployanBlock
  | ShorthandFormatControlsBlock
  | ByzantineMusicalSymbolsBlock
  | MusicalSymbolsBlock
  | AncientGreekMusicalNotationBlock
  | MayanNumeralsBlock
  | TaiXuanJingSymbolsBlock
  | CountingRodNumeralsBlock
  | MathematicalAlphanumericSymbolsBlock
  | SuttonSignWritingBlock
  | GlagoliticSupplementBlock
  | NyiakengPuachueHmongBlock
  | WanchoBlock
  | MendeKikakuiBlock
  | AdlamBlock
  | IndicSiyaqNumbersBlock
  | OttomanSiyaqNumbersBlock
  | ArabicMathematicalAlphabeticSymbolsBlock
  | MahjongTilesBlock
  | DominoTilesBlock
  | PlayingCardsBlock
  | EnclosedAlphanumericSupplementBlock
  | EnclosedIdeographicSupplementBlock
  | MiscellaneousSymbolsAndPictographsBlock
  | EmoticonsBlock
  | OrnamentalDingbatsBlock
  | TransportAndMapSymbolsBlock
  | AlchemicalSymbolsBlock
  | GeometricShapesExtendedBlock
  | SupplementalArrowsCBlock
  | SupplementalSymbolsAndPictographsBlock
  | ChessSymbolsBlock
  | SymbolsAndPictographsExtendedABlock
  | CJKUnifiedIdeographsExtensionBBlock
  | CJKUnifiedIdeographsExtensionCBlock
  | CJKUnifiedIdeographsExtensionDBlock
  | CJKUnifiedIdeographsExtensionEBlock
  | CJKUnifiedIdeographsExtensionFBlock
  | CJKCompatibilityIdeographsSupplementBlock
  | TagsBlock
  | VariationSelectorsSupplementBlock
  | SupplementaryPrivateUseAreaABlock
  | SupplementaryPrivateUseAreaBBlock
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

data Script
  = AdlamScript
  | AhomScript
  | AnatolianHieroglyphsScript
  | ArabicScript
  | ArmenianScript
  | AvestanScript
  | BalineseScript
  | BamumScript
  | BassaVahScript
  | BatakScript
  | BengaliScript
  | BhaiksukiScript
  | BopomofoScript
  | BrahmiScript
  | BrailleScript
  | BugineseScript
  | BuhidScript
  | CanadianAboriginalScript
  | CarianScript
  | CaucasianAlbanianScript
  | ChakmaScript
  | ChamScript
  | CherokeeScript
  | CommonScript
  | CopticScript
  | CuneiformScript
  | CypriotScript
  | CyrillicScript
  | DeseretScript
  | DevanagariScript
  | DograScript
  | DuployanScript
  | EgyptianHieroglyphsScript
  | ElbasanScript
  | ElymaicScript
  | EthiopicScript
  | GeorgianScript
  | GlagoliticScript
  | GothicScript
  | GranthaScript
  | GreekScript
  | GujaratiScript
  | GunjalaGondiScript
  | GurmukhiScript
  | HanScript
  | HangulScript
  | HanifiRohingyaScript
  | HanunooScript
  | HatranScript
  | HebrewScript
  | HiraganaScript
  | ImperialAramaicScript
  | InheritedScript
  | InscriptionalPahlaviScript
  | InscriptionalParthianScript
  | JavaneseScript
  | KaithiScript
  | KannadaScript
  | KatakanaScript
  | KayahLiScript
  | KharoshthiScript
  | KhmerScript
  | KhojkiScript
  | KhudawadiScript
  | LaoScript
  | LatinScript
  | LepchaScript
  | LimbuScript
  | LinearAScript
  | LinearBScript
  | LisuScript
  | LycianScript
  | LydianScript
  | MahajaniScript
  | MakasarScript
  | MalayalamScript
  | MandaicScript
  | ManichaeanScript
  | MarchenScript
  | MasaramGondiScript
  | MedefaidrinScript
  | MeeteiMayekScript
  | MendeKikakuiScript
  | MeroiticCursiveScript
  | MeroiticHieroglyphsScript
  | MiaoScript
  | ModiScript
  | MongolianScript
  | MroScript
  | MultaniScript
  | MyanmarScript
  | NabataeanScript
  | NandinagariScript
  | NewaScript
  | NewTaiLueScript
  | NkoScript
  | NushuScript
  | NyiakengPuachueHmongScript
  | OghamScript
  | OlChikiScript
  | OldHungarianScript
  | OldItalicScript
  | OldNorthArabianScript
  | OldPermicScript
  | OldPersianScript
  | OldSogdianScript
  | OldSouthArabianScript
  | OldTurkicScript
  | OriyaScript
  | OsageScript
  | OsmanyaScript
  | PahawhHmongScript
  | PalmyreneScript
  | PauCinHauScript
  | PhagsPaScript
  | PhoenicianScript
  | PsalterPahlaviScript
  | RejangScript
  | RunicScript
  | SamaritanScript
  | SaurashtraScript
  | SharadaScript
  | ShavianScript
  | SiddhamScript
  | SignWritingScript
  | SinhalaScript
  | SogdianScript
  | SoraSompengScript
  | SoyomboScript
  | SundaneseScript
  | SylotiNagriScript
  | SyriacScript
  | TagalogScript
  | TagbanwaScript
  | TaiLeScript
  | TaiThamScript
  | TaiVietScript
  | TakriScript
  | TamilScript
  | TangutScript
  | TeluguScript
  | ThaanaScript
  | ThaiScript
  | TibetanScript
  | TifinaghScript
  | TirhutaScript
  | UgariticScript
  | UnknownScript
  | VaiScript
  | WanchoScript
  | WarangCitiScript
  | YiScript
  | ZanabazarSquareScript
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

data DecompositionType
  = Canonical
  | Compat
  | Circle
  | Final
  | Font
  | Fraction
  | Initial
  | Isolated
  | Medial
  | Narrow
  | NoBreak
  | Small
  | Square
  | Sub
  | Super
  | Vertical
  | Wide
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

data JoiningGroup
  = AfricanFeh
  | AfricanNoon
  | AfricanQaf
  | Ain
  | Alaph
  | Alef
  | Beh
  | Beth
  | BurushaskiYehBarree
  | Dal
  | DalathRish
  | E
  | FarsiYeh
  | Fe
  | Feh
  | FinalSemkath
  | Gaf
  | Gamal
  | Hah
  | HanifiRohingyaKinnaYa
  | HanifiRohingyaPa
  | He
  | Heh
  | HehGoal
  | Heth
  | Kaf
  | Kaph
  | Khaph
  | KnottedHeh
  | Lam
  | Lamadh
  | MalayalamBha
  | MalayalamJa
  | MalayalamLla
  | MalayalamLlla
  | MalayalamNga
  | MalayalamNna
  | MalayalamNnna
  | MalayalamNya
  | MalayalamRa
  | MalayalamSsa
  | MalayalamTta
  | ManichaeanAleph
  | ManichaeanAyin
  | ManichaeanBeth
  | ManichaeanDaleth
  | ManichaeanDhamedh
  | ManichaeanFive
  | ManichaeanGimel
  | ManichaeanHeth
  | ManichaeanHundred
  | ManichaeanKaph
  | ManichaeanLamedh
  | ManichaeanMem
  | ManichaeanNun
  | ManichaeanOne
  | ManichaeanPe
  | ManichaeanQoph
  | ManichaeanResh
  | ManichaeanSadhe
  | ManichaeanSamekh
  | ManichaeanTaw
  | ManichaeanTen
  | ManichaeanTeth
  | ManichaeanThamedh
  | ManichaeanTwenty
  | ManichaeanWaw
  | ManichaeanYodh
  | ManichaeanZayin
  | Meem
  | Mim
  | Noon
  | Nun
  | Nya
  | Pe
  | Qaf
  | Qaph
  | Reh
  | ReversedPe
  | RohingyaYeh
  | Sad
  | Sadhe
  | Seen
  | Semkath
  | Shin
  | StraightWaw
  | SwashKaf
  | SyriacWaw
  | Tah
  | Taw
  | TehMarbuta
  | TehMarbutaGoal
  | Teth
  | Waw
  | Yeh
  | YehBarree
  | YehWithTail
  | Yudh
  | YudhHe
  | Zain
  | Zhain
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

data JoiningType
  = JoinCausing
  | DualJoining
  | LeftJoining
  | RightJoining
  | Transparent
  | NonJoining
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

data VerticalOrientation
  = Upright
  | Rotated
  | TransformedUpright
  | TransformedRotated
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

data LineBreak
  = AmbiguousLB
  | AlphabeticLB
  | BreakBothLB
  | BreakAfterLB
  | BreakBeforeLB
  | MandatoryBreakLB
  | ContingentBreakLB
  | ConditionalJapaneseStarterLB
  | ClosePunctuationLB
  | CombiningMarkLB
  | CloseParenthesisLB
  | CarriageReturnLB
  | EBaseLB
  | EModifierLB
  | ExclamationLB
  | GlueLB
  | H2LB
  | H3LB
  | HebrewLetterLB
  | HyphenLB
  | IdeographicLB
  | InseparableLB
  | InfixNumericLB
  | JLLB
  | JTLB
  | JVLB
  | LineFeedLB
  | NextLineLB
  | NonstarterLB
  | NumericLB
  | OpenPunctuationLB
  | PostfixNumericLB
  | PrefixNumericLB
  | QuotationLB
  | RegionalIndicatorLB
  | ComplexContextLB
  | SurrogateLB
  | SpaceLB
  | BreakSymbolsLB
  | WordJoinerLB
  | UnknownLB
  | ZWSpaceLB
  | ZWJLB
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

data GraphemeClusterBreak
  = ControlGCB
  | CRGCB
  | ExtendGCB
  | LGCB
  | LFGCB
  | LVGCB
  | LVTGCB
  | PrependGCB
  | RegionalIndicatorGCB
  | SpacingMarkGCB
  | TGCB
  | VGCB
  | OtherGCB
  | ZWJGCB
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

data SentenceBreak
  = ATermSB
  | CloseSB
  | CRSB
  | ExtendSB
  | FormatSB
  | OLetterSB
  | LFSB
  | LowerSB
  | NumericSB
  | SContinueSB
  | SepSB
  | SpSB
  | STermSB
  | UpperSB
  | OtherSB
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

data WordBreak
  = CRWB
  | DoubleQuoteWB
  | ExtendNumLetWB
  | ExtendWB
  | FormatWB
  | HebrewLetterWB
  | KatakanaWB
  | ALetterWB
  | LFWB
  | MidNumLetWB
  | MidLetterWB
  | MidNumWB
  | NewlineWB
  | NumericWB
  | RegionalIndicatorWB
  | SingleQuoteWB
  | WSegSpaceWB
  | OtherWB
  | ZWJWB
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

data EastAsianWidth
  = AmbiguousEAW
  | FullwidthEAW
  | HalfwidthEAW
  | NeutralEAW
  | NarrowEAW
  | WideEAW
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

data BidiPairedBracketType
  = Close
  | Open
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

data IndicPositionalCategory
  = BottomIPC
  | BottomAndLeftIPC
  | BottomAndRightIPC
  | LeftIPC
  | LeftAndRightIPC
  | OverstruckIPC
  | RightIPC
  | TopIPC
  | TopAndBottomIPC
  | TopAndBottomAndRightIPC
  | TopAndLeftIPC
  | TopAndLeftAndRightIPC
  | TopAndRightIPC
  | VisualOrderLeftIPC
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

data IndicSyllabicCategory
  = Avagraha
  | Bindu
  | BrahmiJoiningNumber
  | CantillationMark
  | Consonant
  | ConsonantDead
  | ConsonantFinal
  | ConsonantHeadLetter
  | ConsonantInitialPostfixed
  | ConsonantKiller
  | ConsonantMedial
  | ConsonantPlaceholder
  | ConsonantPrecedingRepha
  | ConsonantPrefixed
  | ConsonantSubjoined
  | ConsonantSucceedingRepha
  | ConsonantWithStacker
  | GeminationMark
  | InvisibleStacker
  | Joiner
  | ModifyingLetter
  | NonJoiner
  | Nukta
  | Number
  | NumberJoiner
  | Other
  | PureKiller
  | RegisterShifter
  | SyllableModifier
  | ToneLetter
  | ToneMark
  | Virama
  | Visarga
  | Vowel
  | VowelDependent
  | VowelIndependent
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
