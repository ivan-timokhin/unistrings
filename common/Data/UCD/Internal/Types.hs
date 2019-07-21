{-# LANGUAGE OverloadedStrings #-}

module Data.UCD.Internal.Types
  ( Age(..)
  , BidiClass(..)
  , HangulSyllableType(..)
  , NameAliasType(..)
  , Block(..)
  , Script(..)
  , EnumeratedProperty(..)
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Char (GeneralCategory(..))

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
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

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
  deriving (Eq, Ord, Show, Bounded, Enum)

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
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

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
