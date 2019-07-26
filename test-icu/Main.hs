module Main where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Foldable (for_)
import qualified Data.Text.ICU.Char as ICU
import Numeric (showHex)
import System.Exit (exitFailure)
import Test.HUnit

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests =
        TestList
          [ generalCategory
          , canonicalCombiningClass
          , charName
          , propList
          , derivedCoreProps
          , numeric
          ]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

canonicalCombiningClass :: Test
canonicalCombiningClass =
  compareForAll
    "Canonical combining class"
    (ICU.property ICU.CanonicalCombiningClass)
    (fromIntegral . UCD.canonicalCombiningClass)

generalCategory :: Test
generalCategory =
  compareForAll
    "General category"
    (icu2ucd . ICU.property ICU.GeneralCategory)
    UCD.generalCategory
  where
    icu2ucd :: ICU.GeneralCategory -> UCD.GeneralCategory
    icu2ucd c =
      case c of
        ICU.GeneralOtherType -> UCD.NotAssigned
        ICU.UppercaseLetter -> UCD.UppercaseLetter
        ICU.LowercaseLetter -> UCD.LowercaseLetter
        ICU.TitlecaseLetter -> UCD.TitlecaseLetter
        ICU.ModifierLetter -> UCD.ModifierLetter
        ICU.OtherLetter -> UCD.OtherLetter
        ICU.NonSpacingMark -> UCD.NonSpacingMark
        ICU.EnclosingMark -> UCD.EnclosingMark
        ICU.CombiningSpacingMark -> UCD.SpacingCombiningMark
        ICU.DecimalDigitNumber -> UCD.DecimalNumber
        ICU.LetterNumber -> UCD.LetterNumber
        ICU.OtherNumber -> UCD.OtherNumber
        ICU.SpaceSeparator -> UCD.Space
        ICU.LineSeparator -> UCD.LineSeparator
        ICU.ParagraphSeparator -> UCD.ParagraphSeparator
        ICU.ControlChar -> UCD.Control
        ICU.FormatChar -> UCD.Format
        ICU.PrivateUseChar -> UCD.PrivateUse
        ICU.Surrogate -> UCD.Surrogate
        ICU.DashPunctuation -> UCD.DashPunctuation
        ICU.StartPunctuation -> UCD.OpenPunctuation
        ICU.EndPunctuation -> UCD.ClosePunctuation
        ICU.ConnectorPunctuation -> UCD.ConnectorPunctuation
        ICU.OtherPunctuation -> UCD.OtherPunctuation
        ICU.MathSymbol -> UCD.MathSymbol
        ICU.CurrencySymbol -> UCD.CurrencySymbol
        ICU.ModifierSymbol -> UCD.ModifierSymbol
        ICU.OtherSymbol -> UCD.OtherSymbol
        ICU.InitialPunctuation -> UCD.InitialQuote
        ICU.FinalPunctuation -> UCD.FinalQuote

charName :: Test
charName = compareForAll "Name" (B.pack . ICU.charName) UCD.name

hangulSyllableType :: Test
hangulSyllableType =
  compareForAll
    "Hangul syllable type"
    (fmap icu2ucd . ICU.property ICU.HangulSyllableType)
    UCD.hangulSyllableType
  where
    icu2ucd :: ICU.HangulSyllableType -> UCD.HangulSyllableType
    icu2ucd hst =
      case hst of
        ICU.LeadingJamo -> UCD.LeadingJamo
        ICU.VowelJamo -> UCD.VowelJamo
        ICU.TrailingJamo -> UCD.TrailingJamo
        ICU.LVSyllable -> UCD.LVSyllable
        ICU.LVTSyllable -> UCD.LVTSyllable

propList :: Test
propList =
  TestList
    [ mkBoolTest "White space" ICU.WhiteSpace UCD.whiteSpace
    , mkBoolTest "Bidi control" ICU.BidiControl UCD.bidiControl
    , mkBoolTest "Join control" ICU.JoinControl UCD.joinControl
    , mkBoolTest "Dash" ICU.Dash UCD.dash
    , mkBoolTest "Quotation mark" ICU.QuotationMark UCD.quotationMark
    , mkBoolTest
        "Terminal punctuation"
        ICU.TerminalPunctuation
        UCD.terminalPunctuation
    , mkBoolTest "Hex digit" ICU.HexDigit UCD.hexDigit
    , mkBoolTest "ASCII hex digit" ICU.ASCIIHexDigit UCD.asciiHexDigit
    , mkBoolTest "Ideographic" ICU.Ideographic UCD.ideographic
    , mkBoolTest "Diacritic" ICU.Diacritic UCD.diacritic
    , mkBoolTest "Extender" ICU.Extender UCD.extender
    , mkBoolTest
        "Noncharacter code point"
        ICU.NonCharacter
        UCD.noncharacterCodePoint
    , mkBoolTest
        "IDS binary operator"
        ICU.IDSBinaryOperator
        UCD.idsBinaryOperator
    , mkBoolTest
        "IDS trinary operator"
        ICU.IDSTrinaryOperator
        UCD.idsTrinaryOperator
    , mkBoolTest "Radical" ICU.Radical UCD.radical
    , mkBoolTest "Unified ideograph" ICU.UnifiedIdeograph UCD.unifiedIdeograph
    , mkBoolTest "Deprecated" ICU.Deprecated UCD.deprecated
    , mkBoolTest "Soft dotted" ICU.SoftDotted UCD.softDotted
    , mkBoolTest
        "Logical order exception"
        ICU.LogicalOrderException
        UCD.logicalOrderException
    , mkBoolTest "Sentence terminal" ICU.STerm UCD.sentenceTerminal
    , mkBoolTest
        "Variation selector"
        ICU.VariationSelector
        UCD.variationSelector
    , mkBoolTest
        "Pattern white space"
        ICU.PatternWhiteSpace
        UCD.patternWhiteSpace
    , mkBoolTest "Pattern syntax" ICU.PatternSyntax UCD.patternSyntax
    ]

derivedCoreProps :: Test
derivedCoreProps =
  TestList
    [ mkBoolTest "Math" ICU.Math UCD.math
    , mkBoolTest "Alphabetic" ICU.Alphabetic UCD.alphabetic
    , mkBoolTest "Uppercase" ICU.Uppercase UCD.uppercase
    , mkBoolTest "Lowercase" ICU.Lowercase UCD.lowercase
    , mkBoolTest "ID start" ICU.IDStart UCD.idStart
    , mkBoolTest "ID continue" ICU.IDContinue UCD.idContinue
    , mkBoolTest "XID start" ICU.XidStart UCD.xidStart
    , mkBoolTest "XID continue" ICU.XidContinue UCD.xidContinue
    , mkBoolTest
        "Default ignorable"
        ICU.DefaultIgnorable
        UCD.defaultIgnorableCodePoint
    , mkBoolTest "Grapheme extend" ICU.GraphemeExtend UCD.graphemeExtend
    , mkBoolTest "Grapheme base" ICU.GraphemeBase UCD.graphemeBase
    ]

numeric :: Test
numeric =
  TestLabel "Numeric" $
  TestList
    [ compareForAll
        "Type"
        (ICU.property ICU.NumericType)
        (fmap ucd2icuType . UCD.numeric)
    , compareForAll "Value" ICU.numericValue (fmap ucd2icuVal . UCD.numeric)
    ]
  where
    ucd2icuType (UCD.Decimal _) = ICU.NTDecimal
    ucd2icuType (UCD.Digit _) = ICU.NTDigit
    ucd2icuType (UCD.Numeric _) = ICU.NTNumeric
    ucd2icuVal (UCD.Decimal n) = fromIntegral n
    ucd2icuVal (UCD.Digit n) = fromIntegral n
    ucd2icuVal (UCD.Numeric q) = fromRational $ toRational q

mkBoolTest :: String -> ICU.Bool_ -> (Char -> Bool) -> Test
mkBoolTest name prop = compareForAll name (ICU.property prop)

compareForAll :: (Show a, Eq a) => String -> (Char -> a) -> (Char -> a) -> Test
compareForAll name icuQuery ucdQuery =
  TestLabel name $
  TestCase $
  for_ [minBound .. maxBound] $ \c ->
    assertEqual (showHex (ord c) "") (icuQuery c) (ucdQuery c)
