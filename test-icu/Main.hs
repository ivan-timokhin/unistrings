module Main where

import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.ICU.Char as ICU
import qualified Data.Text.ICU.Normalize as ICU
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
          , decompositionType
          , canonicalDecomposition
          , compatibilityDecomposition
          , canonicalComposition
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

decompositionType :: Test
decompositionType =
  compareForAll
    "Decomposition type"
    (fmap icu2ucd . ICU.property ICU.Decomposition)
    UCD.decompositionType
  where
    icu2ucd :: ICU.Decomposition -> UCD.DecompositionType
    icu2ucd d =
      case d of
        ICU.Canonical -> UCD.Canonical
        ICU.Compat -> UCD.Compatibility
        ICU.Circle -> UCD.Encircled
        ICU.Final -> UCD.FinalPresentationForm
        ICU.Font -> UCD.Font
        ICU.Fraction -> UCD.VulgarFraction
        ICU.Initial -> UCD.InitialPresentationForm
        ICU.Isolated -> UCD.IsolatedPresentationForm
        ICU.Medial -> UCD.MedialPresentationForm
        ICU.Narrow -> UCD.Narrow
        ICU.NoBreak -> UCD.NoBreak
        ICU.Small -> UCD.Small
        ICU.Square -> UCD.Squared
        ICU.Sub -> UCD.Subscript
        ICU.Super -> UCD.Superscript
        ICU.Vertical -> UCD.VerticalLayout
        ICU.Wide -> UCD.Wide
        ICU.Count -> error "'Count' is not actually a decomposition type"

canonicalDecomposition :: Test
canonicalDecomposition =
  mkDecompositionTest
    "Canonical decomposition"
    ICU.NFD
    UCD.canonicalDecomposition

compatibilityDecomposition :: Test
compatibilityDecomposition =
  mkDecompositionTest
    "Compatibility decomposition"
    ICU.NFKD
    UCD.compatibilityDecomposition

mkDecompositionTest ::
     String -> ICU.NormalizationMode -> (Char -> [UCD.CodePoint]) -> Test
mkDecompositionTest name mode =
  compareForAll
    name
    (\c ->
       map UCD.toCodePoint $
       case ICU.property ICU.GeneralCategory c of
         ICU.Surrogate -> [c]
         _ -> T.unpack . ICU.normalize mode $ T.singleton c)

canonicalComposition :: Test
canonicalComposition =
  TestLabel "Canonical composition" $
  TestList
    [ TestLabel "Pairs" $
      TestCase $
      for_ [minBound .. maxBound] $ \cp1 ->
        for_ (UCD.canonicalCompositionStart cp1) $ \token ->
          for_ [minBound .. maxBound] $ \cp2 ->
            for_ (UCD.canonicalCompositionFinish token cp2) $ \composed ->
              assertEqual
                (showHex (ord cp1) $ ' ' : showHex (ord cp2) "")
                (map UCD.toCodePoint $
                 T.unpack $ ICU.normalize ICU.NFC $ T.pack [cp1, cp2])
                [composed]
    , TestLabel "Decompositions" $
      TestCase $
      for_ [minBound .. maxBound] $ \cp ->
        unless (ICU.property ICU.GeneralCategory cp == ICU.Surrogate) $
        assertEqual
          (showHex (ord cp) "")
          (map UCD.toCodePoint $
           T.unpack $ ICU.normalize ICU.NFC $ T.singleton cp)
          (ucdCompose $ UCD.canonicalDecomposition cp)
    ]
  where
    ucdCompose :: [UCD.CodePoint] -> [UCD.CodePoint]
    ucdCompose [] = []
    ucdCompose [c] = [c]
    ucdCompose (c1:cs@(c2:cs')) =
      case UCD.canonicalComposition c1 c2 of
        Just c -> ucdCompose (c : cs')
        Nothing -> c1 : ucdCompose cs

mkBoolTest :: String -> ICU.Bool_ -> (Char -> Bool) -> Test
mkBoolTest name prop = compareForAll name (ICU.property prop)

compareForAll :: (Show a, Eq a) => String -> (Char -> a) -> (Char -> a) -> Test
compareForAll name icuQuery ucdQuery =
  TestLabel name $
  TestCase $
  for_ [minBound .. maxBound] $ \c ->
    assertEqual (showHex (ord c) "") (icuQuery c) (ucdQuery c)
