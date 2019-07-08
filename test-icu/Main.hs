module Main where

import Control.Monad (unless, when)
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
  let tests = TestList [generalCategory, canonicalCombiningClass, charName]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

canonicalCombiningClass :: Test
canonicalCombiningClass =
  TestLabel "Canonical combining class" $
  TestCase $
  for_ [minBound .. maxBound] $ \c ->
    assertEqual
      (showHex (ord c) "")
      (ICU.property ICU.CanonicalCombiningClass c)
      (fromIntegral $ UCD.canonicalCombiningClass c)

generalCategory :: Test
generalCategory =
  TestLabel "General category" $
  TestCase $
  for_ [minBound .. maxBound] $ \c ->
    assertEqual
      (showHex (ord c) "")
      (icu2ucd $ ICU.property ICU.GeneralCategory c)
      (UCD.generalCategory c)
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
charName =
  TestLabel "Name" $
  TestCase $
  for_ [minBound .. maxBound] $ \c ->
    assertEqual (showHex (ord c) "") (B.pack $ ICU.charName c) (UCD.name c)
