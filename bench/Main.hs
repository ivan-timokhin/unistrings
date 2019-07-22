module Main where

import Control.Exception (evaluate)
import qualified Criterion.Main as C
import qualified Data.Char as Ch
import qualified Data.Text.ICU.Char as ICU
import qualified Data.Vector.Unboxed as V
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, withFile)

import qualified Data.UCD as UCD

main :: IO ()
main =
  C.defaultMain
    [ C.env readUDHR $ \udhr ->
        C.bgroup
          "UDHR"
          [ C.bgroup
              "General category"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.generalCategory
              , C.bench "Data.Char" $ mkBenchmark udhr Ch.generalCategory
              , C.bench "ICU" $
                mkBenchmark udhr (ICU.property ICU.GeneralCategory)
              ]
          , C.bgroup
              "Canonical combining class"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.canonicalCombiningClass
              , C.bench "ICU" $
                mkBenchmark udhr (ICU.property ICU.CanonicalCombiningClass)
              ]
          , C.bgroup
              "Name"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.name
              , C.bench "ICU" $ mkBenchmark udhr ICU.charName
              ]
          , C.bgroup
              "Name aliases"
              [ C.bench "UCD" $
                mkBenchmark udhr (evalPairsList . UCD.nameAliases)
              ]
          , C.bgroup
              "Block"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.block
              -- https://github.com/bos/text-icu/pull/37
              -- , C.bench "ICU" $ mkBenchmark udhr ICU.blockCode
              ]
          , C.bgroup
              "Hangul syllable type"
              [ C.bench "UCD" $
                mkBenchmark udhr (maybe () (`seq` ()) . UCD.hangulSyllableType)
              , C.bench "ICU" $
                mkBenchmark
                  udhr
                  (maybe () (`seq` ()) . ICU.property ICU.HangulSyllableType)
              ]
          , C.bgroup
              "PropList"
              [ mkBoolGroup
                  udhr
                  "White space"
                  (Just ICU.WhiteSpace)
                  UCD.whiteSpace
              , mkBoolGroup
                  udhr
                  "Bidi control"
                  (Just ICU.BidiControl)
                  UCD.bidiControl
              , mkBoolGroup
                  udhr
                  "Join control"
                  (Just ICU.JoinControl)
                  UCD.joinControl
              , mkBoolGroup udhr "Dash" (Just ICU.Dash) UCD.dash
              , mkBoolGroup
                  udhr
                  "Quotation mark"
                  (Just ICU.QuotationMark)
                  UCD.quotationMark
              , mkBoolGroup
                  udhr
                  "Terminal punctuation"
                  (Just ICU.TerminalPunctuation)
                  UCD.terminalPunctuation
              , mkBoolGroup udhr "Hex digit" (Just ICU.HexDigit) UCD.hexDigit
              , mkBoolGroup
                  udhr
                  "ASCII hex digit"
                  (Just ICU.ASCIIHexDigit)
                  UCD.asciiHexDigit
              , mkBoolGroup
                  udhr
                  "Ideographic"
                  (Just ICU.Ideographic)
                  UCD.ideographic
              , mkBoolGroup udhr "Diacritic" (Just ICU.Diacritic) UCD.diacritic
              , mkBoolGroup udhr "Extender" (Just ICU.Extender) UCD.extender
              , mkBoolGroup
                  udhr
                  "Noncharacter code point"
                  (Just ICU.NonCharacter)
                  UCD.noncharacterCodePoint
              , mkBoolGroup
                  udhr
                  "IDS binary operator"
                  (Just ICU.IDSBinaryOperator)
                  UCD.idsBinaryOperator
              , mkBoolGroup
                  udhr
                  "IDS trinary operator"
                  (Just ICU.IDSTrinaryOperator)
                  UCD.idsTrinaryOperator
              , mkBoolGroup udhr "Radical" (Just ICU.Radical) UCD.radical
              , mkBoolGroup
                  udhr
                  "Unified ideograph"
                  (Just ICU.UnifiedIdeograph)
                  UCD.unifiedIdeograph
              , mkBoolGroup
                  udhr
                  "Deprecated"
                  (Just ICU.Deprecated)
                  UCD.deprecated
              , mkBoolGroup
                  udhr
                  "Soft dotted"
                  (Just ICU.SoftDotted)
                  UCD.softDotted
              , mkBoolGroup
                  udhr
                  "Logical order exception"
                  (Just ICU.LogicalOrderException)
                  UCD.logicalOrderException
              , mkBoolGroup
                  udhr
                  "Sentence terminal"
                  (Just ICU.STerm)
                  UCD.sentenceTerminal
              , mkBoolGroup
                  udhr
                  "Variation selector"
                  (Just ICU.VariationSelector)
                  UCD.variationSelector
              , mkBoolGroup
                  udhr
                  "Pattern white space"
                  (Just ICU.PatternWhiteSpace)
                  UCD.patternWhiteSpace
              , mkBoolGroup
                  udhr
                  "Pattern syntax"
                  (Just ICU.PatternSyntax)
                  UCD.patternSyntax
              , mkBoolGroup
                  udhr
                  "Prepended concatenation mark"
                  Nothing
                  UCD.prependedConcatenationMark
              , mkBoolGroup
                  udhr
                  "Regional indicator"
                  Nothing
                  UCD.regionalIndicator
              ]
          , C.bgroup
              "DerivedCoreProps"
              [ mkBoolGroup udhr "Math" (Just ICU.Math) UCD.math
              , mkBoolGroup
                  udhr
                  "Alphabetic"
                  (Just ICU.Alphabetic)
                  UCD.alphabetic
              , mkBoolGroup udhr "Uppercase" (Just ICU.Uppercase) UCD.uppercase
              , mkBoolGroup udhr "Lowercase" (Just ICU.Lowercase) UCD.lowercase
              , mkBoolGroup udhr "Cased" Nothing UCD.cased
              , mkBoolGroup udhr "Case ignorable" Nothing UCD.caseIgnorable
              , mkBoolGroup
                  udhr
                  "Changes when lowercased"
                  Nothing
                  UCD.changesWhenLowercased
              , mkBoolGroup
                  udhr
                  "Changes when uppercased"
                  Nothing
                  UCD.changesWhenUppercased
              , mkBoolGroup
                  udhr
                  "Changes when titlecased"
                  Nothing
                  UCD.changesWhenTitlecased
              , mkBoolGroup
                  udhr
                  "Changes when casefolded"
                  Nothing
                  UCD.changesWhenCasefolded
              , mkBoolGroup
                  udhr
                  "Changes when casemapped"
                  Nothing
                  UCD.changesWhenCasemapped
              , mkBoolGroup udhr "ID start" (Just ICU.IDStart) UCD.idStart
              , mkBoolGroup
                  udhr
                  "ID continue"
                  (Just ICU.IDContinue)
                  UCD.idContinue
              , mkBoolGroup udhr "XID start" (Just ICU.XidStart) UCD.xidStart
              , mkBoolGroup
                  udhr
                  "XID continue"
                  (Just ICU.XidContinue)
                  UCD.xidContinue
              , mkBoolGroup
                  udhr
                  "Default ignorable"
                  (Just ICU.DefaultIgnorable)
                  UCD.defaultIgnorableCodePoint
              , mkBoolGroup
                  udhr
                  "Grapheme extend"
                  (Just ICU.GraphemeExtend)
                  UCD.graphemeExtend
              , mkBoolGroup
                  udhr
                  "Grapheme base"
                  (Just ICU.GraphemeBase)
                  UCD.graphemeBase
              ]
          , C.bgroup
              "Simple case mappings"
              [ C.bgroup
                  "Lowercase"
                  [C.bench "UCD" $ mkBenchmark udhr UCD.simpleLowercaseMapping]
              , C.bgroup
                  "Uppercase"
                  [C.bench "UCD" $ mkBenchmark udhr UCD.simpleUppercaseMapping]
              , C.bgroup
                  "Titlecase"
                  [C.bench "UCD" $ mkBenchmark udhr UCD.simpleTitlecaseMapping]
              ]
          , C.bench "No-op" $ mkBenchmark udhr id
          ]
    ]

mkBoolGroup ::
     V.Vector Char -> String -> Maybe ICU.Bool_ -> (Char -> Bool) -> C.Benchmark
{-# INLINE mkBoolGroup #-}
mkBoolGroup vals name mprop f =
  C.bgroup name $
  C.bench "UCD" (mkBenchmark vals f) :
  case mprop of
    Nothing -> []
    Just prop -> [C.bench "ICU" $ mkBenchmark vals (ICU.property prop)]

mkBenchmark :: V.Vector Char -> (Char -> a) -> C.Benchmarkable
{-# INLINE mkBenchmark #-}
mkBenchmark vals f = C.nf (V.foldl' (\() c -> f c `seq` ()) ()) vals

evalPairsList :: [(a, b)] -> [(a, b)]
{-# INLINE evalPairsList #-}
evalPairsList xs = go xs `seq` xs
  where
    go ((a, b):rest) = a `seq` b `seq` go rest
    go [] = ()

readUDHR :: IO (V.Vector Char)
readUDHR =
  withFile "data/udhr/full_all.txt" ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    evaluate $ V.fromList contents
