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
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception (evaluate)
import qualified Criterion.Main as C
import qualified Data.Char as Ch
import Data.Maybe (fromMaybe)
import qualified Data.Text.ICU.Char as ICU
import qualified Data.Vector.Unboxed as V
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, withFile)

import qualified Data.Unistring.UCD as UCD

main :: IO ()
main =
  C.defaultMain
    [ C.env readUDHR $ \udhr ->
        C.bgroup
          "UDHR"
          [ C.bgroup
              "General category"
              [ C.bench "UCD" $ mkEnumBenchmark udhr UCD.generalCategory
              , C.bench "Data.Char" $ mkEnumBenchmark udhr Ch.generalCategory
              , C.bench "ICU" $
                mkEnumBenchmark udhr (ICU.property ICU.GeneralCategory)
              ]
          , C.bgroup
              "Canonical combining class"
              [ C.bench "UCD" $
                mkIntegralBenchmark udhr UCD.canonicalCombiningClass
              , C.bench "ICU" $
                mkBenchmark udhr (ICU.property ICU.CanonicalCombiningClass)
              ]
          , C.bgroup
              "Name"
              [ C.bench "UCD" $ mkBenchmark udhr ((`seq` 0) . UCD.name)
              , C.bench "ICU" $ mkBenchmark udhr ((`seq` 0) . ICU.charName)
              ]
          , C.bgroup
              "Name aliases"
              [ C.bench "UCD" $
                mkBenchmark udhr (evalPairsList . UCD.nameAliases)
              ]
          , C.bgroup
              "Block"
              [ C.bench "UCD" $ mkBenchmark udhr (maybe 0 fromEnum . UCD.block)
              -- https://github.com/bos/text-icu/pull/37
              -- , C.bench "ICU" $ mkBenchmark udhr ICU.blockCode
              ]
          , C.bgroup
              "Hangul syllable type"
              [ C.bench "UCD" $
                mkBenchmark udhr (maybe 0 fromEnum . UCD.hangulSyllableType)
              , C.bench "ICU" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . ICU.property ICU.HangulSyllableType)
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
                  [ C.bench "UCD" $
                    mkEnumBenchmark udhr UCD.simpleLowercaseMapping
                  ]
              , C.bgroup
                  "Uppercase"
                  [ C.bench "UCD" $
                    mkEnumBenchmark udhr UCD.simpleUppercaseMapping
                  ]
              , C.bgroup
                  "Titlecase"
                  [ C.bench "UCD" $
                    mkEnumBenchmark udhr UCD.simpleTitlecaseMapping
                  ]
              ]
          , C.bgroup
              "Full case mappings"
              [ C.bgroup
                  "Lowercase"
                  [C.bench "UCD" $ mkCMBenchmark udhr UCD.lowercaseMapping]
              , C.bgroup
                  "Uppercase"
                  [C.bench "UCD" $ mkCMBenchmark udhr UCD.uppercaseMapping]
              , C.bgroup
                  "Titlecase"
                  [C.bench "UCD" $ mkCMBenchmark udhr UCD.titlecaseMapping]
              ]
          , C.bgroup
              "Case folding"
              [ C.bgroup
                  "Simple"
                  [C.bench "UCD" $ mkEnumBenchmark udhr UCD.simpleCaseFolding]
              , C.bgroup
                  "Full"
                  [C.bench "UCD" $ mkCMBenchmark udhr UCD.caseFolding]
              ]
          , C.bgroup
              "Numeric"
              [ C.bgroup
                  "Value"
                  [ C.bench "UCD" $
                    mkBenchmark udhr $ \c ->
                      case UCD.numeric c of
                        Nothing -> 0
                        Just num ->
                          case num of
                            UCD.Decimal n -> fromIntegral n
                            UCD.Digit n -> fromIntegral n
                            UCD.Numeric q -> round q
                  , C.bench "ICU" $
                    mkBenchmark udhr (round . fromMaybe 0 . ICU.numericValue)
                  ]
              , C.bgroup
                  "Type"
                  [ C.bench "UCD" $
                    mkBenchmark udhr $ \c ->
                      case UCD.numeric c of
                        Nothing -> 0
                        Just num ->
                          case num of
                            UCD.Decimal _ -> 1
                            UCD.Digit _ -> 2
                            UCD.Numeric _ -> 3
                  , C.bench "ICU" $
                    mkBenchmark
                      udhr
                      (maybe 0 fromEnum . ICU.property ICU.NumericType)
                  ]
              , C.bgroup
                  "Decimals only"
                  [ C.bench "UCD" $
                    mkBenchmark udhr $ \c ->
                      case UCD.numeric c of
                        Just (UCD.Decimal n) -> fromIntegral n
                        _ -> 0
                  , C.bench "ICU" $
                    mkBenchmark udhr $ \c ->
                      case ICU.property ICU.NumericType c of
                        Just ICU.NTDecimal -> maybe 0 round $ ICU.numericValue c
                        _ -> 0
                  ]
              ]
          , C.bgroup
              "Decomposition type"
              [ C.bench "UCD" $
                mkBenchmark udhr (maybe 0 fromEnum . UCD.decompositionType)
              , C.bench "ICU" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . ICU.property ICU.Decomposition)
              ]
          , C.bgroup
              "Quick checks"
              [ mkMBoolGroup
                  udhr
                  "NFD"
                  ICU.NFDQuickCheck
                  (Just . UCD.nfdQuickCheck)
              , mkMBoolGroup udhr "NFC" ICU.NFCQuickCheck UCD.nfcQuickCheck
              , mkMBoolGroup
                  udhr
                  "NFKD"
                  ICU.NFKDQuickCheck
                  (Just . UCD.nfkdQuickCheck)
              , mkMBoolGroup udhr "NFKC" ICU.NFKCQuickCheck UCD.nfkcQuickCheck
              ]
          , mkBoolGroup
              udhr
              "Changes when NFKC casefolded"
              Nothing
              UCD.changesWhenNFKCCasefolded
          , C.bgroup
              "Joining type"
              [ C.bench "UCD" $ mkEnumBenchmark udhr UCD.joiningType
              , C.bench "ICU" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . ICU.property ICU.JoiningType)
              ]
          , C.bgroup
              "Joining group"
              [ C.bench "UCD" $
                mkBenchmark udhr (maybe 0 fromEnum . UCD.joiningGroup)
              ]
          , C.bgroup
              "Vertical orientation"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.verticalOrientation]
          , C.bgroup
              "Line break"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.lineBreak]
          , C.bgroup
              "Grapheme cluster break"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.graphemeClusterBreak]
          , C.bgroup
              "Sentence break"
              [ C.bench "UCD" $ mkEnumBenchmark udhr UCD.sentenceBreak
              , C.bench "ICU" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . ICU.property ICU.SentenceBreak)
              ]
          , C.bgroup
              "Word break"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.wordBreak]
          , C.bgroup
              "East Asian width"
              [ C.bench "UCD" $ mkEnumBenchmark udhr UCD.eastAsianWidth
              , C.bench "ICU" $
                mkEnumBenchmark udhr (ICU.property ICU.EastAsianWidth)
              ]
          , C.bgroup
              "Bidi Class"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.bidiClass]
          , mkBoolGroup
              udhr
              "Bidi Mirrored"
              (Just ICU.BidiMirrored)
              UCD.bidiMirrored
          , C.bgroup
              "Bidi mirroring glyph"
              [ C.bench "UCD" $
                mkBenchmark udhr (maybe 0 fromEnum . UCD.bidiMirroringGlyph)
              , C.bench "ICU" $ mkEnumBenchmark udhr ICU.mirror
              ]
          , C.bgroup
              "Bidi Paired Bracket"
              [ C.bgroup
                  "Value"
                  [C.bench "UCD" $ mkEnumBenchmark udhr UCD.bidiPairedBracket]
              , C.bgroup
                  "Type"
                  [ C.bench "UCD" $
                    mkBenchmark
                      udhr
                      (maybe 0 fromEnum . UCD.bidiPairedBracketType)
                  ]
              ]
          , C.bgroup
              "Equivalent unified ideograph"
              [ C.bench "UCD" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . UCD.equivalentUnifiedIdeograph)
              ]
          , C.bgroup
              "Indic positional category"
              [ C.bench "UCD" $
                mkBenchmark
                  udhr
                  (maybe 0 fromEnum . UCD.indicPositionalCategory)
              ]
          , C.bgroup
              "Indic syllabic category"
              [C.bench "UCD" $ mkEnumBenchmark udhr UCD.indicSyllabicCategory]
          , C.bench "No-op" $ mkEnumBenchmark udhr id
          ]
    ]

mkBoolGroup ::
     V.Vector Char -> String -> Maybe ICU.Bool_ -> (Char -> Bool) -> C.Benchmark
{-# INLINE mkBoolGroup #-}
mkBoolGroup vals name mprop f =
  C.bgroup name $
  C.bench "UCD" (mkEnumBenchmark vals f) :
  case mprop of
    Nothing -> []
    Just prop -> [C.bench "ICU" $ mkEnumBenchmark vals (ICU.property prop)]

mkMBoolGroup ::
     ICU.Property p (Maybe Bool)
  => V.Vector Char
  -> String
  -> p
  -> (Char -> Maybe Bool)
  -> C.Benchmark
{-# INLINE mkMBoolGroup #-}
mkMBoolGroup vals name prop f =
  C.bgroup
    name
    [ C.bench "UCD" $ mkBenchmark vals $ maybe 0 fromEnum . f
    , C.bench "ICU" $ mkBenchmark vals $ maybe 0 fromEnum . ICU.property prop
    ]

mkEnumBenchmark :: Enum a => V.Vector Char -> (Char -> a) -> C.Benchmarkable
{-# INLINE mkEnumBenchmark #-}
mkEnumBenchmark vals f = mkBenchmark vals (fromEnum . f)

mkIntegralBenchmark ::
     Integral a => V.Vector Char -> (Char -> a) -> C.Benchmarkable
{-# INLINE mkIntegralBenchmark #-}
mkIntegralBenchmark vals f = mkBenchmark vals (fromIntegral . f)

mkCMBenchmark :: V.Vector Char -> (Char -> UCD.CaseMapping) -> C.Benchmarkable
{-# INLINE mkCMBenchmark #-}
mkCMBenchmark vals f =
  mkBenchmark vals $ \c ->
    case f c of
      UCD.SingleCM w -> fromEnum w
      UCD.DoubleCM w1 w2 -> fromEnum w1 + fromEnum w2
      UCD.TripleCM w1 w2 w3 -> fromEnum w1 + fromEnum w2 + fromEnum w3

mkBenchmark :: V.Vector Char -> (Char -> Int) -> C.Benchmarkable
{-# INLINE mkBenchmark #-}
mkBenchmark vals f = C.whnf (V.foldl' (\n c -> n + f c) 0) vals

evalPairsList :: [(a, b)] -> Int
{-# INLINE evalPairsList #-}
evalPairsList xs = go xs `seq` 0
  where
    go ((a, b):rest) = a `seq` b `seq` go rest
    go [] = ()

readUDHR :: IO (V.Vector Char)
readUDHR =
  withFile "../data/udhr/full_all.txt" ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    evaluate $ V.fromList contents
