{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))
import System.Exit (exitFailure)
import Test.HUnit

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests =
        TestList
          [ TestLabel "Properties" $
            TestList
              [ generalCategory
              , canonicalCombiningClass
              , nameAliases
              , ages
              , script
              , scriptExts
              , propList
              , derivedCoreProps
              , hangulSyllableType
              ]
          , TestLabel "Names" $
            TestList
              [ testFullNames @UCD.Block "Block"
              , testFullNames @UCD.Script "Script"
              ]
          ]
  results <- runTestTT tests
  when (errors results + failures results /= 0) exitFailure

generalCategory :: Test
generalCategory =
  testEnum "General category" "general_category" UCD.generalCategory

canonicalCombiningClass :: Test
canonicalCombiningClass =
  TestLabel "Canonical combining class" $
  TestCase $ do
    reference <-
      readFullTable
        A.decimal
        "generated/test_data/canonical_combining_class.txt"
    assertEqual "Table size" (fromEnum maxCp - fromEnum minCp + 1) $
      length reference
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refCCC) ->
      assertEqual (show cp) refCCC $ UCD.canonicalCombiningClass cp

nameAliases :: Test
nameAliases =
  TestLabel "Name aliases" $
  TestCase $ do
    reference <- readFullTable parser "generated/test_data/name_aliases.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refNA) ->
      assertEqual (show cp) (sort refNA) $ UCD.nameAliases cp
  where
    parser :: A.Parser [(UCD.NameAliasType, B.ByteString)]
    parser =
      enclosedP "[" "]" $
      enclosedP
        "("
        ")"
        ((,) <$> enumP <* "," <*> enclosedP "\"" "\"" (A.takeWhile (/= '"'))) `A.sepBy`
      ","

ages :: Test
ages =
  TestLabel "Ages" $
  TestCase $ do
    reference <- readFullTable mayEnumP "generated/test_data/age.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, ref) ->
      assertEqual (show cp) ref $ UCD.age cp

hangulSyllableType :: Test
hangulSyllableType =
  TestLabel "Hangul syllable type" $
  TestCase $ do
    reference <-
      readFullTable mayEnumP "generated/test_data/hangul_syllable_type.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, ref) ->
      assertEqual (show cp) ref $ UCD.hangulSyllableType cp

script :: Test
script = testEnum "Script" "script" UCD.script

scriptExts :: Test
scriptExts =
  TestLabel "Script extensions (raw)" $
  TestCase $ do
    reference <- readFullTable parser "generated/test_data/script_exts.txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, refSE) ->
      assertEqual (show cp) (sort refSE) $ sort $ UCD.scriptExtensionsRaw cp
  where
    parser = enclosedP "[" "]" $ enumP `A.sepBy` ","

propList :: Test
propList =
  TestList
    [ testEnum "White space" "white_space" UCD.whiteSpace
    , testEnum "Bidi control" "bidi_control" UCD.bidiControl
    , testEnum "Join control" "join_control" UCD.joinControl
    , testEnum "Dash" "dash" UCD.dash
    , testEnum "Quotation mark" "quotation_mark" UCD.quotationMark
    , testEnum
        "Terminal punctuation"
        "terminal_punctuation"
        UCD.terminalPunctuation
    , testEnum "Hex digit" "hex_digit" UCD.hexDigit
    , testEnum "ASCII hex digit" "ascii_hex_digit" UCD.asciiHexDigit
    , testEnum "Ideographic" "ideographic" UCD.ideographic
    , testEnum "Diacritic" "diacritic" UCD.diacritic
    , testEnum "Extender" "extender" UCD.extender
    , testEnum
        "Noncharacter code point"
        "noncharacter_code_point"
        UCD.noncharacterCodePoint
    , testEnum "IDS binary operator" "ids_binary_operator" UCD.idsBinaryOperator
    , testEnum
        "IDS trinary operator"
        "ids_trinary_operator"
        UCD.idsTrinaryOperator
    , testEnum "Radical" "radical" UCD.radical
    , testEnum "Unified ideograph" "unified_ideograph" UCD.unifiedIdeograph
    , testEnum "Deprecated" "deprecated" UCD.deprecated
    , testEnum "Soft dotted" "soft_dotted" UCD.softDotted
    , testEnum
        "Logical order exception"
        "logical_order_exception"
        UCD.logicalOrderException
    , testEnum "Sentence terminal" "sentence_terminal" UCD.sentenceTerminal
    , testEnum "Variation selector" "variation_selector" UCD.variationSelector
    , testEnum "Pattern white space" "pattern_white_space" UCD.patternWhiteSpace
    , testEnum "Pattern syntax" "pattern_syntax" UCD.patternSyntax
    , testEnum
        "Prepended concatenation mark"
        "prepended_concatenation_mark"
        UCD.prependedConcatenationMark
    , testEnum "Regional indicator" "regional_indicator" UCD.regionalIndicator
    ]

derivedCoreProps :: Test
derivedCoreProps =
  TestList
    [ testEnum "Math" "math" UCD.math
    , testEnum "Alphabetic" "alphabetic" UCD.alphabetic
    , testEnum "Lowercase" "lowercase" UCD.lowercase
    , testEnum "Uppercase" "uppercase" UCD.uppercase
    , testEnum "Cased" "cased" UCD.cased
    , testEnum "Case ignorable" "case_ignorable" UCD.caseIgnorable
    , testEnum
        "Changes when lowercased"
        "changes_when_lowercased"
        UCD.changesWhenLowercased
    , testEnum
        "Changes when uppercased"
        "changes_when_uppercased"
        UCD.changesWhenUppercased
    , testEnum
        "Changes when titlecased"
        "changes_when_titlecased"
        UCD.changesWhenTitlecased
    , testEnum
        "Changes when casefolded"
        "changes_when_casefolded"
        UCD.changesWhenCasefolded
    , testEnum
        "Changes when casemapped"
        "changes_when_casemapped"
        UCD.changesWhenCasemapped
    , testEnum "ID start" "id_start" UCD.idStart
    , testEnum "ID continue" "id_continue" UCD.idContinue
    , testEnum "XID start" "xid_start" UCD.xidStart
    , testEnum "XID continue" "xid_continue" UCD.xidContinue
    , testEnum
        "Default ignorable code point"
        "default_ignorable_code_point"
        UCD.defaultIgnorableCodePoint
    , testEnum "Grapheme extend" "grapheme_extend" UCD.graphemeExtend
    , testEnum "Grapheme base" "grapheme_base" UCD.graphemeBase
    ]

testFullNames ::
     forall p. (Show p, UCD.EnumeratedProperty p)
  => String
  -> Test
testFullNames name =
  TestLabel name $
  TestCase $
  for_ [minBound @p .. maxBound @p] $ \pval ->
    let pstr =
          map toLower . filter (\c -> c /= '_' && c /= '-') . B.unpack $
          UCD.fullPropertyValueName pval
        sstr = map toLower $ show pval
     in assertEqual (show pval) (take (length pstr) sstr) pstr

testEnum ::
     (Enum e, Show e, Bounded e, Eq e)
  => String
  -> FilePath
  -> (UCD.CodePoint -> e)
  -> Test
testEnum name file f =
  TestLabel name $
  TestCase $ do
    reference <- readFullTable enumP $ "generated/test_data/" ++ file ++ ".txt"
    for_ (zip [minCp .. maxCp] reference) $ \(cp, ref) ->
      assertEqual (show cp) ref $ f cp

maxCp :: UCD.CodePoint
maxCp = maxBound

minCp :: UCD.CodePoint
minCp = minBound

readFullTable :: A.Parser a -> FilePath -> IO [a]
readFullTable p file = do
  txt <- B.readFile file
  let parsed = A.parseOnly (replicateM 0x110000 (p <* A.char '\n')) txt
  case parsed of
    Left err -> assertFailure err
    Right vals -> pure vals

enumP :: (Enum a, Show a, Bounded a) => A.Parser a
enumP =
  A.choice $
  map (\(e, str) -> e <$ A.string str) $
  sortOn (Down . B.length . snd) $
  map (\e -> (e, B.pack (show e))) [minBound .. maxBound]

mayEnumP :: (Enum a, Show a, Bounded a) => A.Parser (Maybe a)
mayEnumP = Nothing <$ "Nothing" <|> Just <$> ("Just " *> enumP)

enclosedP :: A.Parser a -> A.Parser b -> A.Parser c -> A.Parser c
enclosedP start end p = start *> p <* end
