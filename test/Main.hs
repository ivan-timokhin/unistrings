{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))
import Test.Yocto

import qualified Data.UCD as UCD

main :: IO ()
main = do
  let tests =
        group_
          [ group
              "Properties"
              [ generalCategory
              , canonicalCombiningClass
              , nameAliases
              , ages
              , script
              , scriptExts
              , propList
              , derivedCoreProps
              , hangulSyllableType
              , simpleCaseMappings
              , caseMappings
              , decompositionType
              , normalFormQuickCheck
              , changesWhenNFKCCaseFolded
              , joiningType
              , joiningGroup
              , verticalOrientation
              , lineBreak
              , graphemeCluster
              , sentenceBreak
              , testEnum "Word break" "word_break" UCD.wordBreak
              , testEnum
                  "East Asian width"
                  "east_asian_width"
                  UCD.eastAsianWidth
              , testEnum "Bidi Class" "bidi_class" UCD.bidiClass
              , testEnum "Bidi mirrored" "bidi_mirrored" UCD.bidiMirrored
              , testMayEnum
                  "Bidi Paired Bracket Type"
                  "bidi_paired_bracket_type"
                  UCD.bidiPairedBracketType
              ]
          , group
              "Names"
              [ testFullNames @UCD.Block "Block"
              , testFullNames @UCD.Script "Script"
              ]
          ]
  defaultMain 100 tests

generalCategory :: Suite
generalCategory =
  testEnum "General category" "general_category" UCD.generalCategory

canonicalCombiningClass :: Suite
canonicalCombiningClass =
  mkTest
    A.decimal
    "Canonical combining class"
    "canonical_combining_class"
    UCD.canonicalCombiningClass

nameAliases :: Suite
nameAliases =
  mkTest (sort <$> parser) "Name aliases" "name_aliases" UCD.nameAliases
  where
    parser :: A.Parser [(UCD.NameAliasType, B.ByteString)]
    parser =
      enclosedP "[" "]" $
      enclosedP
        "("
        ")"
        ((,) <$> enumP <* "," <*> enclosedP "\"" "\"" (A.takeWhile (/= '"'))) `A.sepBy`
      ","

ages :: Suite
ages = testMayEnum "Ages" "age" UCD.age

hangulSyllableType :: Suite
hangulSyllableType =
  testMayEnum
    "Hangul syllable type"
    "hangul_syllable_type"
    UCD.hangulSyllableType

script :: Suite
script = testEnum "Script" "script" UCD.script

scriptExts :: Suite
scriptExts =
  mkTest
    (sort <$> parser)
    "Script extensions (raw)"
    "script_exts"
    (sort . UCD.scriptExtensionsRaw)
  where
    parser = enclosedP "[" "]" $ enumP `A.sepBy` ","

propList :: Suite
propList =
  group_
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

derivedCoreProps :: Suite
derivedCoreProps =
  group_
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

simpleCaseMappings :: Suite
simpleCaseMappings =
  group
    "Simple case mappings"
    [ testCP "Lowercase" "simple_lowercase_mapping" UCD.simpleLowercaseMapping
    , testCP "Uppercase" "simple_uppercase_mapping" UCD.simpleUppercaseMapping
    , testCP "Titlecase" "simple_titlecase_mapping" UCD.simpleTitlecaseMapping
    , testCP "Case folding" "simple_case_folding" UCD.simpleCaseFolding
    ]

caseMappings :: Suite
caseMappings =
  group
    "Full case mappings"
    [ testCM
        "Uppercase"
        "special_uppercase_mapping"
        UCD.simpleUppercaseMapping
        UCD.uppercaseMapping
    , testCM
        "Lowercase"
        "special_lowercase_mapping"
        UCD.simpleLowercaseMapping
        UCD.lowercaseMapping
    , testCM
        "Titlecase"
        "special_titlecase_mapping"
        UCD.simpleTitlecaseMapping
        UCD.titlecaseMapping
    , testCM
        "Case folding"
        "full_case_folding"
        UCD.simpleCaseFolding
        UCD.caseFolding
    ]
  where
    testCM name file sf f =
      withT_ $ do
        reference <-
          readFullTable
            (enclosedP "[" "]" $ A.decimal `A.sepBy` ",")
            ("generated/test_data/" ++ file ++ ".txt")
        pure $
          group name $
          zipWith
            (\cp ref ->
               test (show cp) $
               if null ref
                 then expect "Value mismatch" $ UCD.SingleCM (sf cp) =? f cp
                 else expect "Value mismatch" $ map toEnum ref =? cm2list (f cp))
            [minCp .. maxCp]
            reference
    cm2list (UCD.SingleCM c) = [c]
    cm2list (UCD.DoubleCM c1 c2) = [c1, c2]
    cm2list (UCD.TripleCM c1 c2 c3) = [c1, c2, c3]

decompositionType :: Suite
decompositionType =
  mkTestConditional
    (\cp -> 0xac00 <= fromEnum cp && fromEnum cp <= 0xd7a3)
    mayEnumP
    "Decomposition type"
    "decomposition_type"
    UCD.decompositionType

normalFormQuickCheck :: Suite
normalFormQuickCheck =
  group
    "Quick checks for normal form"
    [ testEnum "NFD" "nfd_quick_check" UCD.nfdQuickCheck
    , testMayEnum "NFC" "nfc_quick_check" UCD.nfcQuickCheck
    , testEnum "NFKD" "nfkd_quick_check" UCD.nfkdQuickCheck
    , testMayEnum "NFKC" "nfkc_quick_check" UCD.nfkcQuickCheck
    ]

changesWhenNFKCCaseFolded :: Suite
changesWhenNFKCCaseFolded =
  testEnum
    "Changes when NFKC casefolded"
    "changes_when_nfkc_casefolded"
    UCD.changesWhenNFKCCasefolded

joiningType :: Suite
joiningType = testEnum "Joining type" "joining_type" UCD.joiningType

joiningGroup :: Suite
joiningGroup = testMayEnum "Joining group" "joining_group" UCD.joiningGroup

verticalOrientation :: Suite
verticalOrientation =
  testEnum "Vertical orientation" "vertical_orientation" UCD.verticalOrientation

lineBreak :: Suite
lineBreak = testEnum "Line break" "line_break" UCD.lineBreak

graphemeCluster :: Suite
graphemeCluster =
  testEnum
    "Grapheme cluster break"
    "grapheme_cluster_break"
    UCD.graphemeClusterBreak

sentenceBreak :: Suite
sentenceBreak = testEnum "Sentence break" "sentence_break" UCD.sentenceBreak

testFullNames ::
     forall p. (Show p, UCD.EnumeratedProperty p)
  => String
  -> Suite
testFullNames name =
  group name $
  flip map [minBound @p .. maxBound @p] $ \pval ->
    test (show pval) $
    let pstr =
          map toLower . filter (\c -> c /= '_' && c /= '-') . B.unpack $
          UCD.fullPropertyValueName pval
        sstr = map toLower $ show pval
     in expect "Name mismatch" $ take (length pstr) sstr =? pstr

testEnum ::
     (Enum e, Show e, Bounded e, Eq e)
  => String
  -> FilePath
  -> (UCD.CodePoint -> e)
  -> Suite
testEnum = mkTest enumP

testMayEnum ::
     (Enum e, Show e, Bounded e, Eq e)
  => String
  -> FilePath
  -> (UCD.CodePoint -> Maybe e)
  -> Suite
testMayEnum = mkTest mayEnumP

testCP :: String -> FilePath -> (UCD.CodePoint -> UCD.CodePoint) -> Suite
testCP = mkTest $ toEnum <$> A.decimal

mkTest ::
     (Show a, Eq a)
  => A.Parser a
  -> String
  -> FilePath
  -> (UCD.CodePoint -> a)
  -> Suite
mkTest = mkTestConditional (const False)

mkTestConditional ::
     (Show a, Eq a)
  => (UCD.CodePoint -> Bool)
  -> A.Parser a
  -> String
  -> FilePath
  -> (UCD.CodePoint -> a)
  -> Suite
mkTestConditional exclude parser name file f =
  withT_ $ do
    reference <- readFullTable parser $ "generated/test_data/" ++ file ++ ".txt"
    pure $
      group name $
      zipWith
        (\cp ref ->
           test (show cp) $
           unless (exclude cp) $ expect "Value mismatch" $ ref =? f cp)
        [minCp .. maxCp]
        reference

maxCp :: UCD.CodePoint
maxCp = maxBound

minCp :: UCD.CodePoint
minCp = minBound

readFullTable :: A.Parser a -> FilePath -> Test [a]
readFullTable p file = do
  txt <- liftIO $ B.readFile file
  let parsed = A.parseOnly (replicateM 0x110000 (p <* A.char '\n')) txt
  case parsed of
    Left err -> criticalFailure err
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
