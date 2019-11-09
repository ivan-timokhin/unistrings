{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.Async
  ( concurrently_
  , forConcurrently_
  , mapConcurrently_
  )
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory(NotAssigned))
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
import Data.Ratio (denominator, numerator)
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Directory (createDirectoryIfMissing)

import Data.UCD.Internal.Types
  ( JoiningType(NonJoining)
  , LineBreak(UnknownLB)
  , Script(UnknownScript)
  , VerticalOrientation(Rotated)
  )
import Driver
  ( generateASCIITableSources
  , generateASCIIVectorTableSources
  , generateSources
  , generateTests
  , processTable
  )
import qualified UCD.Age
import qualified UCD.Blocks
import qualified UCD.CaseFolding
import UCD.Common
  ( adjustWith
  , adjustWithM
  , dropNothing
  , tableToVector
  , unicodeTableSize
  )
import qualified UCD.DerivedCoreProperties as UCD.DCP
import qualified UCD.DerivedJoiningGroup
import qualified UCD.DerivedJoiningType
import qualified UCD.DerivedNormalizationProps as UCD.DNP
import qualified UCD.HangulSyllableType
import qualified UCD.Jamo
import qualified UCD.LineBreak
import qualified UCD.NameAliases
import qualified UCD.PropList
import qualified UCD.ScriptExtensions
import qualified UCD.Scripts
import qualified UCD.SpecialCasing
import qualified UCD.UnicodeData
import qualified UCD.Unihan.NumericValues
import qualified UCD.VerticalOrientation

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "generated/test_data"
  let fullPartitionings = (4, 16)
  mapConcurrently_
    id
    [ processTable fullPartitionings "general_category" $
      UCD.UnicodeData.tableToVector NotAssigned $
      fmap UCD.UnicodeData.propCategory records
    , processTable fullPartitionings "canonical_combining_class" $
      UCD.UnicodeData.tableToVector 0 $
      fmap UCD.UnicodeData.propCanonicalCombiningClass records
    , let identityMapping = V.generate unicodeTableSize fromIntegral
          processSimpleCaseMapping name getter =
            let table = identityMapping `adjustWithM` fmap getter records
                diffTable =
                  V.imap (\cp mapping -> fromIntegral mapping - cp) table
             in do generateTests name table
                   generateSources fullPartitionings name diffTable
       in mapConcurrently_
            id
            [ processSimpleCaseMapping
                "simple_uppercase_mapping"
                UCD.UnicodeData.propSimpleUppercaseMapping
            , processSimpleCaseMapping
                "simple_lowercase_mapping"
                UCD.UnicodeData.propSimpleLowercaseMapping
            , processSimpleCaseMapping
                "simple_titlecase_mapping"
                UCD.UnicodeData.propSimpleTitlecaseMapping
            ]
    , do special <- UCD.SpecialCasing.fetch
         let zeroMapping = V.replicate unicodeTableSize 0
             processSpecialCaseMapping maxLen name getter = do
               let sparseTable =
                     dropNothing $
                     fmap
                       (\r ->
                          let field = getter r
                           in if V.length field == 1
                                then Nothing
                                else Just field)
                       special
                   fullTable = tableToVector V.empty sparseTable
               generateTests name fullTable
               forConcurrently_ [0 .. maxLen - 1] $ \i ->
                 let ithCPSparse = fmap (V.!? i) sparseTable
                     ithCPFull = zeroMapping `adjustWithM` ithCPSparse
                  in generateSources
                       fullPartitionings
                       (name <> "_" <> B.pack (show i))
                       ithCPFull
         mapConcurrently_
           id
           [ processSpecialCaseMapping
               3
               "special_uppercase_mapping"
               UCD.SpecialCasing.upper
           , processSpecialCaseMapping
               2
               "special_lowercase_mapping"
               UCD.SpecialCasing.lower
           , processSpecialCaseMapping
               3
               "special_titlecase_mapping"
               UCD.SpecialCasing.title
           ]
    , do foldings <- UCD.CaseFolding.fetch
         let identityMapping = V.generate unicodeTableSize id
             fullSimpleTable =
               identityMapping `adjustWith` UCD.CaseFolding.common foldings `adjustWith`
               UCD.CaseFolding.simple foldings
             diffSimpleTable = V.imap (flip (-)) fullSimpleTable
             maxLen = 3
         concurrently_
           (do generateTests "simple_case_folding" fullSimpleTable
               generateSources
                 fullPartitionings
                 "simple_case_folding"
                 diffSimpleTable)
           (do generateTests "full_case_folding" $
                 V.replicate unicodeTableSize V.empty `adjustWith`
                 UCD.CaseFolding.full foldings
               forConcurrently_ [0 .. maxLen - 1] $ \i ->
                 let ithSparseTable = (V.!? i) <$> UCD.CaseFolding.full foldings
                     ithFullMap =
                       V.replicate unicodeTableSize 0 `adjustWithM`
                       ithSparseTable
                  in generateSources
                       fullPartitionings
                       ("full_case_folding_" <> B.pack (show i))
                       ithFullMap)
    , generateASCIITableSources fullPartitionings "name" $
      UCD.UnicodeData.tableToNames records V.//
      map
        (, "")
        ([0xAC00 .. 0xD7A3] ++
         [0x3400 .. 0x4DB5] ++
         [0x4E00 .. 0x9FEF] ++
         [0x20000 .. 0x2A6D6] ++
         [0x2A700 .. 0x2B734] ++
         [0x2B740 .. 0x2B81D] ++
         [0x2B820 .. 0x2CEA1] ++
         [0x2CEB0 .. 0x2EBE0] ++
         [0x17000 .. 0x187F7] ++
         [0xF900 .. 0xFA6D] ++ [0xFA70 .. 0xFAD9] ++ [0x2F800 .. 0x2FA1D])
    , do shortNames <- UCD.Jamo.fetch
         generateASCIITableSources (0, 0) "jamo_short_name" shortNames
    , do aliases <- tableToVector V.empty <$> UCD.NameAliases.fetch
         concurrently_ (generateTests "name_aliases" aliases) $
           concurrently_
             (generateASCIIVectorTableSources
                fullPartitionings
                "name_aliases_aliases" $
              fmap (fmap snd) aliases)
             (generateSources
                fullPartitionings
                "name_aliases_types"
                (fmap (fmap ((toEnum :: Int -> Word8) . fromEnum . fst)) aliases))
    , do blocks <- UCD.Blocks.fetch
         generateSources (4, 12) "blocks" blocks
    , do ages <- UCD.Common.tableToVector Nothing . fmap Just <$> UCD.Age.fetch
         processTable fullPartitionings "age" ages
    , do scripts <- UCD.Common.tableToVector UnknownScript <$> UCD.Scripts.fetch
         processTable fullPartitionings "script" scripts
    , do scriptExts <-
           UCD.Common.tableToVector V.empty <$> UCD.ScriptExtensions.fetch
         concurrently_
           (concurrently_
              (generateSources fullPartitionings "script_exts_ptr" scriptExts)
              (generateSources fullPartitionings "script_exts_len" $
               fmap V.length scriptExts))
           (generateTests "script_exts" scriptExts)
    , do props <- UCD.PropList.fetch
         let processProp snakeName getter =
               processTable fullPartitionings snakeName $
               UCD.Common.tableToVector False $ getter props
             (~>) = (,)
         mapConcurrently_
           (uncurry processProp)
           [ "bidi_control" ~> UCD.PropList.bidiControl
           , "dash" ~> UCD.PropList.dash
           , "quotation_mark" ~> UCD.PropList.quotationMark
           , "terminal_punctuation" ~> UCD.PropList.terminalPunctuation
           , "ideographic" ~> UCD.PropList.ideographic
           , "diacritic" ~> UCD.PropList.diacritic
           , "extender" ~> UCD.PropList.extender
           , "unified_ideograph" ~> UCD.PropList.unifiedIdeograph
           , "deprecated" ~> UCD.PropList.deprecated
           , "soft_dotted" ~> UCD.PropList.softDotted
           , "logical_order_exception" ~> UCD.PropList.logicalOrderException
           , "sentence_terminal" ~> UCD.PropList.sentenceTerminal
           , "pattern_syntax" ~> UCD.PropList.patternSyntax
           , "prepended_concatenation_mark" ~>
             UCD.PropList.prependedConcatenationMark
           ]
         let mkTestsProp snakeName getter =
               generateTests snakeName $
               UCD.Common.tableToVector False $ getter props
         mapConcurrently_
           (uncurry mkTestsProp)
           [ "white_space" ~> UCD.PropList.whiteSpace
           , "join_control" ~> UCD.PropList.joinControl
           , "hex_digit" ~> UCD.PropList.hexDigit
           , "ascii_hex_digit" ~> UCD.PropList.asciiHexDigit
           , "noncharacter_code_point" ~> UCD.PropList.noncharacterCodePoint
           , "ids_binary_operator" ~> UCD.PropList.idsBinaryOperator
           , "ids_trinary_operator" ~> UCD.PropList.idsTrinaryOperator
           , "radical" ~> UCD.PropList.radical
           , "variation_selector" ~> UCD.PropList.variationSelector
           , "regional_indicator" ~> UCD.PropList.regionalIndicator
           , "pattern_white_space" ~> UCD.PropList.patternWhiteSpace
           ]
    , do props <- UCD.DCP.fetch
         let processProp snakeName getter =
               processTable fullPartitionings snakeName $
               UCD.Common.tableToVector False $ getter props
             (~>) = (,)
         mapConcurrently_
           (uncurry processProp)
           [ "math" ~> UCD.DCP.math
           , "alphabetic" ~> UCD.DCP.alphabetic
           , "lowercase" ~> UCD.DCP.lowercase
           , "uppercase" ~> UCD.DCP.uppercase
           , "cased" ~> UCD.DCP.cased
           , "case_ignorable" ~> UCD.DCP.caseIgnorable
           , "changes_when_lowercased" ~> UCD.DCP.changesWhenLowercased
           , "changes_when_uppercased" ~> UCD.DCP.changesWhenUppercased
           , "changes_when_titlecased" ~> UCD.DCP.changesWhenTitlecased
           , "changes_when_casefolded" ~> UCD.DCP.changesWhenCasefolded
           , "changes_when_casemapped" ~> UCD.DCP.changesWhenCasemapped
           , "id_start" ~> UCD.DCP.idStart
           , "id_continue" ~> UCD.DCP.idContinue
           , "xid_start" ~> UCD.DCP.xidStart
           , "xid_continue" ~> UCD.DCP.xidContinue
           , "default_ignorable_code_point" ~> UCD.DCP.defaultIgnorableCodePoint
           , "grapheme_extend" ~> UCD.DCP.graphemeExtend
           , "grapheme_base" ~> UCD.DCP.graphemeBase
           ]
    , do hst <-
           UCD.Common.tableToVector Nothing . fmap Just <$>
           UCD.HangulSyllableType.fetch
         processTable fullPartitionings "hangul_syllable_type" hst
    , do unihanNumVals <- UCD.Unihan.NumericValues.fetch
         let table =
               UCD.Common.tableToVector
                 Nothing
                 (fmap UCD.UnicodeData.propNumeric records) `adjustWith`
               fmap (Just . UCD.UnicodeData.Numeric . fromInteger) unihanNumVals
             typesTable =
               flip fmap table $ \case
                 Nothing -> (0 :: Word8)
                 Just (UCD.UnicodeData.Decimal _) -> 1
                 Just (UCD.UnicodeData.Digit _) -> 2
                 Just (UCD.UnicodeData.Numeric _) -> 3
             numeratorTable =
               flip fmap table $ \case
                 Nothing -> (0 :: Int64)
                 Just (UCD.UnicodeData.Decimal n) -> fromIntegral n
                 Just (UCD.UnicodeData.Digit n) -> fromIntegral n
                 Just (UCD.UnicodeData.Numeric r) -> fromIntegral $ numerator r
             denominatorTable =
               flip fmap table $ \case
                 Just (UCD.UnicodeData.Numeric r) ->
                   fromIntegral $ denominator r
                 _ -> (1 :: Int64)
         concurrently_
           (concurrently_
              (generateSources fullPartitionings "numeric_type" typesTable)
              (generateSources
                 fullPartitionings
                 "numeric_numerator"
                 numeratorTable))
           (generateSources
              fullPartitionings
              "numeric_denominator"
              denominatorTable)
    , processTable fullPartitionings "decomposition_type" $
      UCD.Common.tableToVector Nothing $
      fmap fst . UCD.UnicodeData.propDecompositionMapping <$> records
    , let canonicalDecomposition =
            UCD.UnicodeData.tableToDecompositionVector False records
       in concurrently_
            (generateSources
               fullPartitionings
               "canonical_decomposition_ptr"
               canonicalDecomposition)
            (generateSources fullPartitionings "canonical_decomposition_len" $
             fmap V.length canonicalDecomposition)
    , let compatibilityDecomposition =
            UCD.UnicodeData.tableToDecompositionVector True records
       in concurrently_
            (generateSources
               fullPartitionings
               "compatibility_decomposition_ptr"
               compatibilityDecomposition)
            (generateSources fullPartitionings "compatibility_decomposition_len" $
             fmap V.length compatibilityDecomposition)
    , do nps <- UCD.DNP.fetch
         let fullCompositionExclusion =
               UCD.Common.tableToVector
                 False
                 (UCD.DNP.fullCompositionExclusion nps)
             (topCompositionTable, bottomCompositionTable) =
               UCD.UnicodeData.tableToCompositionTables
                 (\i -> fullCompositionExclusion V.! fromIntegral i)
                 records
         concurrently_
           (generateSources
              fullPartitionings
              "canonical_composition_top"
              topCompositionTable)
           (generateSources
              fullPartitionings
              "canonical_composition_bottom"
              bottomCompositionTable)
         mapConcurrently_
           id
           [ processTable fullPartitionings "nfd_quick_check" $
             UCD.DNP.nfdQuickCheck nps
           , processTable fullPartitionings "nfc_quick_check" $
             UCD.DNP.nfcQuickCheck nps
           , processTable fullPartitionings "nfkd_quick_check" $
             UCD.DNP.nfkdQuickCheck nps
           , processTable fullPartitionings "nfkc_quick_check" $
             UCD.DNP.nfkcQuickCheck nps
           ]
         let identityMapping = V.generate unicodeTableSize fromIntegral
             simpleNFKCCF =
               V.imap (\cp m -> fromIntegral m - cp) $
               identityMapping `adjustWithM`
               (UCD.DNP.nfkcCaseFold nps <&> \case
                  [cp] -> Just cp
                  _ -> Nothing)
             complexNFKCCF =
               tableToVector (V.empty :: V.Vector Int32) $
               dropNothing $
               UCD.DNP.nfkcCaseFold nps <&> \case
                 [_] -> Nothing
                 ws -> Just $ V.fromList $ map fromIntegral ws
             allNFKCCFL =
               tableToVector (1 :: Int) $ length <$> UCD.DNP.nfkcCaseFold nps
         concurrently_
           (generateSources
              fullPartitionings
              "simple_nfkc_casefold"
              simpleNFKCCF)
           (concurrently_
              (generateSources
                 fullPartitionings
                 "complex_nfkc_casefold_ptr"
                 complexNFKCCF)
              (generateSources
                 fullPartitionings
                 "complex_nfkc_casefold_len"
                 allNFKCCFL))
         processTable fullPartitionings "changes_when_nfkc_casefolded" $
           UCD.Common.tableToVector False $
           UCD.DNP.changesWhenNFKCCaseFolded nps
    , do joiningType <- UCD.DerivedJoiningType.fetch
         processTable fullPartitionings "joining_type" $
           UCD.Common.tableToVector NonJoining joiningType
    , do joiningGroup <- UCD.DerivedJoiningGroup.fetch
         processTable fullPartitionings "joining_group" $
           UCD.Common.tableToVector Nothing $ Just <$> joiningGroup
    , do verticalOrientation <- UCD.VerticalOrientation.fetch
         processTable fullPartitionings "vertical_orientation" $
           UCD.Common.tableToVector Rotated verticalOrientation
    , do lineBreak <- UCD.LineBreak.fetch
         processTable fullPartitionings "line_break" $
           UCD.Common.tableToVector UnknownLB lineBreak
    ]

printLong :: Show a => [a] -> IO ()
printLong entries
  | entriesCount <= 2 * magic = print entries
  | otherwise = do
    putStrLn "["
    for_ (take magic entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "\t⋮"
    for_ (drop (entriesCount - magic) entries) $ \e -> putStrLn $ '\t' : show e
    putStrLn "]"
  where
    entriesCount = length entries
    magic = 10
