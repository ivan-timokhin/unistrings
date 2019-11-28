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
import Data.Int (Int32)
import Data.Ratio (denominator, numerator)
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Directory (createDirectoryIfMissing)

import Data.UCD.Internal.Types
  ( BidiClass(LeftToRightBC)
  , EastAsianWidth(NeutralEAW)
  , GraphemeClusterBreak(OtherGCB)
  , IndicSyllabicCategory(Other)
  , JoiningType(NonJoining)
  , LineBreak(UnknownLB)
  , Script(UnknownScript)
  , SentenceBreak(OtherSB)
  , VerticalOrientation(Rotated)
  , WordBreak(OtherWB)
  )
import Driver
  ( bool
  , enum
  , ffiVector
  , generateASCIITableSources
  , generateASCIIVectorTableSources
  , generateSourcesAs
  , generateTests
  , integral
  , maybeBool
  , maybeEnum
  , maybeIntegral
  , processTableAs
  , smallEnumVector
  )
import qualified UCD.Age
import qualified UCD.BidiBrackets
import qualified UCD.BidiMirroring
import qualified UCD.Blocks
import qualified UCD.CaseFolding
import UCD.Common
  ( adjustWith
  , adjustWithM
  , dropNothing
  , tableToVector
  , unicodeTableSize
  )
import qualified UCD.DerivedBidiClass
import qualified UCD.DerivedCoreProperties as UCD.DCP
import qualified UCD.DerivedJoiningGroup
import qualified UCD.DerivedJoiningType
import qualified UCD.DerivedNormalizationProps as UCD.DNP
import qualified UCD.DerivedNumericType
import qualified UCD.DerivedNumericValues
import qualified UCD.EastAsianWidth
import qualified UCD.EquivalentUnifiedIdeograph
import qualified UCD.GraphemeBreakProperty
import qualified UCD.HangulSyllableType
import qualified UCD.IndicPositionalCategory
import qualified UCD.IndicSyllabicCategory
import qualified UCD.Jamo
import qualified UCD.LineBreak
import qualified UCD.NameAliases
import qualified UCD.PropList
import qualified UCD.ScriptExtensions
import qualified UCD.Scripts
import qualified UCD.SentenceBreakProperty
import qualified UCD.SpecialCasing
import qualified UCD.UnicodeData
import qualified UCD.VerticalOrientation
import qualified UCD.WordBreakProperty

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "ucd/generated/cbits"
  createDirectoryIfMissing True "ucd/generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "ucd/generated/test_data"
  let fullPartitionings = (4, 16)
  mapConcurrently_
    id
    [ processTableAs enum fullPartitionings "general_category" $
      UCD.UnicodeData.tableToVector NotAssigned $
      fmap UCD.UnicodeData.propCategory records
    , processTableAs integral fullPartitionings "canonical_combining_class" $
      UCD.UnicodeData.tableToVector 0 $
      fmap UCD.UnicodeData.propCanonicalCombiningClass records
    , let identityMapping = V.generate unicodeTableSize fromIntegral
          processSimpleCaseMapping name getter =
            let table = identityMapping `adjustWithM` fmap getter records
                diffTable =
                  V.imap (\cp mapping -> fromIntegral mapping - cp) table
             in do generateTests name table
                   generateSourcesAs integral fullPartitionings name diffTable
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
                   fullTable :: V.Vector (V.Vector Int)
                   fullTable = tableToVector V.empty sparseTable
               generateTests name fullTable
               forConcurrently_ [0 .. maxLen - 1] $ \i ->
                 let ithCPSparse = fmap (V.!? i) sparseTable
                     ithCPFull = zeroMapping `adjustWithM` ithCPSparse
                  in generateSourcesAs
                       integral
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
               generateSourcesAs
                 integral
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
                  in generateSourcesAs
                       integral
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
    , generateASCIITableSources fullPartitionings "unicode_1_name" $
      UCD.UnicodeData.tableToVector "" $
      UCD.UnicodeData.propUnicode1Name <$> records
    , do shortNames <- UCD.Jamo.fetch
         generateASCIITableSources (0, 0) "jamo_short_name" shortNames
    , do aliases <- tableToVector V.empty <$> UCD.NameAliases.fetch
         concurrently_ (generateTests "name_aliases" aliases) $
           concurrently_
             (generateASCIIVectorTableSources
                fullPartitionings
                "name_aliases_aliases" $
              fmap (fmap snd) aliases)
             (generateSourcesAs
                ffiVector
                fullPartitionings
                "name_aliases_types"
                (fmap (fmap ((toEnum :: Int -> Word8) . fromEnum . fst)) aliases))
    , do blocks <- UCD.Blocks.fetch
         generateSourcesAs maybeEnum (4, 12) "blocks" blocks
    , do ages <- UCD.Common.tableToVector Nothing . fmap Just <$> UCD.Age.fetch
         processTableAs maybeEnum fullPartitionings "age" ages
    , do scripts <- UCD.Common.tableToVector UnknownScript <$> UCD.Scripts.fetch
         processTableAs enum fullPartitionings "script" scripts
    , do scriptExts <-
           UCD.Common.tableToVector V.empty <$> UCD.ScriptExtensions.fetch
         concurrently_
           (concurrently_
              (generateSourcesAs
                 smallEnumVector
                 fullPartitionings
                 "script_exts_ptr"
                 scriptExts)
              (generateSourcesAs integral fullPartitionings "script_exts_len" $
               fmap V.length scriptExts))
           (generateTests "script_exts" scriptExts)
    , do props <- UCD.PropList.fetch
         let processProp snakeName getter =
               processTableAs bool fullPartitionings snakeName $
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
               generateTests
                 snakeName
                 (UCD.Common.tableToVector False (getter props) :: V.Vector Bool)
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
               processTableAs bool fullPartitionings snakeName $
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
         processTableAs maybeEnum fullPartitionings "hangul_syllable_type" hst
    , do nt <- UCD.DerivedNumericType.fetch
         let typesV =
               UCD.Common.tableToVector (0 :: Word8) $
               nt <&> \case
                 UCD.DerivedNumericType.Decimal -> 1
                 UCD.DerivedNumericType.Digit -> 2
                 UCD.DerivedNumericType.Numeric -> 3
         generateSourcesAs integral fullPartitionings "numeric_type" typesV
    , do nv <- UCD.DerivedNumericValues.fetch
         let valuesTable = UCD.Common.tableToVector 0 nv
         concurrently_
           (generateSourcesAs integral fullPartitionings "numeric_numerator" $
            fmap numerator valuesTable)
           (generateSourcesAs integral fullPartitionings "numeric_denominator" $
            fmap denominator valuesTable)
    , processTableAs maybeEnum fullPartitionings "decomposition_type" $
      UCD.Common.tableToVector Nothing $
      fmap fst . UCD.UnicodeData.propDecompositionMapping <$> records
    , let canonicalDecomposition =
            UCD.UnicodeData.tableToDecompositionVector False records
       in concurrently_
            (generateSourcesAs
               ffiVector
               fullPartitionings
               "canonical_decomposition_ptr"
               canonicalDecomposition)
            (generateSourcesAs
               integral
               fullPartitionings
               "canonical_decomposition_len" $
             fmap V.length canonicalDecomposition)
    , let compatibilityDecomposition =
            UCD.UnicodeData.tableToDecompositionVector True records
       in concurrently_
            (generateSourcesAs
               ffiVector
               fullPartitionings
               "compatibility_decomposition_ptr"
               compatibilityDecomposition)
            (generateSourcesAs
               integral
               fullPartitionings
               "compatibility_decomposition_len" $
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
           (generateSourcesAs
              maybeIntegral
              fullPartitionings
              "canonical_composition_top"
              topCompositionTable)
           (generateSourcesAs
              integral
              fullPartitionings
              "canonical_composition_bottom"
              bottomCompositionTable)
         mapConcurrently_
           id
           [ processTableAs bool fullPartitionings "nfd_quick_check" $
             UCD.DNP.nfdQuickCheck nps
           , processTableAs maybeBool fullPartitionings "nfc_quick_check" $
             UCD.DNP.nfcQuickCheck nps
           , processTableAs bool fullPartitionings "nfkd_quick_check" $
             UCD.DNP.nfkdQuickCheck nps
           , processTableAs maybeBool fullPartitionings "nfkc_quick_check" $
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
           (generateSourcesAs
              integral
              fullPartitionings
              "simple_nfkc_casefold"
              simpleNFKCCF)
           (concurrently_
              (generateSourcesAs
                 ffiVector
                 fullPartitionings
                 "complex_nfkc_casefold_ptr"
                 complexNFKCCF)
              (generateSourcesAs
                 integral
                 fullPartitionings
                 "complex_nfkc_casefold_len"
                 allNFKCCFL))
         processTableAs bool fullPartitionings "changes_when_nfkc_casefolded" $
           UCD.Common.tableToVector False $
           UCD.DNP.changesWhenNFKCCaseFolded nps
    , do joiningType <- UCD.DerivedJoiningType.fetch
         processTableAs enum fullPartitionings "joining_type" $
           UCD.Common.tableToVector NonJoining joiningType
    , do joiningGroup <- UCD.DerivedJoiningGroup.fetch
         processTableAs maybeEnum fullPartitionings "joining_group" $
           UCD.Common.tableToVector Nothing $ Just <$> joiningGroup
    , do verticalOrientation <- UCD.VerticalOrientation.fetch
         processTableAs enum fullPartitionings "vertical_orientation" $
           UCD.Common.tableToVector Rotated verticalOrientation
    , do lineBreak <- UCD.LineBreak.fetch
         processTableAs enum fullPartitionings "line_break" $
           UCD.Common.tableToVector UnknownLB lineBreak
    , do graphemeClusterBreak <- UCD.GraphemeBreakProperty.fetch
         processTableAs enum fullPartitionings "grapheme_cluster_break" $
           UCD.Common.tableToVector OtherGCB graphemeClusterBreak
    , do sentenceBreak <- UCD.SentenceBreakProperty.fetch
         processTableAs enum fullPartitionings "sentence_break" $
           UCD.Common.tableToVector OtherSB sentenceBreak
    , do wordBreak <- UCD.WordBreakProperty.fetch
         processTableAs enum fullPartitionings "word_break" $
           UCD.Common.tableToVector OtherWB wordBreak
    , do eaw <- UCD.EastAsianWidth.fetch
         processTableAs enum fullPartitionings "east_asian_width" $
           UCD.Common.tableToVector NeutralEAW eaw
    , do bc <- UCD.DerivedBidiClass.fetch
         processTableAs enum fullPartitionings "bidi_class" $
           UCD.Common.tableToVector LeftToRightBC bc
    , processTableAs bool fullPartitionings "bidi_mirrored" $
      UCD.Common.tableToVector False $
      fmap UCD.UnicodeData.propBidiMirrored records
    , do bm <- UCD.BidiMirroring.fetch
         let identity = V.generate unicodeTableSize id
             withMappings = identity `adjustWith` bm
             diff = V.imap (flip (-)) withMappings
         generateSourcesAs
           integral
           fullPartitionings
           "bidi_mirroring_glyph"
           diff
    , do bb <- UCD.BidiBrackets.fetch
         let types = tableToVector Nothing $ fmap (Just . snd) bb
             pairs =
               V.imap (flip (-)) $
               V.generate unicodeTableSize id `adjustWith` fmap fst bb
         processTableAs
           maybeEnum
           fullPartitionings
           "bidi_paired_bracket_type"
           types
         generateSourcesAs
           integral
           fullPartitionings
           "bidi_paired_bracket"
           pairs
    , do eui <- UCD.EquivalentUnifiedIdeograph.fetch
         generateSourcesAs
           maybeIntegral
           fullPartitionings
           "equivalent_unified_ideograph" $
           UCD.Common.tableToVector Nothing $ fmap Just eui
    , do ipc <- UCD.IndicPositionalCategory.fetch
         processTableAs maybeEnum fullPartitionings "indic_positional_category" $
           UCD.Common.tableToVector Nothing $ fmap Just ipc
    , do isc <- UCD.IndicSyllabicCategory.fetch
         processTableAs enum fullPartitionings "indic_syllabic_category" $
           UCD.Common.tableToVector Other isc
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
