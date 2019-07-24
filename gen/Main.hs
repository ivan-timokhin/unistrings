{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent.Async (concurrently_, mapConcurrently_)
import Data.Char (GeneralCategory(NotAssigned))
import Data.Foldable (for_)
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Directory (createDirectoryIfMissing)

import Data.UCD.Internal.Types (Script(UnknownScript))
import Driver
  ( generateASCIITableSources
  , generateASCIIVectorTableSources
  , generateSources
  , generateTests
  , processTable
  )
import ListM (ListM(Nil), generatePartitionings)
import qualified UCD.Age
import qualified UCD.Blocks
import UCD.Common (adjustWith, dropNothing, tableToVector, unicodeTableSize)
import qualified UCD.DerivedCoreProperties as UCD.DCP
import qualified UCD.HangulSyllableType
import qualified UCD.Jamo
import qualified UCD.NameAliases
import qualified UCD.PropList
import qualified UCD.ScriptExtensions
import qualified UCD.Scripts
import qualified UCD.UnicodeData

main :: IO ()
main = do
  records <- UCD.UnicodeData.fetch
  createDirectoryIfMissing True "generated/cbits"
  createDirectoryIfMissing True "generated/hs/Data/UCD/Internal"
  createDirectoryIfMissing True "generated/test_data"
  let fullPartitionings = generatePartitionings 4 0 16
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
            let table =
                  identityMapping `adjustWith` dropNothing (fmap getter records)
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
         generateASCIITableSources [Nil] "jamo_short_name" shortNames
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
         generateSources (generatePartitionings 4 0 12) "blocks" blocks
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
