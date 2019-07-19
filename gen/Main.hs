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
import UCD.Common (tableToVector)
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
         generateTests "age" ages
         generateSources fullPartitionings "age" $
           fmap (maybe 0 (succ . fromEnum)) ages
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
           [ "white_space" ~> UCD.PropList.whiteSpace
           , "bidi_control" ~> UCD.PropList.bidiControl
           , "join_control" ~> UCD.PropList.joinControl
           , "dash" ~> UCD.PropList.dash
           , "hyphen" ~> UCD.PropList.hyphen
           , "quotation_mark" ~> UCD.PropList.quotationMark
           , "terminal_punctuation" ~> UCD.PropList.terminalPunctuation
           , "hex_digit" ~> UCD.PropList.hexDigit
           , "ascii_hex_digit" ~> UCD.PropList.asciiHexDigit
           , "ideographic" ~> UCD.PropList.ideographic
           , "diacritic" ~> UCD.PropList.diacritic
           , "extender" ~> UCD.PropList.extender
           , "noncharacter_code_point" ~> UCD.PropList.noncharacterCodePoint
           , "ids_binary_operator" ~> UCD.PropList.idsBinaryOperator
           , "ids_trinary_operator" ~> UCD.PropList.idsTrinaryOperator
           , "radical" ~> UCD.PropList.radical
           , "unified_ideograph" ~> UCD.PropList.unifiedIdeograph
           , "deprecated" ~> UCD.PropList.deprecated
           , "soft_dotted" ~> UCD.PropList.softDotted
           , "logical_order_exception" ~> UCD.PropList.logicalOrderException
           , "sentence_terminal" ~> UCD.PropList.sentenceTerminal
           , "variation_selector" ~> UCD.PropList.variationSelector
           , "pattern_white_space" ~> UCD.PropList.patternWhiteSpace
           , "pattern_syntax" ~> UCD.PropList.patternSyntax
           , "prepended_concatenation_mark" ~>
             UCD.PropList.prependedConcatenationMark
           , "regional_indicator" ~> UCD.PropList.regionalIndicator
           ]
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
