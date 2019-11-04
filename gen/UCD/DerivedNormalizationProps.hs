{-# LANGUAGE OverloadedStrings #-}

module UCD.DerivedNormalizationProps
  ( fetch
  , Contents(..)
  ) where

import Control.Applicative ((<|>), many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Functor (void)
import qualified Data.Vector as V
import Data.Word (Word32)

import UCD.Common
  ( Table
  , comments
  , fetchGeneral
  , range
  , tableParser
  , tableToVector
  )

fetch :: IO Contents
fetch = fetchGeneral "data/latest/ucd/DerivedNormalizationProps.txt" parser

data Contents =
  Contents
    { fullCompositionExclusion :: Table () () Bool
    , nfdQuickCheck :: V.Vector Bool
    , nfcQuickCheck :: V.Vector (Maybe Bool)
    , nfkdQuickCheck :: V.Vector Bool
    , nfkcQuickCheck :: V.Vector (Maybe Bool)
    , nfkcCaseFold :: Table () () [Word32]
    , changesWhenNFKCCaseFolded :: Table () () Bool
    }
  deriving (Show)

parser :: A.Parser Contents
parser = do
  comments
  skipTable "FC_NFKC"
  fce <- tableParser $ True <$ A.string "Full_Composition_Exclusion"
  nfdQC <- decomposedQC "NFD_QC"
  nfcQC <- composedQC "NFC_QC"
  nfkdQC <- decomposedQC "NFKD_QC"
  nfkcQC <- composedQC "NFKC_QC"
  skipTable "Expands_On_NFD"
  skipTable "Expands_On_NFC"
  skipTable "Expands_On_NFKD"
  skipTable "Expands_On_NFKC"
  nfkcCF <-
    tableParser $ do
      _ <- A.string "NFKC_CF;"
      many (A.skipSpace *> A.hexadecimal)
  cnfkcCF <- tableParser $ True <$ A.string "Changes_When_NFKC_Casefolded"
  pure $
    Contents
      { fullCompositionExclusion = fce
      , nfdQuickCheck = tableToVector True nfdQC
      , nfcQuickCheck = tableToVector (Just True) nfcQC
      , nfkdQuickCheck = tableToVector True nfkdQC
      , nfkcQuickCheck = tableToVector (Just True) nfkcQC
      , nfkcCaseFold = nfkcCF
      , changesWhenNFKCCaseFolded = cnfkcCF
      }

skipTable :: ByteString -> A.Parser ()
skipTable name =
  void $
  many $ do
    _ <- range
    _ <- A.string name
    A.skipWhile (/= '#')
    comments

decomposedQC :: ByteString -> A.Parser (Table () () Bool)
decomposedQC name =
  tableParser $ do
    _ <- A.string name
    _ <- A.string "; N"
    pure False

composedQC :: ByteString -> A.Parser (Table () () (Maybe Bool))
composedQC name =
  tableParser $ do
    _ <- A.string name
    _ <- A.char ';'
    A.skipSpace
    Just False <$ A.string "N" <|> Nothing <$ A.string "M"
