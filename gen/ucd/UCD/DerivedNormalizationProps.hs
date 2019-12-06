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
{-# LANGUAGE OverloadedStrings #-}

module UCD.DerivedNormalizationProps
  ( fetch
  , Contents(..)
  ) where

import Control.Applicative ((<|>), many)
import Data.ByteString.Char8 (ByteString)
import Data.Functor (void)
import qualified Data.Vector as V
import Data.Word (Word32)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common
  ( Parser_
  , Table(Table)
  , comments
  , fetchGeneral
  , range
  , semicolon
  , tableToVector
  )

fetch :: IO Contents
fetch = fetchGeneral "DerivedNormalizationProps.txt" parser

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

parser :: Parser_ Contents
parser = do
  comments
  skipTable "FC_NFKC"
  fce <- tableParser $ True <$ MB.string "Full_Composition_Exclusion"
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
      _ <- MB.string "NFKC_CF;"
      MB.space
      many (MBL.hexadecimal <* MB.space1)
  cnfkcCF <- tableParser $ True <$ MB.string "Changes_When_NFKC_Casefolded"
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

skipTable :: ByteString -> Parser_ ()
skipTable name =
  void $
  many $
  M.try $ do
    comments
    _ <- range
    _ <- MB.string name
    M.takeWhileP Nothing (/= 0x23) -- '#'

decomposedQC :: ByteString -> Parser_ (Table () () Bool)
decomposedQC name =
  tableParser $ do
    _ <- MB.string name
    _ <- MB.string "; N"
    pure False

composedQC :: ByteString -> Parser_ (Table () () (Maybe Bool))
composedQC name =
  tableParser $ do
    _ <- MB.string name
    semicolon
    MB.space
    Just False <$ MB.string "N" <|> Nothing <$ MB.string "M"

tableParser :: Parser_ a -> Parser_ (Table () () a)
tableParser p = do
  comments
  Table <$> many record
  where
    record =
      M.try $ do
        rng <- range
        v <- p
        MB.space
        comments
        pure $ v <$ rng
