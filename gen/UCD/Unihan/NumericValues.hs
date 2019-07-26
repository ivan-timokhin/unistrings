{-# LANGUAGE OverloadedStrings #-}

module UCD.Unihan.NumericValues where

import Control.Applicative ((<|>), many)
import qualified Data.Attoparsec.ByteString.Char8 as A

import UCD.Common (Range(Single), Table(Table), comments, fetchGeneral)

fetch :: IO (Table () annR Integer)
fetch = fetchGeneral "data/latest/ucd/Unihan_NumericValues.txt" parser

parser :: A.Parser (Table () annR Integer)
parser = do
  comments
  records <- many (record <* A.char '\n')
  comments
  pure $ Table records

record :: A.Parser (Range () annR Integer)
record = do
  _ <- "U+"
  cp <- A.hexadecimal
  A.skipSpace
  _ <- "k" *> ("Accounting" <|> "Other" <|> "Primary") <* "Numeric"
  A.skipSpace
  Single cp () <$> A.decimal
