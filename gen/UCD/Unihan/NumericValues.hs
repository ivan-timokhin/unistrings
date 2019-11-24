{-# LANGUAGE OverloadedStrings #-}

module UCD.Unihan.NumericValues where

import Control.Applicative ((<|>), many)
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common (Parser_, Range(Single), Table(Table), comments, fetchGeneral)

fetch :: IO (Table () annR Integer)
fetch = fetchGeneral "data/latest/ucd/Unihan_NumericValues.txt" parser

parser :: Parser_ (Table () annR Integer)
parser = do
  comments
  records <- many (record <* MB.eol)
  comments
  pure $ Table records

record :: Parser_ (Range () annR Integer)
record = do
  _ <- "U+"
  cp <- MBL.hexadecimal
  MB.space
  _ <- "k" *> ("Accounting" <|> "Other" <|> "Primary") <* "Numeric"
  MB.space
  Single cp () <$> MBL.decimal
