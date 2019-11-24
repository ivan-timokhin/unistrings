{-# LANGUAGE OverloadedStrings #-}

module UCD.DerivedNumericType where

import Control.Applicative ((<|>))

import UCD.Common (Table, fetchSimple)

data NumericType
  = Decimal
  | Digit
  | Numeric

fetch :: IO (Table () () NumericType)
fetch =
  fetchSimple "data/latest/ucd/extracted/DerivedNumericType.txt" $
  Decimal <$ "Decimal" <|> Digit <$ "Digit" <|> Numeric <$ "Numeric"
