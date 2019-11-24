module UCD.DerivedNumericValues where

import Control.Applicative ((<|>))
import Data.Int (Int64)
import Data.Ratio (Ratio, (%))
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common (Table, char8, fetchSimple, semicolon)

fetch :: IO (Table () () (Ratio Int64))
fetch =
  fetchSimple "extracted/DerivedNumericValues.txt" $ do
    _ <- MBL.signed (pure ()) MBL.scientific
    MB.space
    semicolon
    MB.space
    semicolon
    MB.space
    numerator <- MBL.signed (pure ()) MBL.decimal
    denominator <- (char8 '/' *> MBL.decimal) <|> pure 1
    pure $ numerator % denominator
