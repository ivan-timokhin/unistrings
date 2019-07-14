{-# LANGUAGE OverloadedStrings #-}

module UCD.Age where

import Control.Applicative ((<|>), many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)

import UCD.Common (Range(Range, Single), Table(Table), comment, comments)

data Age =
  Age
    { ageMajor :: Word8
    , ageMinor :: Word8
    }
  deriving (Eq, Show, Ord)

fetch :: IO (Table () () Age)
fetch = do
  txt <- B.readFile "data/latest/ucd/DerivedAge.txt"
  case A.parseOnly (parser <* A.endOfInput) txt of
    Left err -> fail err
    Right ages -> pure ages

parser :: A.Parser (Table () () Age)
parser = do
  comments
  Table <$> many (record <* A.char '\n' <* comments)

record :: A.Parser (Range () () Age)
record = do
  rng <- range
  A.skipSpace
  major <- A.decimal
  _ <- A.char '.'
  minor <- A.decimal
  A.skipSpace
  comment
  pure $ Age major minor <$ rng

range :: A.Parser (Range () () ())
range = do
  start <- A.hexadecimal
  rng <- fullRange start <|> pure (Single start () ())
  A.skipSpace
  _ <- A.char ';'
  pure rng
  where
    fullRange start = do
      _ <- A.string ".."
      end <- A.hexadecimal
      pure $ Range start end () ()
