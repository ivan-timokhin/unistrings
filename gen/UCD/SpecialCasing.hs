{-# LANGUAGE RecordWildCards #-}

module UCD.SpecialCasing
  ( fetch
  , Record(lower, upper, title)
  ) where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

import UCD.Common (Range(Single), Table(Table), comments, fetchGeneral)

fetch :: IO (Table () () Record)
fetch = fetchGeneral "data/latest/ucd/SpecialCasing.txt" parser

data Record =
  Record
    { lower :: V.Vector Int
    , upper :: V.Vector Int
    , title :: V.Vector Int
    }
  deriving (Show, Eq)

parser :: A.Parser (Table () () Record)
parser = do
  comments
  records <- catMaybes <$> many (record <* comments)
  pure $ Table records

record :: A.Parser (Maybe (Range () () Record))
record = do
  cp <- A.hexadecimal
  A.char ';' *> A.skipSpace
  lower <- V.fromList <$> many (A.hexadecimal <* A.skipSpace)
  A.char ';' *> A.skipSpace
  title <- V.fromList <$> many (A.hexadecimal <* A.skipSpace)
  A.char ';' *> A.skipSpace
  upper <- V.fromList <$> many (A.hexadecimal <* A.skipSpace)
  A.char ';' *> A.skipSpace
  rest <- A.takeWhile (/= '\n')
  if ';' `B.elem` rest
    then pure Nothing -- Conditional record
    else pure $ Just $ Single cp () Record {..}
