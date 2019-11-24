{-# LANGUAGE RecordWildCards #-}

module UCD.SpecialCasing
  ( fetch
  , Record(lower, upper, title)
  ) where

import Control.Applicative (many)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common
  ( Parser_
  , Range(Single)
  , Table(Table)
  , comments
  , fetchGeneral
  , semicolon
  )

fetch :: IO (Table () () Record)
fetch = fetchGeneral "data/latest/ucd/SpecialCasing.txt" parser

data Record =
  Record
    { lower :: V.Vector Int
    , upper :: V.Vector Int
    , title :: V.Vector Int
    }
  deriving (Show, Eq)

parser :: Parser_ (Table () () Record)
parser = do
  comments
  records <- catMaybes <$> many (record <* comments)
  pure $ Table records

record :: Parser_ (Maybe (Range () () Record))
record = do
  cp <- MBL.hexadecimal
  semicolon *> MB.space
  lower <- V.fromList <$> many (MBL.hexadecimal <* MB.space)
  semicolon *> MB.space
  title <- V.fromList <$> many (MBL.hexadecimal <* MB.space)
  semicolon *> MB.space
  upper <- V.fromList <$> many (MBL.hexadecimal <* MB.space)
  semicolon *> MB.space
  rest <- M.takeWhileP Nothing (/= 0xA)
  if ';' `B.elem` rest
    then pure Nothing -- Conditional record
    else pure $ Just $ Single cp () Record {..}
