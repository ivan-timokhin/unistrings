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
fetch = fetchGeneral "SpecialCasing.txt" parser

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
