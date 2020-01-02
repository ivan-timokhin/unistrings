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

module UCD.NameAliases where

import Control.Applicative (many)
import Data.ByteString.Char8 (ByteString)
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import Data.Unistring.UCD.Internal.Types
  ( NameAliasType(AbbreviationAlias, AlternateAlias, ControlAlias,
              CorrectionAlias, FigmentAlias)
  )
import UCD.Common
  ( Parser_
  , Range(Single)
  , Table(Table)
  , comments
  , fetchGeneral
  , semicolon
  , tableP
  )

fetch :: IO (Table () a (V.Vector (NameAliasType, ByteString)))
fetch =
  fetchGeneral "NameAliases.txt" $ do
    aliases <- parser
    let imap = IM.fromListWith (++) $ map (fmap (: [])) aliases
    pure $
      Table $
      map (\(cp, als) -> Single (toEnum cp) () als) $
      IM.toList $ fmap (V.fromList . sort) imap

parser :: Parser_ [(Int, (NameAliasType, ByteString))]
parser = do
  comments
  many (record <* MB.eol <* comments)

record :: Parser_ (Int, (NameAliasType, ByteString))
record = do
  cp <- MBL.hexadecimal M.<?> "code point"
  semicolon
  alias <- M.takeWhile1P Nothing (/= 0x3b)
  semicolon
  ty <- naType
  pure (cp, (ty, alias))

naType :: Parser_ NameAliasType
naType =
  tableP
    [ "correction" ~> CorrectionAlias
    , "control" ~> ControlAlias
    , "alternate" ~> AlternateAlias
    , "figment" ~> FigmentAlias
    , "abbreviation" ~> AbbreviationAlias
    ]
  where
    (~>) = (,)
