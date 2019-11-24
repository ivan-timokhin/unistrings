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

import Data.UCD.Internal.Types
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
  fetchGeneral "data/latest/ucd/NameAliases.txt" $ do
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
