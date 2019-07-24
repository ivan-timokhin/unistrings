{-# LANGUAGE OverloadedStrings #-}

module UCD.NameAliases where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Vector as V

import Data.UCD.Internal.Types
  ( NameAliasType(AbbreviationAlias, AlternateAlias, ControlAlias,
              CorrectionAlias, FigmentAlias)
  )
import UCD.Common (Range(Single), Table(Table), comments, fetchGeneral, tableP)

fetch :: IO (Table () a (V.Vector (NameAliasType, ByteString)))
fetch =
  fetchGeneral "data/latest/ucd/NameAliases.txt" $ do
    aliases <- parser
    let imap = IM.fromListWith (++) $ map (fmap (: [])) aliases
    pure $
      Table $
      map (\(cp, als) -> Single (toEnum cp) () als) $
      IM.toList $ fmap (V.fromList . sort) imap

parser :: A.Parser [(Int, (NameAliasType, ByteString))]
parser = do
  comments
  many (record <* A.char '\n' <* comments)

record :: A.Parser (Int, (NameAliasType, ByteString))
record = do
  cp <- A.hexadecimal A.<?> "code point"
  _d1 <- A.char ';'
  alias <- A.takeWhile1 (/= ';')
  _d2 <- A.char ';'
  ty <- naType
  pure (cp, (ty, alias))

naType :: A.Parser NameAliasType
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
