{-
Copyright 2020 Ivan Timokhin

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
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Table(Table, getTable)
  , Range(Single, Range)
  , unicodeTableSize
  , tableToVector
  , adjustWith
  , adjustWithM
  , Parser
  , Parser_
  , tableP
  , enumeratedP
  , range
  , comment
  , comments
  , dropNothing
  , semicolon
  , char8
  , tableParser
  , fetchSimple
  , fetchBinaryMulti
  , fetchGeneral
  ) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (asum)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(Down))
import qualified Data.Vector.Generic as VG
import Data.Void (Void)
import Data.Word (Word32)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL
import qualified Text.Megaparsec.Error as ME

newtype Table annS annR a =
  Table
    { getTable :: [Range annS annR a]
    }
  deriving (Show, Functor)

data Range annS annR a
  = Single Word32 annS a
  | Range Word32 Word32 annR a
  deriving (Eq, Show, Functor, Foldable, Traversable)

dropNothing :: Table annS annR (Maybe a) -> Table annS annR a
dropNothing (Table tbl) = Table $ mapMaybe sequence tbl

unicodeTableSize :: Int
unicodeTableSize = 0x110000

tableToVector :: VG.Vector v a => a -> Table annS annR a -> v a
tableToVector def table = VG.replicate unicodeTableSize def `adjustWith` table

adjustWith :: VG.Vector v a => v a -> Table annS annR a -> v a
adjustWith vec table = vec VG.// assignments
  where
    assignments =
      getTable table >>= \case
        Single code _ udata -> [(fromIntegral code, udata)]
        Range lo hi _ udata -> [(fromIntegral i, udata) | i <- [lo .. hi]]

adjustWithM :: VG.Vector v a => v a -> Table annS annR (Maybe a) -> v a
adjustWithM vec table = vec `adjustWith` dropNothing table

type Parser e = M.Parsec e ByteString

type Parser_ = Parser Void

char8 :: Ord e => Char -> Parser e ()
char8 = void . M.single . fromIntegral . fromEnum

semicolon :: Ord e => Parser e ()
semicolon = char8 ';'

comment :: Ord e => Parser e ()
comment = MBL.skipLineComment "#"

comments :: Ord e => Parser e ()
comments = void $ many $ optional comment <* MB.eol

tableP :: Ord e => [(ByteString, a)] -> Parser e a
tableP =
  asum . map (\(str, a) -> a <$ MB.string str) . sortOn (Down . B.length . fst)

range :: Ord e => Parser e (Range () () ())
range = do
  start <- MBL.hexadecimal
  rng <- fullRange start <|> pure (Single start () ())
  MB.space
  semicolon
  MB.space
  pure rng
  where
    fullRange start = do
      _ <- MB.string ".."
      end <- MBL.hexadecimal
      pure $ Range start end () ()

enumeratedP :: (Enum a, Bounded a, Ord e) => (a -> ByteString) -> Parser e a
enumeratedP f = tableP $ flip map [minBound .. maxBound] $ \p -> (f p, p)

fetchSimple :: FilePath -> Parser_ a -> IO (Table () () a)
fetchSimple file = fetchGeneral file . tableParser

tableParser :: Ord e => Parser e a -> Parser e (Table () () a)
tableParser p = do
  comments
  Table <$> many record
  where
    record = do
      rng <- range
      v <- p
      MB.space
      comments
      pure $ v <$ rng

fetchBinaryMulti :: FilePath -> IO (Map ByteString (Table () () Bool))
fetchBinaryMulti = flip fetchGeneral (fmap Table <$> parser)
  where
    parser :: Parser_ (Map ByteString [Range () () Bool])
    parser =
      flip execStateT Map.empty $ do
        lift comments
        many record
    record :: StateT (Map ByteString [Range () () Bool]) Parser_ ()
    record = do
      rng <- lift range
      prop <-
        M.takeWhile1P (Just "Property name") (\c -> c /= 0x20 && c /= 0x23) -- ' ', '#'
      lift $ MB.space *> comments
      modify $ Map.alter (Just . ((True <$ rng) :) . fromMaybe []) prop

fetchGeneral :: ME.ShowErrorComponent e => FilePath -> Parser e a -> IO a
fetchGeneral file parser = do
  txt <- B.readFile file
  case M.parse (parser <* M.eof) file txt of
    Left err ->
      fail $ "Fetching " ++ show file ++ ": " ++ ME.errorBundlePretty err
    Right result -> pure result
