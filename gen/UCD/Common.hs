{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UCD.Common where

import Control.Applicative ((<|>), many, optional)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor (void)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import qualified Data.Vector as V
import Data.Word (Word32)

newtype Table annS annR a =
  Table
    { getTable :: [Range annS annR a]
    }
  deriving (Show, Functor)

data Range annS annR a
  = Single Word32 annS a
  | Range Word32 Word32 annR a
  deriving (Eq, Show, Functor)

unicodeTableSize :: Int
unicodeTableSize = 0x110000

tableToVector :: a -> Table annS annR a -> V.Vector a
tableToVector def table = V.replicate unicodeTableSize def V.// assignments
  where
    assignments =
      getTable table >>= \case
        Single code _ udata -> [(fromIntegral code, udata)]
        Range lo hi _ udata -> [(fromIntegral i, udata) | i <- [lo .. hi]]

comment :: A.Parser ()
comment = void (A.char '#' *> A.takeWhile (/= '\n')) A.<?> "comment"

comments :: A.Parser ()
comments = void $ many $ optional comment <* A.char '\n'

tableP :: [(ByteString, a)] -> A.Parser a
tableP =
  A.choice .
  map (\(str, a) -> a <$ A.string str) . sortOn (Down . B.length . fst)

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
