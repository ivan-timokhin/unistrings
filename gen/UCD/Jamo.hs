{-# LANGUAGE OverloadedStrings #-}

module UCD.Jamo where

import Control.Applicative (many, optional)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (void)
import qualified Data.Vector as V
import Data.Word (Word32)

fetch :: IO (V.Vector ByteString)
fetch = do
  txt <- B.readFile "data/latest/ucd/Jamo.txt"
  case A.parseOnly (parser <* A.endOfInput) txt of
    Left err -> fail err
    Right shortNames ->
      pure $
      V.replicate 0xC3 "" V.//
      map (\(n, str) -> (fromIntegral n - 0x1100, str)) shortNames

parser :: A.Parser [(Word32, ByteString)]
parser = do
  _ <- many (optional comment <* A.char '\n')
  rs <- records
  _ <- many (optional comment <* A.char '\n')
  pure rs

comment :: A.Parser ()
comment = void (A.char '#' *> A.takeWhile (/= '\n')) A.<?> "comment"

record :: A.Parser (Word32, ByteString)
record = do
  codePoint <- A.hexadecimal A.<?> "code point"
  _delimiter <- A.string "; "
  name <- A.takeWhile (\c -> not (A.isSpace c) && c /= '#')
  A.skipSpace
  comment
  pure (codePoint, name)

records :: A.Parser [(Word32, ByteString)]
records = many (record <* A.char '\n')
