{-# LANGUAGE OverloadedStrings #-}

module UCD.Jamo where

import Control.Applicative (many)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common (Parser_, comment, comments, fetchGeneral)

fetch :: IO (V.Vector ByteString)
fetch =
  fetchGeneral "Jamo.txt" $ do
    shortNames <- parser
    pure $
      V.replicate 0xC3 "" V.//
      map (\(n, str) -> (fromIntegral n - 0x1100, str)) shortNames

parser :: Parser_ [(Word32, ByteString)]
parser = do
  comments
  rs <- records
  comments
  pure rs

record :: Parser_ (Word32, ByteString)
record = do
  codePoint <- MBL.hexadecimal M.<?> "code point"
  _delimiter <- MB.string "; "
  name <- M.takeWhileP (Just "Name") (/= 0x20)
  MB.space
  comment
  pure (codePoint, name)

records :: Parser_ [(Word32, ByteString)]
records = many (record <* MB.eol)
