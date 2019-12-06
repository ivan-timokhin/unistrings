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
