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
module UCD.DerivedNumericValues where

import Control.Applicative ((<|>))
import Data.Int (Int64)
import Data.Ratio (Ratio, (%))
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common (Table, char8, fetchSimple, semicolon)

fetch :: IO (Table () () (Ratio Int64))
fetch =
  fetchSimple "extracted/DerivedNumericValues.txt" $ do
    _ <- MBL.signed (pure ()) MBL.scientific
    MB.space
    semicolon
    MB.space
    semicolon
    MB.space
    numerator <- MBL.signed (pure ()) MBL.decimal
    denominator <- (char8 '/' *> MBL.decimal) <|> pure 1
    pure $ numerator % denominator
