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
{-# LANGUAGE LambdaCase #-}

module UCD.CaseFolding where

import Control.Applicative (many)
import Data.Foldable (asum)
import qualified Data.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common
  ( Parser_
  , Range(Single)
  , Table(Table)
  , comments
  , dropNothing
  , fetchGeneral
  , semicolon
  )

data Contents =
  Contents
    { simple :: Table () () Int
    , common :: Table () () Int
    , full :: Table () () (V.Vector Int)
    }
  deriving (Show)

fetch :: IO Contents
fetch =
  fetchGeneral "CaseFolding.txt" $ do
    combined <- parser
    pure $
      Contents
        { simple =
            dropNothing $
            flip fmap combined $ \case
              Simple m -> Just m
              _ -> Nothing
        , common =
            dropNothing $
            flip fmap combined $ \case
              Common m -> Just m
              _ -> Nothing
        , full =
            dropNothing $
            flip fmap combined $ \case
              Full v -> Just v
              _ -> Nothing
        }

data Record
  = Simple Int
  | Common Int
  | Turkic Int
  | Full (V.Vector Int)

parser :: Parser_ (Table () () Record)
parser = do
  comments
  Table <$> many record

record :: Parser_ (Range () () Record)
record = do
  code <- MBL.hexadecimal
  semicolon *> MB.space
  datum <-
    asum
      [ do "C;" *> MB.space
           Common <$> MBL.hexadecimal
      , do "F;" *> MB.space
           mapping <- MBL.hexadecimal `M.sepBy1` MB.space1
           pure $ Full $ V.fromList mapping
      , do "S;" *> MB.space
           Simple <$> MBL.hexadecimal
      , do "T;" *> MB.space
           Turkic <$> MBL.hexadecimal
      ]
  semicolon *> MB.space *> comments
  pure $ Single code () datum
