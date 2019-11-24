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
  fetchGeneral "data/latest/ucd/CaseFolding.txt" $ do
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
