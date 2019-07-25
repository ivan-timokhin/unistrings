{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UCD.CaseFolding where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Vector as V

import UCD.Common
  ( Range(Single)
  , Table(Table)
  , comments
  , dropNothing
  , fetchGeneral
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

parser :: A.Parser (Table () () Record)
parser = do
  comments
  Table <$> many record

record :: A.Parser (Range () () Record)
record = do
  code <- A.hexadecimal
  A.char ';' *> A.skipSpace
  datum <-
    A.choice
      [ do "C;" *> A.skipSpace
           Common <$> A.hexadecimal
      , do "F;" *> A.skipSpace
           mapping <- A.hexadecimal `A.sepBy1` A.skipSpace
           pure $ Full $ V.fromList mapping
      , do "S;" *> A.skipSpace
           Simple <$> A.hexadecimal
      , do "T;" *> A.skipSpace
           Turkic <$> A.hexadecimal
      ]
  A.char ';' *> A.skipSpace *> comments
  pure $ Single code () datum
