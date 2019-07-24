{-# LANGUAGE OverloadedStrings #-}

module UCD.Blocks where

import Control.Applicative (many)
import Control.Arrow ((&&&))
import Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bits ((.&.), shiftR)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Vector as V

import Data.UCD.Internal.Types (Block, fullPropertyValueName)
import UCD.Common (comments, unicodeTableSize, fetchGeneral)

fetch :: IO (V.Vector (Maybe Block))
fetch =
  fetchGeneral "data/latest/ucd/Blocks.txt" $ do
    blocks <- parser
    pure $
      (V.replicate (unicodeTableSize `shiftR` 4) Nothing V.//) $
      blocks >>= \(start, end, block) ->
        [(i, Just block) | i <- [start `shiftR` 4 .. end `shiftR` 4]]

parser :: A.Parser [(Int, Int, Block)]
parser = do
  comments
  records <- many $ record <* A.char '\n'
  comments
  pure records

record :: A.Parser (Int, Int, Block)
record = do
  start <- A.hexadecimal
  guard $ start .&. 0xf == 0
  _ <- A.string ".."
  end <- A.hexadecimal
  guard $ end .&. 0xf == 0xf
  _ <- A.char ';'
  A.skipSpace
  blockName <- A.takeWhile (/= '\n')
  let normalised = normalise blockName
  case find ((== normalised) . fst) candidates of
    Just (_, block) -> pure (start, end, block)
    Nothing -> fail $ "Can't recognise block " ++ show blockName

normalise :: ByteString -> ByteString
normalise = B.map toLower . B.filter (\c -> c /= ' ' && c /= '-' && c /= '_')

candidates :: [(ByteString, Block)]
candidates =
  map (normalise . fullPropertyValueName &&& id) [minBound .. maxBound]
