{-# LANGUAGE OverloadedStrings #-}

module UCD.Blocks where

import Control.Applicative (many)
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bits ((.&.), shiftR)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import Data.UCD.Internal.Types (Block, fullPropertyValueName)
import UCD.Common (Parser_, comments, fetchGeneral, semicolon, unicodeTableSize)

fetch :: IO (V.Vector (Maybe Block))
fetch =
  fetchGeneral "Blocks.txt" $ do
    blocks <- parser
    pure $
      (V.replicate (unicodeTableSize `shiftR` 4) Nothing V.//) $
      blocks >>= \(start, end, block) ->
        [(i, Just block) | i <- [start `shiftR` 4 .. end `shiftR` 4]]

parser :: Parser_ [(Int, Int, Block)]
parser = do
  comments
  records <- many $ record <* MB.eol
  comments
  pure records

record :: Parser_ (Int, Int, Block)
record = do
  start <- MBL.hexadecimal
  guard $ start .&. 0xf == 0
  _ <- MB.string ".."
  end <- MBL.hexadecimal
  guard $ end .&. 0xf == 0xf
  semicolon
  MB.space
  blockName <- M.takeWhile1P (Just "Block name") (/= 0xA)
  let normalised = normalise blockName
  case find ((== normalised) . fst) candidates of
    Just (_, block) -> pure (start, end, block)
    Nothing -> fail $ "Can't recognise block " ++ show blockName

normalise :: ByteString -> ByteString
normalise = B.map toLower . B.filter (\c -> c /= ' ' && c /= '-' && c /= '_')

candidates :: [(ByteString, Block)]
candidates =
  map (normalise . fullPropertyValueName &&& id) [minBound .. maxBound]
