{-# LANGUAGE OverloadedStrings #-}

module UCD.Blocks where

import Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Char8 as B
import Data.Traversable (for)
import qualified Data.Vector as V

import Data.UCD.Internal.Types (Block(NoBlock))
import UCD.Common (comments, unicodeTableSize)

fetch :: IO (V.Vector Block)
fetch = do
  txt <- B.readFile "data/latest/ucd/Blocks.txt"
  case A.parseOnly (parser <* A.endOfInput) txt of
    Left err -> fail err
    Right blocks ->
      pure $
      (V.replicate (unicodeTableSize `shiftR` 4) NoBlock V.//) $
      blocks >>= \(start, end, block) ->
        [(i, block) | i <- [start `shiftR` 4 .. end `shiftR` 4]]

parser :: A.Parser [(Int, Int, Block)]
parser = do
  comments
  records <-
    for [succ minBound .. maxBound] $ \block -> do
      (start, end) <- record <* A.char '\n'
      pure (start, end, block)
  -- This relies rather crucially on there being no duplicate block
  -- names in Blocks.txt /and/ variants in Block enum being in exactly
  -- the same order as blocks in Blocks.txt
  comments
  pure records

record :: A.Parser (Int, Int)
record = do
  start <- A.hexadecimal
  guard $ start .&. 0xf == 0
  _ <- A.string ".."
  end <- A.hexadecimal
  guard $ end .&. 0xf == 0xf
  _ <- A.char ';'
  _ <- A.takeWhile (/= '\n')
  pure (start, end)
