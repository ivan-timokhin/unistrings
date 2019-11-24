module UCD.EquivalentUnifiedIdeograph where

import qualified Text.Megaparsec.Byte.Lexer as MBL

import UCD.Common (Table, fetchSimple)

fetch :: IO (Table () () Int)
fetch =
  fetchSimple "data/latest/ucd/EquivalentUnifiedIdeograph.txt" MBL.hexadecimal
