module UCD.EquivalentUnifiedIdeograph where

import qualified Data.Attoparsec.ByteString.Char8 as A

import UCD.Common (Table, fetchSimple)

fetch :: IO (Table () () Int)
fetch =
  fetchSimple "data/latest/ucd/EquivalentUnifiedIdeograph.txt" A.hexadecimal
