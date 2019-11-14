module UCD.SentenceBreakProperty where

import Data.UCD.Internal.Types (SentenceBreak)
import UCD.Common (Table, enumeratedFullP, fetchSimple)

fetch :: IO (Table () () SentenceBreak)
fetch =
  fetchSimple
    "data/latest/ucd/auxiliary/SentenceBreakProperty.txt"
    enumeratedFullP