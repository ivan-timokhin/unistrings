module UCD.IndicPositionalCategory where

import Data.UCD.Internal.Types (IndicPositionalCategory)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () IndicPositionalCategory)
fetch =
  fetchSimple "data/latest/ucd/IndicPositionalCategory.txt" enumeratedAbbrP
