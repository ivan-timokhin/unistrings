module UCD.EastAsianWidth where

import Data.UCD.Internal.Types (EastAsianWidth)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () EastAsianWidth)
fetch = fetchSimple "data/latest/ucd/EastAsianWidth.txt" enumeratedAbbrP
