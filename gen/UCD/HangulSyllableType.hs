module UCD.HangulSyllableType where

import Data.UCD.Internal.Types (HangulSyllableType)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () HangulSyllableType)
fetch = fetchSimple "data/latest/ucd/HangulSyllableType.txt" enumeratedAbbrP
