module UCD.DerivedBidiClass where

import Data.UCD.Internal.Types (BidiClass)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () BidiClass)
fetch =
  fetchSimple "data/latest/ucd/extracted/DerivedBidiClass.txt" enumeratedAbbrP
