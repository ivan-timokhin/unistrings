module UCD.LineBreak where

import Data.UCD.Internal.Types (LineBreak)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () LineBreak)
fetch = fetchSimple "LineBreak.txt" enumeratedAbbrP
