module UCD.WordBreakProperty where

import Data.UCD.Internal.Types (WordBreak)
import UCD.Common (Table, enumeratedFullP, fetchSimple)

fetch :: IO (Table () () WordBreak)
fetch = fetchSimple "auxiliary/WordBreakProperty.txt" enumeratedFullP
