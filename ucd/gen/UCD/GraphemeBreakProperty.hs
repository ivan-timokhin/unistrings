module UCD.GraphemeBreakProperty where

import Data.UCD.Internal.Types (GraphemeClusterBreak)
import UCD.Common (Table, enumeratedFullP, fetchSimple)

fetch :: IO (Table () () GraphemeClusterBreak)
fetch = fetchSimple "auxiliary/GraphemeBreakProperty.txt" enumeratedFullP
