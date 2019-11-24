module UCD.VerticalOrientation where

import Data.UCD.Internal.Types (VerticalOrientation)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () VerticalOrientation)
fetch = fetchSimple "VerticalOrientation.txt" enumeratedAbbrP
