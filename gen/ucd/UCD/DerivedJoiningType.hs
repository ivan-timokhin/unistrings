module UCD.DerivedJoiningType where

import Data.UCD.Internal.Types (JoiningType)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () JoiningType)
fetch = fetchSimple "extracted/DerivedJoiningType.txt" enumeratedAbbrP
