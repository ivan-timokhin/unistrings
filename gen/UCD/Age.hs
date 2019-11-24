module UCD.Age
  ( fetch
  ) where

import Data.UCD.Internal.Types (Age)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () Age)
fetch = fetchSimple "DerivedAge.txt" enumeratedAbbrP
