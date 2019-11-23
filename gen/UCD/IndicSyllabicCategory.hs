module UCD.IndicSyllabicCategory where

import Data.UCD.Internal.Types (IndicSyllabicCategory)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () IndicSyllabicCategory)
fetch = fetchSimple "data/latest/ucd/IndicSyllabicCategory.txt" enumeratedAbbrP
