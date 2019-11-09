module UCD.DerivedJoiningGroup where

import Data.UCD.Internal.Types (JoiningGroup)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () JoiningGroup)
fetch =
  fetchSimple
    "data/latest/ucd/extracted/DerivedJoiningGroup.txt"
    enumeratedAbbrP
