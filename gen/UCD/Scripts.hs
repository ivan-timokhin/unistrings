{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module UCD.Scripts
  ( fetch
  ) where

import Data.UCD.Internal.Types (Script(..))
import UCD.Common (Table, enumeratedFullP, fetchSimple)

fetch :: IO (Table () () Script)
fetch = fetchSimple "data/latest/ucd/Scripts.txt" enumeratedFullP
