module UCD.ScriptExtensions
  ( fetch
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.UCD.Internal.Types (Script)
import qualified Data.Vector as V
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () (V.Vector Script))
fetch =
  fetchSimple "data/latest/ucd/ScriptExtensions.txt" $
  fmap V.fromList $ enumeratedAbbrP `A.sepBy1` A.skipSpace
