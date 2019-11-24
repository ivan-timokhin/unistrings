module UCD.ScriptExtensions
  ( fetch
  ) where

import Control.Applicative (some)
import qualified Data.Vector as V
import qualified Text.Megaparsec.Byte as MB

import Data.UCD.Internal.Types (Script)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () (V.Vector Script))
fetch =
  fetchSimple "data/latest/ucd/ScriptExtensions.txt" $
  fmap V.fromList $ some $ enumeratedAbbrP <* MB.space1
