module UCD.BidiBrackets where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.UCD.Internal.Types (BidiPairedBracketType)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple)

fetch :: IO (Table () () (Int, BidiPairedBracketType))
fetch =
  fetchSimple "data/latest/ucd/BidiBrackets.txt" $ do
    cp <- A.hexadecimal
    A.char ';' *> A.skipSpace
    ty <- enumeratedAbbrP
    pure (cp, ty)
