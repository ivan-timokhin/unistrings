module UCD.BidiBrackets where

import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Byte.Lexer as MBL

import Data.UCD.Internal.Types (BidiPairedBracketType)
import UCD.Common (Table, enumeratedAbbrP, fetchSimple, semicolon)

fetch :: IO (Table () () (Int, BidiPairedBracketType))
fetch =
  fetchSimple "data/latest/ucd/BidiBrackets.txt" $ do
    cp <- MBL.hexadecimal
    semicolon *> MB.space
    ty <- enumeratedAbbrP
    pure (cp, ty)
