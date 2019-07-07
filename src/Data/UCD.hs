module Data.UCD
  ( CodePoint
  , IsCodePoint(toCodePoint)
  , generalCategory
  , canonicalCombiningClass
  , name
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Char (GeneralCategory, ord)
import Data.Word (Word8)
import GHC.Exts (Ptr(Ptr))
import System.IO.Unsafe (unsafePerformIO)

import Data.UCD.Internal (CodePoint(CodePoint))
import qualified Data.UCD.Internal.CanonicalCombiningClass as CCC
import qualified Data.UCD.Internal.GeneralCategory as GC
import qualified Data.UCD.Internal.NameLen as NameLen
import qualified Data.UCD.Internal.NamePtr as NamePtr

class IsCodePoint c where
  toCodePoint :: c -> CodePoint

instance IsCodePoint CodePoint where
  toCodePoint = id

instance IsCodePoint Char where
  toCodePoint = CodePoint . toEnum . ord

generalCategory :: IsCodePoint cp => cp -> GeneralCategory
generalCategory = GC.retrieve . fromEnum . toCodePoint

canonicalCombiningClass :: IsCodePoint cp => cp -> Word8
canonicalCombiningClass = CCC.retrieve . fromEnum . toCodePoint

-- NB: This function is incorrect!  It does not currently handle
-- ranges that do not have names listed in UnicodeData.txt (and
-- instead derive them algorithmically)
name :: IsCodePoint cp => cp -> ByteString
name cp = mkByteString (NameLen.retrieve icp) (NamePtr.retrieve icp)
  where
    icp = fromEnum $ toCodePoint cp

mkByteString :: Int -> Ptr Word8 -> ByteString
-- Is it necessary to strengthen protections around unsafePerformIO
-- (add NOINLINE, maybe)?  Can I relax them (unsafeDupablePerformIO,
-- or even inline version)?
mkByteString len (Ptr addr) = unsafePerformIO $ unsafePackAddressLen len addr
