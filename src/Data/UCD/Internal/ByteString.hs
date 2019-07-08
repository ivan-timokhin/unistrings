module Data.UCD.Internal.ByteString (mkByteString) where

import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word8)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)

mkByteString :: Int -> Ptr Word8 -> ByteString
-- Is it necessary to strengthen protections around unsafePerformIO
-- (add NOINLINE, maybe)?  Can I relax them (unsafeDupablePerformIO,
-- or even inline version)?
mkByteString len (Ptr addr) = unsafePerformIO $ unsafePackAddressLen len addr
