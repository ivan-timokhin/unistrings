module Data.UCD.Internal.ByteString
  ( mkByteString
  , renderUnicodeInt
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (unsafeCreate)
import Data.ByteString.Unsafe (unsafeIndex, unsafePackAddressLen)
import Data.Word (Word8)
import Foreign.Storable (pokeElemOff)
import GHC.Ptr (Ptr(Ptr))
import System.IO.Unsafe (unsafePerformIO)

mkByteString :: Int -> Ptr Word8 -> ByteString
-- Is it necessary to strengthen protections around unsafePerformIO
-- (add NOINLINE, maybe)?  Can I relax them (unsafeDupablePerformIO,
-- or even inline version)?
mkByteString len (Ptr addr) = unsafePerformIO $ unsafePackAddressLen len addr

renderUnicodeInt :: Int -> ByteString
renderUnicodeInt n
  | n < 0x10000 =
    unsafeCreate 4 $ \ptr -> do
      let pokeDigit d =
            pokeElemOff ptr d . unsafeIndex digits . (.&. 0xf) . (n `shiftR`)
      pokeDigit 0 12
      pokeDigit 1 8
      pokeDigit 2 4
      pokeDigit 3 0
  | n < 0x100000 =
    unsafeCreate 5 $ \ptr -> do
      let pokeDigit d =
            pokeElemOff ptr d . unsafeIndex digits . (.&. 0xf) . (n `shiftR`)
      pokeDigit 0 16
      pokeDigit 1 12
      pokeDigit 2 8
      pokeDigit 3 4
      pokeDigit 4 0
  | otherwise =
    unsafeCreate 6 $ \ptr -> do
      let pokeDigit d =
            pokeElemOff ptr d . unsafeIndex digits . (.&. 0xf) . (n `shiftR`)
      pokeDigit 0 20
      pokeDigit 1 16
      pokeDigit 2 12
      pokeDigit 3 8
      pokeDigit 4 4
      pokeDigit 5 0
  where
    digits = B.pack "0123456789ABCDEF"
