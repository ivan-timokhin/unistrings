{-
Copyright 2019 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
module Data.Unistring.UCD.Internal.ByteString
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
