{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.NameAliasesAliasesPtr (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Foreign.Ptr (plusPtr)

foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_layer_1" layer_1 :: Ptr Int8
foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_layer_2" layer_2 :: Ptr Int8
foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_layer_3" layer_3 :: Ptr Int8
foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_bottom" bottom :: Ptr Int16
foreign import ccall "&_hs__ucd__name_aliases_aliases_ptr_values" values :: Ptr Word8

retrieve :: Int -> Ptr Word8
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 13
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 4 + (cp `shiftR` 9) .&. 0xf
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 3 + (cp `shiftR` 6) .&. 0x7
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 3 + (cp `shiftR` 3) .&. 0x7
  val = plusPtr values  . fromEnum $ unsafeReadPtr bottom $ i3 `shiftL` 3 + cp .&. 0x7
