{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.SimpleCaseFolding (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)

foreign import ccall "&_hs__ucd__simple_case_folding_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__simple_case_folding_layer_1" layer_1 :: Ptr Int8
foreign import ccall "&_hs__ucd__simple_case_folding_layer_2" layer_2 :: Ptr Int8
foreign import ccall "&_hs__ucd__simple_case_folding_layer_3" layer_3 :: Ptr Int8
foreign import ccall "&_hs__ucd__simple_case_folding_bottom" bottom :: Ptr Int32

retrieve :: Int -> Int
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 13
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 5 + (cp `shiftR` 8) .&. 0x1f
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 4 + (cp `shiftR` 4) .&. 0xf
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 3 + (cp `shiftR` 1) .&. 0x7
  val = fromIntegral $ unsafeReadPtr bottom $ i3 `shiftL` 1 + cp .&. 0x1