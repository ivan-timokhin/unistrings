{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.Age (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Data.UCD.Internal.Types (Age)
import Data.UCD.Internal.Int (toInt#)
import GHC.Exts(tagToEnum#)

foreign import ccall "&_hs__ucd__age_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__age_layer_1" layer_1 :: Ptr Word8
foreign import ccall "&_hs__ucd__age_layer_2" layer_2 :: Ptr Int16
foreign import ccall "&_hs__ucd__age_layer_3" layer_3 :: Ptr Word8
foreign import ccall "&_hs__ucd__age_bottom" bottom :: Ptr Int8

retrieve :: Int -> Maybe Age
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 11
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 4 + (cp `shiftR` 7) .&. 0xf
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 3 + (cp `shiftR` 4) .&. 0x7
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 3 + (cp `shiftR` 1) .&. 0x7
  val = let v = (unsafeReadPtr bottom $ i3 `shiftL` 1 + cp .&. 0x1) in if v == 0 then Nothing else Just (tagToEnum# (toInt# ((v - 1))) :: Age)
