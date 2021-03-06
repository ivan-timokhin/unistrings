-- This file is automatically generated.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.Unistring.UCD.Internal.CanonicalCompositionTop (retrieve) where

import Data.Unistring.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)

foreign import ccall "&_hs__ucd__canonical_composition_top_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__canonical_composition_top_layer_1" layer_1 :: Ptr Int8
foreign import ccall "&_hs__ucd__canonical_composition_top_layer_2" layer_2 :: Ptr Int8
foreign import ccall "&_hs__ucd__canonical_composition_top_layer_3" layer_3 :: Ptr Word8
foreign import ccall "&_hs__ucd__canonical_composition_top_bottom" bottom :: Ptr Int16

retrieve :: Int -> Maybe Int
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 14
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 6 + (cp `shiftR` 8) .&. 0x3f
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 3 + (cp `shiftR` 5) .&. 0x7
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 3 + (cp `shiftR` 2) .&. 0x7
  val = let v = (unsafeReadPtr bottom $ i3 `shiftL` 2 + cp .&. 0x3) in if v == 0 then Nothing else Just (fromIntegral $ (v - 1))
