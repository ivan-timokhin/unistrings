-- This file is automatically generated.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.Unistring.UCD.Internal.JoiningGroup (retrieve) where

import Data.Unistring.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Data.Unistring.UCD.Internal.Types (JoiningGroup)
import Data.Unistring.UCD.Internal.Int (toInt#)
import GHC.Exts(tagToEnum#)

foreign import ccall "&_hs__ucd__joining_group_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__joining_group_layer_1" layer_1 :: Ptr Int8
foreign import ccall "&_hs__ucd__joining_group_layer_2" layer_2 :: Ptr Int8
foreign import ccall "&_hs__ucd__joining_group_layer_3" layer_3 :: Ptr Int8
foreign import ccall "&_hs__ucd__joining_group_bottom" bottom :: Ptr Int8

retrieve :: Int -> Maybe JoiningGroup
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 15
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 4 + (cp `shiftR` 11) .&. 0xf
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 4 + (cp `shiftR` 7) .&. 0xf
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 4 + (cp `shiftR` 3) .&. 0xf
  val = let v = (unsafeReadPtr bottom $ i3 `shiftL` 3 + cp .&. 0x7) in if v == 0 then Nothing else Just (tagToEnum# (toInt# ((v - 1))) :: JoiningGroup)
