{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.LineBreak (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Data.UCD.Internal.Types (LineBreak)
import Data.UCD.Internal.Int (toInt#)
import GHC.Exts(tagToEnum#)

foreign import ccall "&_hs__ucd__line_break_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__line_break_layer_1" layer_1 :: Ptr Word8
foreign import ccall "&_hs__ucd__line_break_layer_2" layer_2 :: Ptr Int16
foreign import ccall "&_hs__ucd__line_break_layer_3" layer_3 :: Ptr Word8
foreign import ccall "&_hs__ucd__line_break_bottom" bottom :: Ptr Int8

retrieve :: Int -> LineBreak
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 12
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 5 + (cp `shiftR` 7) .&. 0x1f
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 3 + (cp `shiftR` 4) .&. 0x7
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 3 + (cp `shiftR` 1) .&. 0x7
  val = tagToEnum# (toInt# (unsafeReadPtr bottom $ i3 `shiftL` 1 + cp .&. 0x1)) :: LineBreak