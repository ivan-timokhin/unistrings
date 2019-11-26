{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.ChangesWhenCasefolded (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Data.Bool (Bool)
import Data.UCD.Internal.Int (toInt#)
import GHC.Exts(tagToEnum#)

foreign import ccall "&_hs__ucd__changes_when_casefolded_layer_0" layer_0 :: Ptr Int8
foreign import ccall "&_hs__ucd__changes_when_casefolded_layer_1" layer_1 :: Ptr Int8
foreign import ccall "&_hs__ucd__changes_when_casefolded_layer_2" layer_2 :: Ptr Int8
foreign import ccall "&_hs__ucd__changes_when_casefolded_layer_3" layer_3 :: Ptr Int8
foreign import ccall "&_hs__ucd__changes_when_casefolded_bottom" bottom :: Ptr Int8

retrieve :: Int -> Bool
{-# INLINE retrieve #-}
retrieve cp = val
 where
  i0 = fromEnum $ unsafeReadPtr layer_0 $ cp `shiftR` 13
  i1 = fromEnum $ unsafeReadPtr layer_1 $ i0 `shiftL` 5 + (cp `shiftR` 8) .&. 0x1f
  i2 = fromEnum $ unsafeReadPtr layer_2 $ i1 `shiftL` 4 + (cp `shiftR` 4) .&. 0xf
  i3 = fromEnum $ unsafeReadPtr layer_3 $ i2 `shiftL` 2 + (cp `shiftR` 2) .&. 0x3
  val = tagToEnum# (toInt# (unsafeReadPtr bottom $ i3 `shiftL` 2 + cp .&. 0x3)) :: Bool
