{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.JamoShortNamePtr (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Foreign.Ptr (plusPtr)

foreign import ccall "&_hs__ucd__jamo_short_name_ptr_bottom" bottom :: Ptr Int8
foreign import ccall "&_hs__ucd__jamo_short_name_ptr_values" values :: Ptr Word8

retrieve :: Int -> Ptr Word8
{-# INLINE retrieve #-}
retrieve cp = val
 where
  val = plusPtr values  . fromEnum $ unsafeReadPtr bottom cp
