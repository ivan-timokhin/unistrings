{-# OPTIONS_GHC -Wno-unused-imports -Wno-identities #-}
{- HLINT ignore -}
{-# LANGUAGE MagicHash #-}
module Data.UCD.Internal.JamoShortNameLen (retrieve) where

import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)

foreign import ccall "&_hs__ucd__jamo_short_name_len_bottom" bottom :: Ptr Int8

retrieve :: Int -> Int
{-# INLINE retrieve #-}
retrieve cp = val
 where
  val = fromIntegral $ unsafeReadPtr bottom cp
