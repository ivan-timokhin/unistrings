{-# LANGUAGE MagicHash #-}

module Data.UCD.Internal.Int
  ( ToInt#(toInt#)
  ) where

import GHC.Exts (Int#, word2Int#)
import GHC.Int (Int16(I16#), Int32(I32#), Int64(I64#), Int8(I8#))
import GHC.Word (Word16(W16#), Word8(W8#))

class ToInt# a where
  toInt# :: a -> Int#

instance ToInt# Int16 where
  toInt# (I16# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Int32 where
  toInt# (I32# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Int64 where
  toInt# (I64# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Int8 where
  toInt# (I8# i) = i
  {-# INLINE toInt# #-}

instance ToInt# Word16 where
  toInt# (W16# w) = word2Int# w
  {-# INLINE toInt# #-}

instance ToInt# Word8 where
  toInt# (W8# w) = word2Int# w
  {-# INLINE toInt# #-}
