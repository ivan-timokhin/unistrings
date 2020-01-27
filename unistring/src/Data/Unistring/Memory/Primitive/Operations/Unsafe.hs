{-
Copyright 2020 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Primitive.Operations.Unsafe
Description : Unsafe primitive bulk operations on byte arrays
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Primitive.Operations.Unsafe
  ( compareBytesForeign
  , compareBytesNative
  , compareBytesMixed
  , sizeOfByteArray
  , getSizeOfMutableByteArray
  ) where

import Foreign.C.Types (CInt(CInt), CSize(CSize))
import qualified GHC.Exts as E
import GHC.ST (ST(ST))

import Data.Unistring.Memory.Count (ByteCount(ByteCount, getByteCount))

compareBytesForeign :: E.Ptr a -> E.Ptr a -> ByteCount -> IO Int
{-# INLINE compareBytesForeign #-}
compareBytesForeign lp rp len =
  fromIntegral <$> c_memcmp lp rp (fromIntegral $ getByteCount len)

-- See note ‘Unsafe FFI’
-- It is logically pure, assuming the memory Ptr's point to is
-- immutable, but we need IO anyway to sequence touch# (via
-- withForeignPtr) after it.
foreign import ccall unsafe "string.h memcmp" c_memcmp
  :: E.Ptr a -> E.Ptr a -> CSize -> IO CInt

compareBytesNative :: E.ByteArray# -> E.ByteArray# -> ByteCount -> Int
{-# INLINE compareBytesNative #-}
#if MIN_VERSION_base(4, 11, 0)
compareBytesNative x# y# (ByteCount (E.I# n#)) =
  E.I# (E.compareByteArrays# x# 0# y# 0# n#)
#else
compareBytesNative x# y# (ByteCount n) =
  fromIntegral $ c_memcmp_bytes x# y# (fromIntegral n)

-- See note ‘Unsafe FFI’
-- It is logically pure, so we mark it as such
foreign import ccall unsafe "string.h memcmp" c_memcmp_bytes
  :: E.ByteArray# -> E.ByteArray# -> CSize -> CInt
#endif

compareBytesMixed :: E.ByteArray# -> E.Ptr a -> ByteCount -> IO Int
{-# INLINE compareBytesMixed #-}
compareBytesMixed ba# ptr (ByteCount n) =
  fromIntegral <$> c_memcmp_mixed ba# ptr (fromIntegral n)

-- See note ‘Unsafe FFI’
foreign import ccall unsafe "string.h memcmp" c_memcmp_mixed
  :: E.ByteArray# -> E.Ptr a -> CSize -> IO CInt

sizeOfByteArray :: E.ByteArray# -> ByteCount
{-# INLINE sizeOfByteArray #-}
sizeOfByteArray ba = ByteCount (E.I# (E.sizeofByteArray# ba))

getSizeOfMutableByteArray :: E.MutableByteArray# s -> ST s ByteCount
{-# INLINE getSizeOfMutableByteArray #-}
getSizeOfMutableByteArray mba =
  ST $ \s ->
    case E.getSizeofMutableByteArray# mba s of
      (# s', n #) -> (# s', ByteCount (E.I# n) #)
