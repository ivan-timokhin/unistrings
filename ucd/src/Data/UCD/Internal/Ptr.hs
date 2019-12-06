{-
Copyright 2019 Ivan Timokhin

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

module Data.UCD.Internal.Ptr
  ( Ptr
  , unsafeReadPtr
  ) where

import GHC.Exts
  ( Int(I#)
  , Ptr(Ptr)
  , indexInt16OffAddr#
  , indexInt32OffAddr#
  , indexInt64OffAddr#
  , indexInt8OffAddr#
  , indexWord16OffAddr#
  , indexWord8OffAddr#
  )

import GHC.Int (Int16(I16#), Int32(I32#), Int64(I64#), Int8(I8#))
import GHC.Word (Word16(W16#), Word8(W8#))

class Readable a where
  unsafeReadPtr :: Ptr a -> Int -> a

instance Readable Int8 where
  unsafeReadPtr (Ptr addr) (I# offset) = I8# (indexInt8OffAddr# addr offset)

instance Readable Word8 where
  unsafeReadPtr (Ptr addr) (I# offset) = W8# (indexWord8OffAddr# addr offset)

instance Readable Int16 where
  unsafeReadPtr (Ptr addr) (I# offset) = I16# (indexInt16OffAddr# addr offset)

instance Readable Word16 where
  unsafeReadPtr (Ptr addr) (I# offset) = W16# (indexWord16OffAddr# addr offset)

instance Readable Int32 where
  unsafeReadPtr (Ptr addr) (I# offset) = I32# (indexInt32OffAddr# addr offset)

instance Readable Int64 where
  unsafeReadPtr (Ptr addr) (I# offset) = I64# (indexInt64OffAddr# addr offset)
