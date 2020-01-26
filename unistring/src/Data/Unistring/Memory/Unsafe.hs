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
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Unsafe
Description : Unsafe functions for working with underlying memory allocations
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Unsafe
  ( CountOf(CountOf, getCountOf)
  , ByteCount(ByteCount, getByteCount)
  , Primitive(inBytes, inElements, uncheckedIndexPtr, uncheckedReadPtr,
          uncheckedWritePtr, uncheckedIndexBytes, uncheckedReadBytes,
          uncheckedWriteBytes)
  , ForeignArray(ForeignArray, foreignArrayPtr, foreignArrayLength)
  , NativeArray(NativeArray, nativeArrayBytes)
  , nativeArrayLength
  , MutableArray(uncheckedRead, uncheckedWrite)
  , Storage(Native, Foreign)
  , Array(NArray, FArray, getNArray, getFArray)
  , size
  , toList
  , equal
  , forgetArrayAllocator
  , allocatorCoercion
  , Default
  , Pinned
  , Unknown
  , AllocatorM(new)
  , Allocator(withAllocator, adopt)
  , Sing(SNative, SForeign)
  , allocator
  , storage
  ) where

import Data.Unistring.Memory.Array.Internal
  ( Allocator(adopt, withAllocator)
  , AllocatorM(new)
  , Array(FArray, NArray, getFArray, getNArray)
  , Default
  , ForeignArray(ForeignArray, foreignArrayLength, foreignArrayPtr)
  , MutableArray(uncheckedRead, uncheckedWrite)
  , NativeArray(NativeArray, nativeArrayBytes)
  , Pinned
  , Unknown
  , allocator
  , allocatorCoercion
  , equal
  , forgetArrayAllocator
  , nativeArrayLength
  , size
  , storage
  , toList
  )
import Data.Unistring.Memory.Count
  ( ByteCount(ByteCount, getByteCount)
  , CountOf(CountOf, getCountOf)
  )
import Data.Unistring.Memory.Primitive.Class.Unsafe
  ( Primitive(inBytes, inElements, uncheckedIndexBytes,
          uncheckedIndexPtr, uncheckedReadBytes, uncheckedReadPtr,
          uncheckedWriteBytes, uncheckedWritePtr)
  )
import Data.Unistring.Memory.Storage
  ( Sing(SForeign, SNative)
  , Storage(Foreign, Native)
  )
