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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Inspection.Unistring.Memory.Unsafe
  ( tests
  ) where

import Control.Monad.ST (ST)
import Data.Type.Coercion (Coercion)
import Data.Word (Word16, Word8)
import GHC.Exts (Addr#, Int(I#), Int#, fromListN, toList)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents)
import Test.Inspection (hasNoType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)

import qualified Data.Unistring.Memory.Unsafe as U

import Inspection.TH (inspectTest)

tests :: [TestTree]
tests =
  [ $(inspectTest "Native array length" $
      'nativeArrayLength `hasNoType` ''U.Sing)
  , $(inspectTest "Foreign array length" $
      'foreignArrayLength `hasNoType` ''U.Sing)
  , $(inspectTest "Native array toList no singletons" $
      'toListArrayNative `hasNoType` ''U.Sing)
  , $(inspectTest "Foreign array toList no singletons" $
      'toListArrayForeign `hasNoType` ''U.Sing)
  , $(inspectTest "Native array toList fuses" $
      'toListArrayNativeFoldr `hasNoType` ''[])
  , expectFail
      $(inspectTest "fromListN fuses" $ 'fromListNEnum `hasNoType` ''[])
  , testGroup
      "Default allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnum `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnum `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnum `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnum `hasNoType` ''ST)
      ]
  , testGroup
      "Pinned allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnumP `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnumP `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnumP `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnumP `hasNoType` ''ST)
      ]
  , testGroup
      "Pinned foreign allocator optimised out"
      [ $(inspectTest "AllocatorM" $ 'fromListNEnumF `hasNoType` ''U.AllocatorM)
      , $(inspectTest "Allocator" $ 'fromListNEnumF `hasNoType` ''U.Allocator)
      , $(inspectTest "IO" $ 'fromListNEnumF `hasNoType` ''IO)
      , $(inspectTest "ST" $ 'fromListNEnumF `hasNoType` ''ST)
      ]
  , testGroup
      "Unpack"
      [ testGroup
          "Foreign"
          [ $(inspectTest "ForeignPtr" $
              'mkForeignArray `hasNoType` ''ForeignPtr)
          , $(inspectTest "CountOf" $ 'mkForeignArray `hasNoType` ''U.CountOf)
          , $(inspectTest "Int" $ 'mkForeignArray `hasNoType` ''Int)
          ]
      ]
  , testGroup
      "Free forgetfulness"
      [ $(inspectTest "Coercion" $ 'forgetNativeAllocator `hasNoType` ''Coercion)
      , $(inspectTest "Sing" $ 'forgetNativeAllocator `hasNoType` ''U.Sing)
      ]
  ]

nativeArrayLength :: U.Array alloc 'U.Native Word8 -> U.CountOf Word8
nativeArrayLength = U.arrayLength

foreignArrayLength :: U.Array alloc 'U.Foreign Word8 -> U.CountOf Word8
foreignArrayLength = U.arrayLength

toListArrayNative ::
     U.Allocator 'U.Native alloc => U.Array alloc 'U.Native Word16 -> [Word16]
toListArrayNative = toList

toListArrayForeign ::
     U.Allocator 'U.Foreign alloc => U.Array alloc 'U.Foreign Word16 -> [Word16]
toListArrayForeign = toList

toListArrayNativeFoldr ::
     (U.Allocator 'U.Native alloc, U.Primitive a)
  => (a -> r -> r)
  -> r
  -> U.Array alloc 'U.Native a
  -> r
toListArrayNativeFoldr f z = foldr f z . toList

fromListNEnum :: U.Array U.Default 'U.Native Word8
fromListNEnum = fromListN 10 [1 .. 10]

fromListNEnumP :: U.Array U.Pinned 'U.Native Word8
fromListNEnumP = fromListN 10 [1 .. 10]

fromListNEnumF :: U.Array U.Pinned 'U.Foreign Word8
fromListNEnumF = fromListN 10 [1 .. 10]

mkForeignArray ::
     Addr# -> ForeignPtrContents -> Int# -> U.Array allocator 'U.Foreign a
mkForeignArray ptr cts i =
  U.FArray (U.ForeignArray (ForeignPtr ptr cts) (U.CountOf (I# i)))

forgetNativeAllocator ::
     U.Array allocator 'U.Native a -> U.Array U.Unknown 'U.Native a
forgetNativeAllocator = U.forgetArrayAllocator
