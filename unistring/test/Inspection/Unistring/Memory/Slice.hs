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

module Inspection.Unistring.Memory.Slice
  ( tests
  ) where

import Data.Word (Word16)
import GHC.Exts (ByteArray#, Int(I#), Int#)
import Test.Tasty (TestTree, testGroup)
import Data.Typeable (Typeable)

import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Allocator as Allocator
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Singletons as Singletons

import Inspection.TH (allHaveNoneOfTypes, hasNoneOfTypes, inspectTests)

tests :: [TestTree]
tests =
  [ testGroup
      "Unpack"
      [ testGroup
          "Native"
          $(inspectTests $
            'mkNativeSlice `hasNoneOfTypes`
            [''Array.NativeArray, ''Array.Array, ''Count.CountOf, ''Int])
      ]
  , testGroup
      "Slice unchecked"
      $(inspectTests $
        ['sliceUncheckedNativeW16, 'sliceUncheckedForeignW16] `allHaveNoneOfTypes`
        [''Singletons.Known, ''Singletons.Sing, ''Primitive.Primitive])
  , testGroup
      "Size"
      $(inspectTests $
        ['nativeSize, 'foreignSize] `allHaveNoneOfTypes`
        [''Singletons.Known, ''Singletons.Sing])
  , testGroup
      "List fusion"
      $(inspectTests $
        ['toListNativeFoldr, 'toListForeignFoldr] `allHaveNoneOfTypes`
        [''[], ''Maybe, ''(,)])
  , testGroup
      "Equality"
      $(inspectTests $
        ['sliceEqNative, 'sliceEqForeign, 'sliceEqMixed] `allHaveNoneOfTypes`
        [''Singletons.Sing, ''Singletons.Known, ''Primitive.Primitive])
  , testGroup
      "To Array"
      $(inspectTests $
        ['sliceToArrayFromNative, 'sliceToArrayFromForeign] `allHaveNoneOfTypes`
        [ ''Primitive.Primitive
        , ''Singletons.Sing
        , ''Singletons.Known
        , ''Typeable
        , ''Allocator.Allocator
        , ''Allocator.MutableArray
        , ''Allocator.MonadWithPtr
        , ''IO
        ])
  ]

mkNativeSlice ::
     ByteArray# -> Int# -> Int# -> Slice.Slice 'Storage.Native allocator a
mkNativeSlice ba# offset# len# =
  Slice.NativeSlice
    (Array.NArray (Array.NativeArray ba#))
    (Count.CountOf (I# offset#))
    (Count.CountOf (I# len#))

sliceUncheckedNativeW16 ::
     Count.CountOf Word16
  -> Count.CountOf Word16
  -> Slice.Slice 'Storage.Native allocator Word16
  -> Slice.Slice 'Storage.Native allocator Word16
sliceUncheckedNativeW16 = Slice.sliceUnchecked

sliceUncheckedForeignW16 ::
     Count.CountOf Word16
  -> Count.CountOf Word16
  -> Slice.Slice 'Storage.Foreign allocator Word16
  -> Slice.Slice 'Storage.Foreign allocator Word16
sliceUncheckedForeignW16 = Slice.sliceUnchecked

nativeSize :: Slice.Slice 'Storage.Native allocator a -> Count.CountOf a
nativeSize = Slice.size

foreignSize :: Slice.Slice 'Storage.Foreign allocator a -> Count.CountOf a
foreignSize = Slice.size

toListNativeFoldr ::
     (Word16 -> r -> r)
  -> r
  -> Slice.Slice 'Storage.Native allocator Word16
  -> r
toListNativeFoldr f z = foldr f z . Slice.toList

toListForeignFoldr ::
     (Word16 -> r -> r)
  -> r
  -> Slice.Slice 'Storage.Foreign allocator Word16
  -> r
toListForeignFoldr f z = foldr f z . Slice.toList

sliceEqNative ::
     Slice.Slice 'Storage.Native allocator Word16
  -> Slice.Slice 'Storage.Native allocator' Word16
  -> Bool
sliceEqNative = Slice.equal

sliceEqForeign ::
     Slice.Slice 'Storage.Foreign allocator Word16
  -> Slice.Slice 'Storage.Foreign allocator' Word16
  -> Bool
sliceEqForeign = Slice.equal

sliceEqMixed ::
     Slice.Slice 'Storage.Native allocator Word16
  -> Slice.Slice 'Storage.Foreign allocator' Word16
  -> Bool
sliceEqMixed = Slice.equal

sliceToArrayFromNative ::
     Slice.Slice 'Storage.Native Allocator.Default Word16
  -> Array.Array 'Storage.Foreign Allocator.Pinned Word16
sliceToArrayFromNative = Slice.toArray

sliceToArrayFromForeign ::
     Slice.Slice 'Storage.Foreign Allocator.Pinned Word16
  -> Array.Array 'Storage.Native Allocator.Default Word16
sliceToArrayFromForeign = Slice.toArray
