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

module Inspection.Unistring.Memory.Sequence
  ( tests
  ) where

import GHC.Exts (Addr#, ByteArray#, Int(I#), Int#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents)
import Test.Tasty (TestTree, testGroup)
import Data.Word (Word32)
import Test.Tasty.ExpectedFailure (expectFail)

import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Sequence.Internal as Sequence
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import Data.Unistring.Singletons (Known, Sing)

import Inspection.TH (allHaveNoneOfTypes, inspectTests)

tests :: [TestTree]
tests =
  [ testGroup
      "Unpack"
      $(inspectTests $
        [ 'mkNativeFullLazy
        , 'mkForeignFullLazy
        , 'mkNativeSliceLazy
        , 'mkForeignSliceLazy
        ] `allHaveNoneOfTypes`
        [ ''ForeignPtr
        , ''Count.CountOf
        , ''Int
        , ''Array.NativeArray
        , ''Array.ForeignArray
        , ''Array.Array
        , ''Slice.Slice
        ])
  , testGroup
      "toList + fusion"
      $(inspectTests $
        [ 'toListFoldrNativeFullStrict
        , 'toListFoldrNativeSliceStrict
        , 'toListFoldrForeignFullStrict
        , 'toListFoldrForeignSliceStrict
        , 'toListFoldrNativeFullLazy
        , 'toListFoldrForeignFullLazy
        , 'toListFoldrNativeSliceLazy
        , 'toListFoldrForeignSliceLazy
        ] `allHaveNoneOfTypes`
        [''[], ''Known, ''Sing, ''Primitive.Primitive])
  ]

mkNativeFullLazy ::
     ByteArray#
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy a
mkNativeFullLazy ba# =
  Sequence.ConsNF (Array.NArray $ Array.NativeArray ba#) Sequence.NilNF

mkForeignFullLazy ::
     Addr#
  -> ForeignPtrContents
  -> Int#
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy a
mkForeignFullLazy ptr# cts len# =
  Sequence.ConsFF
    (Array.FArray $
     Array.ForeignArray (ForeignPtr ptr# cts) (Count.CountOf $ I# len#))
    Sequence.NilFF

mkNativeSliceLazy ::
     ByteArray#
  -> Int#
  -> Int#
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy a
mkNativeSliceLazy ba# off# len# =
  Sequence.ConsNS
    (Slice.NativeSlice
       (Array.NArray $ Array.NativeArray ba#)
       (Count.CountOf $ I# off#)
       (Count.CountOf $ I# len#))
    Sequence.NilNS

mkForeignSliceLazy ::
     Addr#
  -> ForeignPtrContents
  -> Int#
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy a
mkForeignSliceLazy ptr# cts len# =
  Sequence.ConsFS
    (Slice.ForeignSlice $
     Array.FArray $
     Array.ForeignArray (ForeignPtr ptr# cts) (Count.CountOf $ I# len#))
    Sequence.NilFS

toListFoldrNativeFullStrict ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Strict Word32
  -> r
toListFoldrNativeFullStrict f z = foldr f z . Sequence.toList

toListFoldrForeignFullStrict ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Strict Word32
  -> r
toListFoldrForeignFullStrict f z = foldr f z . Sequence.toList

toListFoldrNativeSliceStrict ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Strict Word32
  -> r
toListFoldrNativeSliceStrict f z = foldr f z . Sequence.toList

toListFoldrForeignSliceStrict ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Strict Word32
  -> r
toListFoldrForeignSliceStrict f z = foldr f z . Sequence.toList

toListFoldrNativeFullLazy ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Full 'Strictness.Lazy Word32
  -> r
toListFoldrNativeFullLazy f z = foldr f z . Sequence.toList

toListFoldrForeignFullLazy ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Full 'Strictness.Lazy Word32
  -> r
toListFoldrForeignFullLazy f z = foldr f z . Sequence.toList

toListFoldrNativeSliceLazy ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Native allocator 'Ownership.Slice 'Strictness.Lazy Word32
  -> r
toListFoldrNativeSliceLazy f z = foldr f z . Sequence.toList

toListFoldrForeignSliceLazy ::
     (Word32 -> r -> r)
  -> r
  -> Sequence.Sequence 'Storage.Foreign allocator 'Ownership.Slice 'Strictness.Lazy Word32
  -> r
toListFoldrForeignSliceLazy f z = foldr f z . Sequence.toList
