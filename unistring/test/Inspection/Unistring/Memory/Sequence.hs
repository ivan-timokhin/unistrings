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

import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Ownership as Ownership
import qualified Data.Unistring.Memory.Sequence.Internal as Sequence
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Memory.Strictness as Strictness

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
