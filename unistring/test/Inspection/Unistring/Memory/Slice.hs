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
import Test.Inspection (hasNoType)
import Test.Tasty (TestTree, testGroup)

import qualified Data.Unistring.Memory.Array as Array
import qualified Data.Unistring.Memory.Array.Unsafe as Array
import qualified Data.Unistring.Memory.Count as Count
import qualified Data.Unistring.Memory.Primitive.Class.Unsafe as Primitive
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Storage as Storage
import qualified Data.Unistring.Singletons as Singletons

import Inspection.TH (inspectTest)

tests :: [TestTree]
tests =
  [ testGroup
      "Unpack"
      [ testGroup
          "Native"
          [ $(inspectTest "NativeArray" $
              'mkNativeSlice `hasNoType` ''Array.NativeArray)
          , $(inspectTest "Array" $ 'mkNativeSlice `hasNoType` ''Array.Array)
          , $(inspectTest "CountOf" $ 'mkNativeSlice `hasNoType` ''Count.CountOf)
          , $(inspectTest "Int" $ 'mkNativeSlice `hasNoType` ''Int)
          ]
      ]
  , testGroup
      "Slice unchecked"
      [ testGroup
          "Native"
          [ $(inspectTest "Known" $
              'sliceUncheckedNativeW16 `hasNoType` ''Singletons.Known)
          , $(inspectTest "Sing" $
              'sliceUncheckedNativeW16 `hasNoType` ''Singletons.Sing)
          , $(inspectTest "Primitive" $
              'sliceUncheckedNativeW16 `hasNoType` ''Primitive.Primitive)
          ]
      , testGroup
          "Foreign"
          [ $(inspectTest "Known" $
              'sliceUncheckedForeignW16 `hasNoType` ''Singletons.Known)
          , $(inspectTest "Sing" $
              'sliceUncheckedForeignW16 `hasNoType` ''Singletons.Sing)
          , $(inspectTest "Primitive" $
              'sliceUncheckedForeignW16 `hasNoType` ''Primitive.Primitive)
          ]
      ]
  , testGroup
      "Size"
      [ testGroup
          "Native"
          [ $(inspectTest "Known" $ 'nativeSize `hasNoType` ''Singletons.Known)
          , $(inspectTest "Sing" $ 'nativeSize `hasNoType` ''Singletons.Sing)
          ]
      , testGroup
          "Foreign"
          [ $(inspectTest "Known" $ 'foreignSize `hasNoType` ''Singletons.Known)
          , $(inspectTest "Sing" $ 'foreignSize `hasNoType` ''Singletons.Sing)
          ]
      ]
  , testGroup
      "List fusion"
      [ testGroup
          "Native"
          [ $(inspectTest "List" $ 'toListNativeFoldr `hasNoType` ''[])
          , $(inspectTest "Maybe" $ 'toListNativeFoldr `hasNoType` ''Maybe)
          , $(inspectTest "Tuple" $ 'toListNativeFoldr `hasNoType` ''(,))
          ]
      , testGroup
          "Foreign"
          [ $(inspectTest "List" $ 'toListForeignFoldr `hasNoType` ''[])
          , $(inspectTest "Maybe" $ 'toListForeignFoldr `hasNoType` ''Maybe)
          , $(inspectTest "Tuple" $ 'toListForeignFoldr `hasNoType` ''(,))
          ]
      ]
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
